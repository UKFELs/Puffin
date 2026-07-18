#!/usr/bin/env python3
"""
Interactive Bokeh viewer for Puffin 1D field output (aperp_*.h5).

Run with:
    bokeh serve --show viewField1D_bokeh.py --args /path/to/data [basename] [lattice.latt]

Layout:
    Top row  — temporal intensity profile (W/m²) vs ct-z (µm)
               spectral intensity (a.u.) vs ω/ωr  (linear + log)
    Mid row  — beam current (A) vs ct-z (µm)  [with phase-space zoom indicator]
               electron phase space Δγ/⟨γ⟩ vs ct-z (µm)  [zoomed to one cycle]
    Lower    — power vs z (from integrated files)
    Bottom   — lattice diagram + slider
"""

import sys, os, glob, re
import numpy as np
import h5py

from bokeh.plotting import figure, curdoc
from bokeh.models import (ColumnDataSource, Slider, LinearColorMapper,
                           ColorBar, Div, Span, Range1d, RangeTool)
from bokeh.layouts import column, row

# ── physical constants ──────────────────────────────────────────────────────
C0   = 2.99792458e8
QE   = 1.60217653e-19
EPS0 = 8.854187817e-12
ME   = 9.1093826e-31


# ── helpers ──────────────────────────────────────────────────────────────────

def step_num(path):
    m = re.search(r'_(\d+)\.h5$', path)
    return int(m.group(1)) if m else 0


def intens_scale(attrs):
    return C0 * EPS0 * (attrs['gamma_r'] * ME * C0**2
                        / (QE * attrs['kappa'] * attrs['Lg']))**2


def load_field_1d(path):
    """Return xf, yf (shape nz2), and attrs dict.
    h5py reads the Fortran-written array as (2, nz2): axis 0 = component."""
    with h5py.File(path, 'r') as f:
        aperp = f['aperp'][()]   # (2, nz2)
        attrs = dict(f['runInfo'].attrs)
    xf = aperp[0, :]
    yf = aperp[1, :]
    return xf, yf, attrs


def _sliding_mean(arr, n):
    """O(n) sliding window mean of length n, edge-padded."""
    padded = np.pad(arr, (n // 2, n - 1 - n // 2), mode='edge')
    cs     = np.insert(np.cumsum(padded), 0, 0)
    return (cs[n:] - cs[:-n]) / n


def cycle_avg_intensity(xf, yf, attrs):
    """Peak amplitude squared, cycle-averaged over one resonant period."""
    nz2   = int(attrs['nZ2'])
    dz2   = float(attrs['sLengthOfElmZ2'])
    rho   = float(attrs['rho'])
    lr    = 4 * np.pi * rho
    nnl   = max(3, int(np.round(lr / dz2)) + 1)
    ax2   = _sliding_mean(xf**2, nnl)
    ay2   = _sliding_mean(yf**2, nnl)
    return 2.0 * (ax2 + ay2)


def temporal_profile(xf, yf, attrs):
    """Cycle-averaged intensity (W/m²) and ct-z axis (µm)."""
    iscale = intens_scale(attrs)
    nz2    = int(attrs['nZ2'])
    dz2    = float(attrs['sLengthOfElmZ2'])
    Lc     = float(attrs['Lc'])
    saxis  = np.arange(nz2) * dz2 * Lc * 1e6     # µm
    intens = cycle_avg_intensity(xf, yf, attrs) * iscale
    return saxis, intens


def spectral_profile(xf, yf, attrs):
    """Normalised spectral intensity and ω/ωr axis."""
    nz2  = int(attrs['nZ2'])
    dz2  = float(attrs['sLengthOfElmZ2'])
    rho  = float(attrs['rho'])
    lenz2 = (nz2 - 1) * dz2
    fs    = nz2 / lenz2
    npts  = int(np.ceil((nz2 + 1) / 2))

    ftx = np.fft.fft(xf)
    fty = np.fft.fft(yf)

    px = np.abs(ftx[:npts])**2
    py = np.abs(fty[:npts])**2
    for p in (px, py):
        if nz2 % 2 == 1:
            p[1:] *= 2
        else:
            p[1:-1] *= 2

    spec = px + py
    spec_norm = spec / np.max(spec) if np.max(spec) > 0 else spec

    omega_axis = np.arange(npts) * (fs / nz2) * (4 * np.pi * rho)
    return omega_axis, spec_norm


def load_electrons(path, attrs):
    """Return (z2_µm, d_gam_rel) from an electron HDF5 file.

    z2_µm   — macroparticle ct-z positions in µm
    d_gam_rel — fractional energy deviation (γ - ⟨γ⟩) / ⟨γ⟩
    """
    with h5py.File(path, 'r') as f:
        data = f['electrons'][()]   # Fortran: (nMPs, 7); cols = x,y,z2,px,py,gam,chi
    if data.ndim == 2 and data.shape[1] == 7:
        data = data.T               # → (7, nMPs) so data[coord, :] indexes by coord
    Lc      = float(attrs['Lc'])
    z2_um   = data[2, :] * Lc * 1e6
    gam     = data[5, :]
    gam_mean = float(np.mean(gam))
    d_gam   = (gam - gam_mean) / gam_mean if gam_mean != 0 else gam - gam_mean
    return z2_um, d_gam


def load_current(int_path, attrs):
    """Return (curr_x_µm, curr_A) arrays from an integrated HDF5 file.

    The current mesh spans the same physical z2 range as the field mesh.
    """
    with h5py.File(int_path, 'r') as f:
        current = f['beamCurrentSI'][()]
    nz2 = int(attrs['nZ2'])
    dz2 = float(attrs['sLengthOfElmZ2'])
    Lc  = float(attrs['Lc'])
    x   = np.linspace(0.0, (nz2 - 1) * dz2 * Lc * 1e6, len(current))
    return x, current


# ── collect aperp files ───────────────────────────────────────────────────────

args     = sys.argv[1:]
data_dir = args[0] if args else '.'
basename = args[1] if len(args) > 1 else None
latt_arg = args[2] if len(args) > 2 else None

all_h5 = glob.glob(os.path.join(data_dir, '*_aperp_*.h5'))
if not all_h5:
    raise RuntimeError(f'No aperp HDF5 files found in {data_dir}')

if basename is None:
    basename = re.sub(r'_aperp_\d+\.h5$', '',
                      os.path.basename(sorted(all_h5, key=step_num)[0]))

aperp_files = sorted(
    glob.glob(os.path.join(data_dir, f'{basename}_aperp_*.h5')),
    key=step_num)
n_files = len(aperp_files)
print(f'Found {n_files} aperp files  (basename="{basename}")', flush=True)


# ── pre-scan z positions ──────────────────────────────────────────────────────
print('Scanning z positions...', end=' ', flush=True)
z_vals = []
for p in aperp_files:
    with h5py.File(p, 'r') as f:
        z_vals.append(float(f['runInfo'].attrs['zTotal']))
z_vals = np.array(z_vals)
print('done', flush=True)


# ── power curve from integrated files ─────────────────────────────────────────
int_files = sorted(
    glob.glob(os.path.join(data_dir, f'{basename}_integrated_*.h5')),
    key=step_num)
pow_z, pow_W = np.array([]), np.array([])
int_z_vals   = np.array([])
if int_files:
    print(f'Loading power curve ({len(int_files)} files)...', end=' ', flush=True)
    pz, pw = [], []
    for p in int_files:
        with h5py.File(p, 'r') as f:
            ri = f['runInfo'].attrs
            pz.append(float(ri['zTotal']))
            pw.append(float(np.mean(f['powerSI'][()])))
    pow_z      = np.array(pz)
    pow_W      = np.where(np.array(pw) > 0, pw, np.nan)
    int_z_vals = pow_z.copy()
    print('done', flush=True)


def _closest_int_file(z):
    """Return the integrated file path whose z is closest to z, or None."""
    if len(int_z_vals) == 0:
        return None
    return int_files[int(np.argmin(np.abs(int_z_vals - z)))]


# ── collect electron files by step number ─────────────────────────────────────
elec_files_by_step = {
    step_num(p): p
    for p in glob.glob(os.path.join(data_dir, f'{basename}_electrons_*.h5'))
}
print(f'Found {len(elec_files_by_step)} electron file(s)', flush=True)


def _elec_file_for(aperp_idx):
    """Return electron file path matching aperp file index, or None."""
    return elec_files_by_step.get(step_num(aperp_files[aperp_idx]))


# ── lattice ───────────────────────────────────────────────────────────────────
def parse_lattice(latt_path, lw):
    elements, z = [], 0.0
    with open(latt_path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('!'): continue
            tag  = line[:2].upper()
            nums = list(map(float, re.findall(
                r'[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?', line[2:])))
            if tag == 'UN' and nums:
                L = int(nums[0]) * lw; elements.append(('UN', z, L)); z += L
            elif tag == 'DR' and nums:
                L = nums[0] * lw;      elements.append(('DR', z, L)); z += L
            elif tag == 'CH' and nums:
                L = nums[0] * lw
                elements.append(('CH', z, L)); z += L
            elif tag == 'QU':
                elements.append(('QU', z, 0.0))
    return elements, z

if latt_arg:
    latt_file = latt_arg
else:
    candidates = (glob.glob(os.path.join(data_dir, '*.latt')) +
                  glob.glob(os.path.join(
                      os.path.dirname(os.path.abspath(data_dir)), '*.latt')))
    latt_file = candidates[0] if candidates else None

with h5py.File(aperp_files[0], 'r') as f:
    lambda_w = float(f['runInfo'].attrs.get('lambda_w', 0.0275))

latt_elements, latt_z_total = [], 0.0
if latt_file and os.path.exists(latt_file):
    latt_elements, latt_z_total = parse_lattice(latt_file, lambda_w)
    print(f'Lattice: {len(latt_elements)} elements, total z = {latt_z_total:.3f} m',
          flush=True)


# ── initial data ──────────────────────────────────────────────────────────────
xf0, yf0, attrs0 = load_field_1d(aperp_files[0])
s0, intens0       = temporal_profile(xf0, yf0, attrs0)
omega0, spec0     = spectral_profile(xf0, yf0, attrs0)

# Initial electron phase space
_elec0 = _elec_file_for(0)
if _elec0:
    z2_ps0, dgam0 = load_electrons(_elec0, attrs0)
else:
    z2_ps0, dgam0 = np.array([0.0]), np.array([0.0])

# Initial current profile
_int0 = _closest_int_file(z_vals[0])
if _int0:
    curr_x0, curr_A0 = load_current(_int0, attrs0)
else:
    curr_x0, curr_A0 = np.array([0.0]), np.array([0.0])

# One-cycle length in µm  (resonant wavelength = 4π·ρ·Lc)
_rho = float(attrs0['rho'])
_Lc  = float(attrs0['Lc'])
cycle_len_um = 4.0 * np.pi * _rho * _Lc * 1e6

# Default phase-space x_range: one cycle centred on particle centroid
_z_center = float(np.median(z2_ps0)) if len(z2_ps0) > 0 else float(np.mean(s0))
ps_xrange = Range1d(
    start=_z_center - cycle_len_um / 2,
    end  =_z_center + cycle_len_um / 2,
)

# Initial phase-space y-range from particles inside the starting window
def _yrange_from_data(x_arr, y_arr, x0, x1):
    """Return (ylo, yhi) with 5 % padding for particles in [x0, x1]."""
    if len(x_arr) == 0:
        return -0.05, 0.05
    mask = (x_arr >= x0) & (x_arr <= x1)
    y_vis = y_arr[mask] if np.any(mask) else y_arr
    if len(y_vis) == 0:
        return -0.05, 0.05
    rng = float(np.max(y_vis)) - float(np.min(y_vis))
    pad = 0.05 * rng if rng > 0 else 1e-4
    return float(np.min(y_vis)) - pad, float(np.max(y_vis)) + pad

_py0, _py1 = _yrange_from_data(z2_ps0, dgam0, ps_xrange.start, ps_xrange.end)
ps_yrange = Range1d(start=_py0, end=_py1)


# ── figures ───────────────────────────────────────────────────────────────────
TOOLS = 'pan,wheel_zoom,box_zoom,reset,save'
PW    = 1000   # total plot width
TW    = PW // 3 - 10   # width of each top panel
HW    = PW // 2 - 10   # width of each half-width panel

src_intens   = ColumnDataSource(dict(x=s0.tolist(),     y=intens0.tolist()))
src_spec_lin = ColumnDataSource(dict(x=omega0.tolist(), y=spec0.tolist()))
spec0_log = np.where(spec0 > 0, spec0, 1e-30)
src_spec_log = ColumnDataSource(dict(x=omega0.tolist(), y=spec0_log.tolist()))

p_intens = figure(title='Temporal intensity  (cycle-avg)',
                  x_axis_label='ct − z  (µm)',
                  y_axis_label='Intensity  (W m⁻²)',
                  width=TW, height=320, tools=TOOLS)
p_intens.line('x', 'y', source=src_intens, color='royalblue', line_width=1.5)

_spec_ref = Span(location=1.0, dimension='height',
                 line_color='red', line_dash='dashed', line_width=1)
_spec_ref2 = Span(location=1.0, dimension='height',
                  line_color='red', line_dash='dashed', line_width=1)

p_spec = figure(title='Spectral intensity  (linear)',
                x_axis_label='ω / ωᵣ',
                y_axis_label='Intensity  (a.u.)',
                width=TW, height=320, tools=TOOLS)
p_spec.line('x', 'y', source=src_spec_lin, color='darkorange', line_width=1.5)
p_spec.add_layout(_spec_ref)

p_spec_log = figure(title='Spectral intensity  (log)',
                    x_axis_label='ω / ωᵣ',
                    y_axis_label='Intensity  (a.u.)',
                    x_range=p_spec.x_range,
                    width=TW, height=320, tools=TOOLS,
                    y_axis_type='log')
p_spec_log.line('x', 'y', source=src_spec_log, color='darkorange', line_width=1.5)
p_spec_log.add_layout(_spec_ref2)


# ── current profile panel ─────────────────────────────────────────────────────
src_curr = ColumnDataSource(dict(x=curr_x0.tolist(), y=curr_A0.tolist()))

p_current = figure(title='Beam current  (drag green window → moves phase-space view)',
                   x_axis_label='ct − z  (µm)',
                   y_axis_label='Current  (A)',
                   x_range=p_intens.x_range,   # shares field x-axis
                   width=HW, height=280, tools='wheel_zoom,box_zoom,reset,save')
p_current.line('x', 'y', source=src_curr, color='steelblue', line_width=1.5)

# RangeTool: draggable/resizable box that directly drives ps_xrange
range_tool = RangeTool(x_range=ps_xrange)
range_tool.overlay.fill_color  = 'green'
range_tool.overlay.fill_alpha  = 0.12
range_tool.overlay.line_color  = 'green'
range_tool.overlay.line_width  = 1.2
p_current.add_tools(range_tool)


# ── electron phase-space panel ────────────────────────────────────────────────
src_phase = ColumnDataSource(dict(x=z2_ps0.tolist(), y=dgam0.tolist()))

p_phase = figure(title='Electron phase space  (one-cycle zoom)',
                 x_axis_label='ct − z  (µm)',
                 y_axis_label='Δγ / ⟨γ⟩',
                 x_range=ps_xrange,
                 y_range=ps_yrange,
                 width=HW, height=280, tools=TOOLS)
p_phase.scatter('x', 'y', source=src_phase,
                color='tomato', size=1.5, alpha=0.5)


def _rescale_ps_y():
    """Rescale the phase-space y-axis to particles inside the current x window."""
    x = np.array(src_phase.data['x'])
    y = np.array(src_phase.data['y'])
    lo, hi = _yrange_from_data(x, y, ps_xrange.start, ps_xrange.end)
    ps_yrange.start = lo
    ps_yrange.end   = hi


def _ps_range_cb(attr, old, new):
    _rescale_ps_y()

ps_xrange.on_change('start', _ps_range_cb)
ps_xrange.on_change('end',   _ps_range_cb)


# ── power vs z plot ───────────────────────────────────────────────────────────
ylo = float(np.nanmin(pow_W)) if len(pow_W) else 1e-3
yhi = float(np.nanmax(pow_W)) if len(pow_W) else 1e10

p_power = figure(title='Power vs z',
                 x_axis_label='z (m)', y_axis_label='Power (W)',
                 width=PW, height=220, tools=TOOLS, y_axis_type='log')
if len(pow_z):
    src_pow = ColumnDataSource(dict(x=pow_z.tolist(), y=pow_W.tolist()))
    p_power.line('x', 'y', source=src_pow, color='royalblue', line_width=1.5)

src_vline = ColumnDataSource(dict(x0=[z_vals[0]], y0=[ylo],
                                  x1=[z_vals[0]], y1=[yhi]))
p_power.segment('x0', 'y0', 'x1', 'y1', source=src_vline,
                color='red', line_dash='dashed', line_width=1.5)

# lattice diagram
p_latt = figure(title='Lattice',
                x_axis_label='z (m)',
                x_range=p_power.x_range,
                width=PW, height=90, tools='pan,reset',
                y_range=(-0.05, 1.05))
p_latt.yaxis.visible = False
p_latt.ygrid.visible = False
p_latt.xgrid.visible = False

if latt_elements:
    un_x, un_y, un_w, un_h     = [], [], [], []
    dr_x, dr_y, dr_w, dr_h     = [], [], [], []
    ch_x, ch_y, ch_w, ch_h     = [], [], [], []
    ch_x0, ch_y0, ch_x1, ch_y1 = [], [], [], []   # zero-length chicanes as lines
    qu_x0, qu_y0, qu_x1, qu_y1 = [], [], [], []

    for typ, zs, L in latt_elements:
        if typ == 'UN':
            un_x.append(zs + L/2); un_y.append(0.5)
            un_w.append(L);        un_h.append(0.9)
        elif typ == 'DR' and L > 0:
            dr_x.append(zs + L/2); dr_y.append(0.5)
            dr_w.append(L);        dr_h.append(0.5)
        elif typ == 'CH':
            if L > 0:
                ch_x.append(zs + L/2); ch_y.append(0.5)
                ch_w.append(L);        ch_h.append(0.7)
            else:
                ch_x0.append(zs); ch_y0.append(0.05)
                ch_x1.append(zs); ch_y1.append(0.95)
        elif typ == 'QU':
            qu_x0.append(zs); qu_y0.append(0.05)
            qu_x1.append(zs); qu_y1.append(0.95)

    if dr_x:
        p_latt.rect(dr_x, dr_y, dr_w, dr_h, color='#CCCCCC',
                    line_color=None, alpha=0.6, legend_label='Drift')
    if un_x:
        p_latt.rect(un_x, un_y, un_w, un_h, color='#2176AE',
                    line_color='white', line_width=0.5, legend_label='Undulator')
    if ch_x:
        p_latt.rect(ch_x, ch_y, ch_w, ch_h, color='#E87D2B',
                    line_color=None, legend_label='Chicane')
    if ch_x0:
        p_latt.segment(ch_x0, ch_y0, ch_x1, ch_y1, color='#E87D2B',
                       line_width=2.5, legend_label='Chicane')
    if qu_x0:
        p_latt.segment(qu_x0, qu_y0, qu_x1, qu_y1, color='#D62839',
                       line_width=2, legend_label='Quad')

    p_latt.legend.orientation = 'horizontal'
    p_latt.legend.label_text_font_size = '9pt'
    p_latt.legend.spacing = 10
    p_latt.legend.padding = 4
    p_latt.add_layout(p_latt.legend[0], 'below')

src_lmark = ColumnDataSource(dict(x0=[z_vals[0]], y0=[0.0],
                                  x1=[z_vals[0]], y1=[1.0]))
p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_lmark,
               color='red', line_dash='dashed', line_width=1.5)


# ── status + slider ───────────────────────────────────────────────────────────
def _status_html(idx):
    return (f'<span style="font-size:13px"><b>Step {idx+1}/{n_files}</b>'
            f' &nbsp;|&nbsp; file step {step_num(aperp_files[idx])}'
            f' &nbsp;|&nbsp; z = {z_vals[idx]:.4f} m</span>')

status = Div(text=_status_html(0), width=PW)
slider = Slider(start=0, end=n_files - 1, value=0, step=1,
                title=f'z = {z_vals[0]:.3f} m', width=PW)


def update(attr, old, new):
    idx = int(slider.value)
    xf, yf, attrs = load_field_1d(aperp_files[idx])

    s, intens = temporal_profile(xf, yf, attrs)
    omega, spec = spectral_profile(xf, yf, attrs)

    spec_log = np.where(spec > 0, spec, 1e-30)
    src_intens.data   = dict(x=s.tolist(),         y=intens.tolist())
    src_spec_lin.data = dict(x=omega.tolist(),      y=spec.tolist())
    src_spec_log.data = dict(x=omega.tolist(),      y=spec_log.tolist())

    # Electron phase space
    ep = _elec_file_for(idx)
    if ep:
        z2_um, dgam = load_electrons(ep, attrs)
        src_phase.data = dict(x=z2_um.tolist(), y=dgam.tolist())
    else:
        src_phase.data = dict(x=[], y=[])
    _rescale_ps_y()

    # Current profile
    ip = _closest_int_file(z_vals[idx])
    if ip:
        cx, cy = load_current(ip, attrs)
        src_curr.data = dict(x=cx.tolist(), y=cy.tolist())
    else:
        src_curr.data = dict(x=[], y=[])

    z = z_vals[idx]
    src_vline.data = dict(x0=[z], y0=[ylo], x1=[z], y1=[yhi])
    src_lmark.data = dict(x0=[z], y0=[0.0], x1=[z], y1=[1.0])

    slider.title = f'z = {z:.3f} m'
    status.text  = _status_html(idx)


slider.on_change('value', update)

# ── layout ────────────────────────────────────────────────────────────────────
curdoc().add_root(column(
    status,
    row(p_intens, p_spec, p_spec_log),
    row(p_current, p_phase),
    p_power,
    p_latt,
    slider,
))
curdoc().title = f'Puffin 1D Field Viewer — {basename}'
