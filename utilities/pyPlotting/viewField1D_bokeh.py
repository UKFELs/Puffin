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
                           ColorBar, Div, Span, Range1d, RangeTool,
                           HoverTool, CrosshairTool, LinearAxis,
                           RadioButtonGroup)
from bokeh.layouts import column, row

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import puffin_viz_theme as pvt
import puffin_viz_player as pvp
import puffin_viz_data as pvd

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


def temporal_profile(xf, yf, attrs, u=None):
    """Cycle-averaged intensity and the z2 axis, in u's units."""
    u = u or UNITS
    nz2 = int(attrs['nZ2'])
    saxis = u.z2_axis(nz2, attrs)
    intens = (cycle_avg_intensity(xf, yf, attrs)
              * u.intensity_factor(intens_scale(attrs)))
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
    z2_um   = data[2, :] * (UNITS.z2_mul)
    gam     = data[5, :]
    gam_mean = float(np.mean(gam))
    d_gam   = (gam - gam_mean) / gam_mean if gam_mean != 0 else gam - gam_mean
    return z2_um, d_gam


def load_current(int_path, attrs):
    """Return (curr_x_µm, curr_A) arrays from an integrated HDF5 file.

    The current mesh is coarser than the field mesh (101 vs 2001 nodes here)
    but covers the same span. Rather than assume that, take the bounds the
    file declares on intCurrMeshSI — this panel shares its x-axis with the
    field panels, so if the two meshes ever diverge, an assumed span would
    silently misregister the beam against the radiation. Fall back to the
    field span for older files that lack the mesh group.
    """
    with h5py.File(int_path, 'r') as f:
        current = f[UNITS.curr_key][()]
        mesh = f.get('intCurrMeshSI')
        lo = hi = None
        if mesh is not None and not UNITS.scaled:
            lo = mesh.attrs.get('vsLowerBounds')
            hi = mesh.attrs.get('vsUpperBounds')

    if lo is None or hi is None:
        n = len(current)
        # spread the (coarser) current mesh across the full z2 window
        dz2 = float(attrs['sLengthOfElmZ2']) * (int(attrs['nZ2']) - 1) / max(n - 1, 1)
        return np.arange(n) * dz2 * UNITS.z2_mul, current

    x = np.linspace(float(lo) * 1e6, float(hi) * 1e6, len(current))
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
pow_z        = np.array([])
int_z_vals   = np.array([])
pow_attrs    = {}
# Both unit systems up front so toggling never re-reads the integrated set.
POW_W = {pvd.SCALED: np.array([]), pvd.SI: np.array([])}
POW_J = {pvd.SCALED: np.array([]), pvd.SI: np.array([])}
if int_files:
    print(f'Loading power curve ({len(int_files)} files)...', end=' ', flush=True)
    pz = []
    acc = {pvd.SCALED: ([], []), pvd.SI: ([], [])}
    for p in int_files:
        with h5py.File(p, 'r') as f:
            pow_attrs = dict(f['runInfo'].attrs)
            pz.append(float(pow_attrs['zTotal']))
            for mode in (pvd.SCALED, pvd.SI):
                u = pvd.Units(pow_attrs, mode)
                arr = f[u.power_key][()]
                # mean over z2 on a periodic mesh, peak on a temporal one —
                # see puffin_viz_data.reduce_power
                acc[mode][0].append(pvd.reduce_power(arr, pow_attrs)[0])
                acc[mode][1].append(u.energy(arr, pow_attrs))
    pow_z = np.array(pz)
    for mode in (pvd.SCALED, pvd.SI):
        w, j = acc[mode]
        POW_W[mode] = np.where(np.array(w) > 0, w, np.nan)
        POW_J[mode] = np.where(np.array(j) > 0, j, np.nan)
    int_z_vals = pow_z.copy()
    print(f'done  ({pvd.reduce_power(np.array([1.0]), pow_attrs)[1]})', flush=True)


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

def find_lattice(latt_arg, data_dir):
    """Resolve the lattice file, falling back to auto-detection.

    An explicit path that does not exist is a typo, not a request for no
    lattice — warn instead of silently drawing an empty lattice panel, which
    looks like the run simply had no elements.
    """
    if latt_arg:
        if os.path.exists(latt_arg):
            return latt_arg
        print(f'WARNING: lattice file not found: {latt_arg}'
              '  — falling back to auto-detection', flush=True)
    candidates = (glob.glob(os.path.join(data_dir, '*.latt')) +
                  glob.glob(os.path.join(
                      os.path.dirname(os.path.abspath(data_dir)), '*.latt')))
    if candidates:
        return candidates[0]
    print('WARNING: no .latt file found — lattice panel will be empty',
          flush=True)
    return None


latt_file = find_lattice(latt_arg, data_dir)

with h5py.File(aperp_files[0], 'r') as f:
    lambda_w = float(f['runInfo'].attrs.get('lambda_w', 0.0275))

latt_elements, latt_z_total = [], 0.0
if latt_file and os.path.exists(latt_file):
    latt_elements, latt_z_total = parse_lattice(latt_file, lambda_w)
    print(f'Lattice: {len(latt_elements)} elements, total z = {latt_z_total:.3f} m',
          flush=True)


# ── initial data ──────────────────────────────────────────────────────────────
xf0, yf0, attrs0 = load_field_1d(aperp_files[0])
UNITS = pvd.Units(attrs0, pvd.SI)     # default to SI; toggle switches it
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
cycle_len_um = 4.0 * np.pi * _rho * UNITS.z2_mul
cycle_len_disp = [cycle_len_um]   # mutable: apply_units rescales it

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
HW    = PW // 2 - 10   # width of each half-width panel
SLIDER_INSET = 90      # left margin keeping the slider off the window edge

T = pvt.tokens()
curdoc().theme = pvt.bokeh_theme(T)
pvt.apply_page_style(curdoc(), T)


def _hover(fig, renderer, xlab, xfmt, ylab, yfmt):
    """Crosshair + vline tooltip — the default hover layer for a line panel."""
    fig.add_tools(
        HoverTool(renderers=[renderer], mode='vline', attachment='above',
                  tooltips=[(xlab, f'@x{xfmt}'), (ylab, f'@y{yfmt}')]),
        CrosshairTool(line_color=T['muted'], line_alpha=0.6, line_width=1))

src_intens   = ColumnDataSource(dict(x=s0.tolist(),     y=intens0.tolist()))
src_spec_lin = ColumnDataSource(dict(x=omega0.tolist(), y=spec0.tolist()))
spec0_log = np.where(spec0 > 0, spec0, 1e-30)
src_spec_log = ColumnDataSource(dict(x=omega0.tolist(), y=spec0_log.tolist()))

# In 1D, power and intensity differ only by the fixed transverse area, so
# the cycle-averaged curve is one series in two unit systems (measured: the
# ratio is exactly pi here, constant to 0.000000% along the pulse, corr
# 1.00000000). The right axis is therefore a unit relabel of the same line,
# not a second series on its own scale — the usual objection to twin axes
# does not apply, because no scaling choice can change their relative shape.
def power_per_intensity(intens, power):
    """Constant relating the plotted intensity curve to power, or None.

    Derived from the data rather than from the scaling algebra so it cannot
    drift out of step with whatever convention cycle_avg_intensity uses.
    """
    i = np.asarray(intens, float)
    p = np.asarray(power, float)
    if i.size == 0 or p.size != i.size:
        return None
    m = i > i.max() * 0.01
    if not np.any(m) or p[m].max() <= 0:
        return None
    return float(np.median(p[m] / i[m]))


def _cycle_avg_power(int_path, attrs):
    """powerSI smoothed with the same window as cycle_avg_intensity."""
    with h5py.File(int_path, 'r') as f:
        pr = f[UNITS.power_key][()]
    dz2 = float(attrs['sLengthOfElmZ2'])
    rho = float(attrs['rho'])
    nnl = max(3, int(np.round(4 * np.pi * rho / dz2)) + 1)
    return _sliding_mean(pr, nnl)


def _derive_i_per_p():
    """Intensity->power factor for the current units, or None.

    Scans for the first step with a non-zero field: a run seeded from noise
    starts at exactly zero, where the ratio is undefined. The factor differs
    between unit systems, so this is re-run whenever the units change.
    """
    for k in range(n_files):
        ep = _closest_int_file(z_vals[k])
        if not ep:
            continue
        xf, yf, a = load_field_1d(aperp_files[k])
        _, i = temporal_profile(xf, yf, a)
        c = _cycle_avg_power(ep, a)
        if len(c) == len(i):
            f = power_per_intensity(i, c)
            if f:
                return f
    return None


i_per_p = _derive_i_per_p()

# Radiation panels all wear the "field" hue — see puffin_viz_theme.
# An explicit Range1d for the left axis, not the default DataRange1d: the
# right axis is a Range1d that honours the values pushed to it, while a
# DataRange1d re-derives its own limits client-side. Mixing the two lets the
# axes disagree, which would make the twin-axis reading wrong.
p_intens = figure(title='Temporal profile  (cycle-avg)',
                  x_axis_label=UNITS.z2_label,
                  y_axis_label=UNITS.intens_label,
                  y_range=Range1d(start=0.0, end=1.0),
                  width=PW, height=300, tools=TOOLS)
_r = p_intens.line('x', 'y', source=src_intens, color=T['field'], line_width=2)
_hover(p_intens, _r, 'ct−z', '{0.0} µm', 'I', '{0.00e+0} W/m²')

if i_per_p:
    # Right axis: the same curve read as power. Both ranges are set
    # explicitly and together (see _sync_intens_axes) so they can never
    # drift apart under autoscaling.
    p_intens.extra_y_ranges = {'power': Range1d(start=0.0, end=1.0)}
    p_intens.add_layout(
        LinearAxis(y_range_name='power',
                   axis_label=UNITS.power_label), 'right')

    def _sync_intens_axes(intens):
        hi = float(np.max(intens)) if len(intens) else 0.0
        hi = hi * 1.05 if hi > 0 else 1.0
        p_intens.y_range.start = 0.0
        p_intens.y_range.end = hi
        p_intens.extra_y_ranges['power'].start = 0.0
        p_intens.extra_y_ranges['power'].end = hi * i_per_p

    _sync_intens_axes(intens0)
else:
    def _sync_intens_axes(intens):
        pass

# ω/ωr = 1 is the resonance: a reference line, so it stays in muted ink
# rather than taking a series hue.
_spec_ref = Span(location=1.0, dimension='height', line_color=T['muted'],
                 line_dash='dashed', line_width=1)
_spec_ref2 = Span(location=1.0, dimension='height', line_color=T['muted'],
                  line_dash='dashed', line_width=1)

p_spec = figure(title='Spectral intensity  (linear)',
                x_axis_label='ω / ωᵣ',
                y_axis_label='Intensity  (a.u.)',
                width=HW, height=300, tools=TOOLS)
_r = p_spec.line('x', 'y', source=src_spec_lin, color=T['field'], line_width=2)
_hover(p_spec, _r, 'ω/ωᵣ', '{0.000}', 'I', '{0.000}')
p_spec.add_layout(_spec_ref)

p_spec_log = figure(title='Spectral intensity  (log)',
                    x_axis_label='ω / ωᵣ',
                    y_axis_label='Intensity  (a.u.)',
                    x_range=p_spec.x_range,
                    width=HW, height=300, tools=TOOLS,
                    y_axis_type='log')
_r = p_spec_log.line('x', 'y', source=src_spec_log, color=T['field'], line_width=2)
_hover(p_spec_log, _r, 'ω/ωᵣ', '{0.000}', 'I', '{0.00e+0}')
p_spec_log.add_layout(_spec_ref2)


# ── current profile panel ─────────────────────────────────────────────────────
src_curr = ColumnDataSource(dict(x=curr_x0.tolist(), y=curr_A0.tolist()))

# Electron-beam panels wear the "beam" hue.
p_current = figure(title='Beam current  (drag the window → moves phase-space view)',
                   x_axis_label=UNITS.z2_label,
                   y_axis_label=UNITS.curr_label,
                   x_range=p_intens.x_range,   # shares field x-axis
                   width=HW, height=280, tools='wheel_zoom,box_zoom,reset,save')
_r = p_current.line('x', 'y', source=src_curr, color=T['beam'], line_width=2)
_hover(p_current, _r, 'ct−z', '{0.0} µm', 'I', '{0.0} A')

# RangeTool: draggable/resizable box that directly drives ps_xrange. It is a
# control rather than data, so it takes the "select" hue — distinct from the
# beam trace underneath it and from the field hue elsewhere.
range_tool = RangeTool(x_range=ps_xrange)
range_tool.overlay.fill_color  = T['select']
range_tool.overlay.fill_alpha  = 0.12
range_tool.overlay.line_color  = T['select']
range_tool.overlay.line_width  = 1.5
p_current.add_tools(range_tool)


# ── electron phase-space panel ────────────────────────────────────────────────
src_phase = ColumnDataSource(dict(x=z2_ps0.tolist(), y=dgam0.tolist()))

p_phase = figure(title='Electron phase space  (one-cycle zoom)',
                 x_axis_label=UNITS.z2_label,
                 y_axis_label='Δγ / ⟨γ⟩',
                 x_range=ps_xrange,
                 y_range=ps_yrange,
                 width=HW, height=280, tools=TOOLS)
# No hover here: this is 10^5-plus macroparticles, and a per-mark hit test at
# that count stalls the pan/zoom the panel is actually for.
p_phase.scatter('x', 'y', source=src_phase,
                color=T['beam'], size=1.5, alpha=0.45)


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
# Series for the active unit system; apply_units() swaps these on toggle.
pow_W = POW_W[UNITS.mode]
pow_J = POW_J[UNITS.mode]

# Every dump can be zero power (a run seeded from noise that has not gained
# yet), and those are masked to NaN above — so check for a finite value, not
# just a non-empty array, or nanmin warns and returns NaN.
if len(pow_W) and np.any(np.isfinite(pow_W)):
    ylo = float(np.nanmin(pow_W)); yhi = float(np.nanmax(pow_W))
else:
    ylo, yhi = 1e-3, 1e10

# Log only when the curve actually spans decades. Start-to-saturation gain
# does and needs it; a short or already-saturated run spans well under a
# decade, and a log axis then crowds every tick into the top of the frame
# where the labels overlap and cannot be read.
_pow_decades = (yhi / ylo) if ylo > 0 else float('inf')
_pow_axis = 'log' if _pow_decades >= 100 else 'linear'

# Span the simulated z, not the lattice. p_latt shares this range, so a run
# that covers only the head of a long lattice (a truncated test case) would
# otherwise squash its whole power curve into a sliver at x=0 to make room
# for undulators that were never simulated.
_zs = np.concatenate([a for a in (z_vals, pow_z) if len(a)])
_zpad = max((float(_zs.max()) - float(_zs.min())) * 0.02, 1e-6)
_zrange = Range1d(start=float(_zs.min()) - _zpad, end=float(_zs.max()) + _zpad)

p_power = figure(title=UNITS.power_title(pow_attrs),
                 x_axis_label=UNITS.z_label,
                 y_axis_label=UNITS.power_label,
                 x_range=_zrange,
                 width=HW, height=230, tools=TOOLS, y_axis_type=_pow_axis)
if len(pow_z):
    src_pow = ColumnDataSource(dict(x=(pow_z*UNITS.z_mul).tolist(),
                                    y=pow_W.tolist()))
    _r = p_power.line('x', 'y', source=src_pow, color=T['field'], line_width=2)
    _hover(p_power, _r, 'z', '{0.000} m',
           pvd.reduce_power(np.array([1.0]), pow_attrs)[1], '{0.00e+0} W')

# The current-z cue is chrome, not a series: primary ink, dashed.
src_vline = ColumnDataSource(dict(x0=[z_vals[0]*UNITS.z_mul], y0=[ylo],
                                  x1=[z_vals[0]*UNITS.z_mul], y1=[yhi]))
p_power.segment('x0', 'y0', 'x1', 'y1', source=src_vline,
                color=T['marker'], line_dash='dashed', line_width=2)


# ── energy vs z ───────────────────────────────────────────────────────────────
# Peak power and energy answer different questions (how intense the spike is
# vs how much light there is in total), so they get their own panels rather
# than a second y-axis on one plot.
if len(pow_J) and np.any(np.isfinite(pow_J)):
    _elo, _ehi = float(np.nanmin(pow_J)), float(np.nanmax(pow_J))
else:
    _elo, _ehi = 1e-12, 1e-3
_e_axis = 'log' if (_elo > 0 and _ehi / _elo >= 100) else 'linear'

p_energy = figure(title=UNITS.energy_title(pow_attrs),
                  x_axis_label=UNITS.z_label,
                  y_axis_label=UNITS.energy_label,
                  x_range=_zrange,
                  width=HW, height=230, tools=TOOLS, y_axis_type=_e_axis)
if len(pow_z):
    src_en = ColumnDataSource(dict(x=(pow_z*UNITS.z_mul).tolist(),
                                   y=pow_J.tolist()))
    _r = p_energy.line('x', 'y', source=src_en, color=T['field'], line_width=2)
    _hover(p_energy, _r, 'z', '{0.000} m', 'E', '{0.000e+0} J')

src_evline = ColumnDataSource(dict(x0=[z_vals[0]*UNITS.z_mul], y0=[_elo],
                                   x1=[z_vals[0]*UNITS.z_mul], y1=[_ehi]))
p_energy.segment('x0', 'y0', 'x1', 'y1', source=src_evline,
                 color=T['marker'], line_dash='dashed', line_width=2)




# lattice diagram
p_latt = figure(title='Lattice',
                x_axis_label=UNITS.z_label,
                x_range=p_power.x_range,
                width=PW, height=90, tools='pan,reset',
                y_range=(-0.05, 1.05))
p_latt.yaxis.visible = False
p_latt.ygrid.visible = False
p_latt.xgrid.visible = False

if latt_elements:
    # CDS-backed so element positions can be rescaled when the unit system
    # changes; plain lists would be baked in at construction.
    src_dr = ColumnDataSource(dict(x=[], y=[], w=[], h=[]))
    src_un = ColumnDataSource(dict(x=[], y=[], w=[], h=[]))
    src_ch = ColumnDataSource(dict(x=[], y=[], w=[], h=[]))
    src_ch0 = ColumnDataSource(dict(x0=[], y0=[], x1=[], y1=[]))
    src_qu = ColumnDataSource(dict(x0=[], y0=[], x1=[], y1=[]))

    def _relayout_lattice():
        """Rebuild the lattice glyph positions for the current units."""
        zmul = UNITS.z_mul
        span = (latt_z_total * zmul) if latt_z_total > 0 else 1.0
        # A 2px gap in the surface colour separates touching elements;
        # convert 2px of the plot's own width into the displayed z units.
        gap = 2.0 / PW * span
        d = {k: dict(x=[], y=[], w=[], h=[]) for k in ('DR', 'UN', 'CH')}
        c0 = dict(x0=[], y0=[], x1=[], y1=[])
        q = dict(x0=[], y0=[], x1=[], y1=[])
        for typ, zs, L in latt_elements:
            zs, L = zs * zmul, L * zmul
            Lw = max(L - gap, L * 0.5)
            if typ == 'QU':
                q['x0'].append(zs); q['y0'].append(0.05)
                q['x1'].append(zs); q['y1'].append(0.95)
            elif typ == 'CH' and L <= 0:
                c0['x0'].append(zs); c0['y0'].append(0.05)
                c0['x1'].append(zs); c0['y1'].append(0.95)
            elif typ in d and (L > 0 or typ == 'UN'):
                h = {'UN': 0.9, 'DR': 0.5, 'CH': 0.7}[typ]
                d[typ]['x'].append(zs + L/2); d[typ]['y'].append(0.5)
                d[typ]['w'].append(Lw);       d[typ]['h'].append(h)
        src_dr.data, src_un.data, src_ch.data = d['DR'], d['UN'], d['CH']
        src_ch0.data, src_qu.data = c0, q

    _relayout_lattice()

    # Drift is the absence of an element, so it stays chrome grey; the three
    # real element types take the validated hues.
    if src_dr.data['x']:
        p_latt.rect('x', 'y', 'w', 'h', source=src_dr, color=T['drift'],
                    line_color=None, alpha=0.45, legend_label='Drift')
    if src_un.data['x']:
        p_latt.rect('x', 'y', 'w', 'h', source=src_un, color=T['field'],
                    line_color=None, legend_label='Undulator')
    if src_ch.data['x']:
        p_latt.rect('x', 'y', 'w', 'h', source=src_ch, color=T['beam'],
                    line_color=None, legend_label='Chicane')
    if src_ch0.data['x0']:
        p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_ch0,
                       color=T['beam'], line_width=2.5, legend_label='Chicane')
    if src_qu.data['x0']:
        p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_qu,
                       color=T['select'], line_width=2, legend_label='Quad')

    pvt.style_legend(p_latt, T)
    p_latt.add_layout(p_latt.legend[0], 'below')

src_lmark = ColumnDataSource(dict(x0=[z_vals[0]*UNITS.z_mul], y0=[0.0],
                                  x1=[z_vals[0]*UNITS.z_mul], y1=[1.0]))
p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_lmark,
               color=T['marker'], line_dash='dashed', line_width=2)


# ── status + slider ───────────────────────────────────────────────────────────
def _status_html(idx):
    return pvt.header_html(
        T, f'{basename} · 1D field', z_vals[idx] * UNITS.z_mul, idx + 1, n_files,
        unit=('' if UNITS.scaled else 'm'),
        extra=f' <span style="color:{T["muted"]};">· file step '
              f'{step_num(aperp_files[idx])}</span>')

status = Div(text=_status_html(0), width=PW,
             margin=(14, 10, 4, SLIDER_INSET))
# Inset from the left so the handle at step 0 is clear of the window edge —
# dragging there otherwise grabs the browser's resize border.
# Bokeh rejects start == end, which is what a single-dump run would give.
slider = Slider(start=0, end=max(n_files - 1, 1), value=0, step=1,
                disabled=n_files < 2,
                # No title: the header above already shows z as its hero
                # figure and the frame counter beside it. Bokeh renders
                # "<title>: <value>", so a title here gives either a
                # duplicated z or, with show_value off, a dangling colon.
                title=None, show_value=False,
                width=PW - SLIDER_INSET - 40,
                margin=(5, 40, 15, SLIDER_INSET))


def apply_units():
    """Re-label and re-scale every panel for the current UNITS."""
    global pow_W, pow_J, ylo, yhi, _elo, _ehi, i_per_p

    pow_W = POW_W[UNITS.mode]
    pow_J = POW_J[UNITS.mode]
    zs = pow_z * UNITS.z_mul
    if len(pow_z):
        src_pow.data = dict(x=zs.tolist(), y=pow_W.tolist())
        src_en.data  = dict(x=zs.tolist(), y=pow_J.tolist())
    _z = np.concatenate([a for a in (z_vals * UNITS.z_mul, zs) if len(a)])
    pad = max((float(_z.max()) - float(_z.min())) * 0.02, 1e-9)
    _zrange.start, _zrange.end = float(_z.min()) - pad, float(_z.max()) + pad

    if len(pow_W) and np.any(np.isfinite(pow_W)):
        ylo, yhi = float(np.nanmin(pow_W)), float(np.nanmax(pow_W))
    if len(pow_J) and np.any(np.isfinite(pow_J)):
        _elo, _ehi = float(np.nanmin(pow_J)), float(np.nanmax(pow_J))

    p_power.title.text = UNITS.power_title(pow_attrs)
    p_power.xaxis.axis_label = UNITS.z_label
    p_power.yaxis.axis_label = UNITS.power_label
    p_energy.title.text = UNITS.energy_title(pow_attrs)
    p_energy.xaxis.axis_label = UNITS.z_label
    p_energy.yaxis.axis_label = UNITS.energy_label
    p_intens.xaxis.axis_label = UNITS.z2_label
    p_intens.yaxis.axis_label = UNITS.intens_label
    p_current.xaxis.axis_label = UNITS.z2_label
    p_current.yaxis.axis_label = UNITS.curr_label
    p_phase.xaxis.axis_label = UNITS.z2_label
    if latt_elements:
        p_latt.xaxis.axis_label = UNITS.z_label
        _relayout_lattice()

    # The intensity->power factor is unit-dependent, so re-derive it.
    i_per_p = _derive_i_per_p()
    if i_per_p and p_intens.right:
        p_intens.right[0].axis_label = UNITS.power_label

    # phase-space x window is in z2 units
    c = 4.0 * np.pi * float(attrs0['rho']) * UNITS.z2_mul
    mid = 0.5 * (ps_xrange.start + ps_xrange.end)
    ratio = c / max(cycle_len_disp[0], 1e-30)
    cycle_len_disp[0] = c
    ps_xrange.start, ps_xrange.end = mid*ratio - c/2, mid*ratio + c/2

    update(None, None, None)


def _on_units(attr, old, new):
    global UNITS
    UNITS = pvd.Units(attrs0, pvd.SCALED if int(new) == 0 else pvd.SI)
    apply_units()


units_btn = RadioButtonGroup(labels=['Scaled', 'SI'], active=pvd.SI, width=170)
units_btn.on_change('active', _on_units)


def update(attr, old, new):
    idx = int(slider.value)
    xf, yf, attrs = load_field_1d(aperp_files[idx])

    s, intens = temporal_profile(xf, yf, attrs)
    omega, spec = spectral_profile(xf, yf, attrs)

    spec_log = np.where(spec > 0, spec, 1e-30)
    src_intens.data   = dict(x=s.tolist(),         y=intens.tolist())
    _sync_intens_axes(intens)
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

    z  = z_vals[idx]
    zd = z * UNITS.z_mul          # z in the displayed units
    src_vline.data  = dict(x0=[zd], y0=[ylo],  x1=[zd], y1=[yhi])
    src_evline.data = dict(x0=[zd], y0=[_elo], x1=[zd], y1=[_ehi])
    src_lmark.data  = dict(x0=[zd], y0=[0.0],  x1=[zd], y1=[1.0])



    status.text  = _status_html(idx)


slider.on_change('value', update)

# Playback. Registered after `update` so a timer tick redraws the panels
# before the player's own slider handler runs.
player = pvp.Player(curdoc(), slider, n_files, T)

# ── layout ────────────────────────────────────────────────────────────────────
# A deck with no lattice file would otherwise get an empty titled panel with
# just an axis in it — the power plot already carries the z cue, so drop it
# rather than show a blank.
_panels = [status,
           row(player.layout(inset=SLIDER_INSET, top=0, bottom=0),
               units_btn),
           slider]
if latt_elements:
    _panels.append(p_latt)
# The temporal panel carries two y-axes, so it gets a full-width row of its
# own rather than a third of one — at TW the axes left almost no plot area.
_panels += [row(p_power, p_energy),
            p_intens,
            row(p_spec, p_spec_log),
            row(p_current, p_phase)]

curdoc().add_root(column(*_panels))
curdoc().title = f'Puffin 1D Field Viewer — {basename}'
