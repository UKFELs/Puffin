#!/usr/bin/env python3
"""
Interactive Bokeh viewer for Puffin 3D field output (aperp_*.h5).

Run with:
    bokeh serve --show viewField3D_bokeh.py --args /path/to/data [basename] [lattice.latt]

Controls:
    Slider  — scrub through output steps; title shows z (m)
"""

import sys, os, glob, re
import numpy as np
import h5py

from bokeh.plotting import figure, curdoc
from bokeh.models import (ColumnDataSource, Slider, LinearColorMapper,
                           ColorBar, Div, Segment, HoverTool, CrosshairTool,
                           Range1d, PrintfTickFormatter, RadioButtonGroup,
                           Span)
from bokeh.layouts import column, row
from bokeh.palettes import Inferno256

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

def xy_extent(attrs, u):
    """(xmin, xmax, ymin, ymax) in u's transverse units."""
    dx = float(attrs['sLengthOfElmX']) * u.xy_mul
    dy = float(attrs['sLengthOfElmY']) * u.xy_mul
    nx, ny = int(attrs['nX']), int(attrs['nY'])
    return -nx/2*dx, nx/2*dx, -ny/2*dy, ny/2*dy

def load_aperp(path):
    with h5py.File(path, 'r') as f:
        aperp = f['aperp'][()]          # (2, nz2, ny, nx)
        attrs = dict(f['runInfo'].attrs)
    return aperp, attrs

def slice_power(aperp):
    """Transverse-integrated |A|^2 per z2 node — the pulse profile in z2."""
    return (aperp[0]**2 + aperp[1]**2).sum(axis=(1, 2))


def peak_slice(aperp):
    """Index of the z2 node carrying the most power."""
    return int(slice_power(aperp).argmax())


def field_and_intens(aperp, iscale, iz2=None):
    """Transverse |A| and intensity, either at one z2 node or averaged.

    iz2 = None averages over the whole z2 window. That is a true cycle
    average on a periodic mesh, but on a temporal mesh the window is mostly
    empty either side of the pulse — 70% of nodes sit below 1% of peak in the
    fig7a example — so the average understates the peak intensity by ~12x and
    smears together slices whose spot size varies by ~50% along the pulse.
    Pass an index to see one z2 node instead.
    """
    if iz2 is None:
        mag = np.mean(np.sqrt(aperp[0]**2 + aperp[1]**2), axis=0)
        ixy = np.mean(aperp[0]**2 + aperp[1]**2, axis=0) * iscale
    else:
        iz2 = int(np.clip(iz2, 0, aperp.shape[1] - 1))
        mag = np.sqrt(aperp[0, iz2]**2 + aperp[1, iz2]**2)
        ixy = (aperp[0, iz2]**2 + aperp[1, iz2]**2) * iscale
    return mag, ixy


# Active unit system. UNITS is rebuilt whenever the toggle changes; every
# axis label and factor is read from it rather than hard-coded.
UNITS = None


def clim(a):
    """Colour-mapper limits, guarding the degenerate all-equal case.

    The first dump of a run seeded from noise is identically zero, and
    low == high makes the mapper put every cell at the same point on the
    ramp — mid-Inferno is a strong crimson, so an empty mesh reads as a
    saturated one. Anchor to [0, 1] so "nothing" sits at the dark end.
    """
    lo, hi = float(np.nanmin(a)), float(np.nanmax(a))
    return (lo, hi) if hi > lo else (0.0, 1.0)


# ── parse arguments ───────────────────────────────────────────────────────────
# bokeh serve passes everything after --args into sys.argv[1:]
args = sys.argv[1:]
data_dir  = args[0] if len(args) > 0 else '.'
basename  = args[1] if len(args) > 1 else None
latt_arg  = args[2] if len(args) > 2 else None


# ── collect aperp files ───────────────────────────────────────────────────────
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


# ── pre-scan z positions (attrs only — fast) ──────────────────────────────────
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
pow_z = np.array([])
int_z_vals = np.array([])
pow_attrs = {}
# Both unit systems are built up front: toggling units must not re-read the
# whole integrated set (316 files on the fig7a run).
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
        # guard against zeros on log axis
        POW_W[mode] = np.where(np.array(w) > 0, w, np.nan)
        POW_J[mode] = np.where(np.array(j) > 0, j, np.nan)
    int_z_vals = pow_z.copy()
    print(f'done  ({pvd.reduce_power(np.array([1.0]), pow_attrs)[1]})', flush=True)


def _closest_int_file(z):
    """Integrated file whose z is nearest z, or None.

    The integrated files are written on their own (finer) cadence, so they do
    not line up with the aperp steps the slider indexes.
    """
    if len(int_z_vals) == 0:
        return None
    return int_files[int(np.argmin(np.abs(int_z_vals - z)))]


def load_power_profile(int_path, u):
    """(z2 axis, power) — the temporal profile at one z, in u's units."""
    with h5py.File(int_path, 'r') as f:
        p = f[u.power_key][()]
        a = dict(f['runInfo'].attrs)
        b = pvd.mesh_bounds(f)
    return u.z2_axis(len(p), a, b), p


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
                L = int(nums[0]) * lw;  elements.append(('UN', z, L));  z += L
            elif tag == 'DR' and nums:
                L = nums[0] * lw;       elements.append(('DR', z, L));  z += L
            elif tag == 'CH' and nums:
                L = nums[0] * lw
                if L > 0: elements.append(('CH', z, L));  z += L
            elif tag == 'QU':
                elements.append(('QU', z, 0.0))
    return elements, z

# find lattice file
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
    print(f'Lattice: {len(latt_elements)} elements, total z = {latt_z_total:.3f} m', flush=True)


# ── initial field data ────────────────────────────────────────────────────────
aperp0, attrs0 = load_aperp(aperp_files[0])
iscale0 = intens_scale(attrs0)

UNITS = pvd.Units(attrs0, pvd.SI)     # default to SI; toggle switches it
NZ2 = int(aperp0.shape[1])
z2_ax = UNITS.z2_axis(NZ2, attrs0)    # ct-z / z2 for the aperp mesh
xmin, xmax, ymin, ymax = xy_extent(attrs0, UNITS)

# How the x-y panels collapse z2. On a periodic mesh the window is one
# radiation period and averaging is the honest cycle average; on a temporal
# mesh it is mostly empty window, so track the peak-power slice instead.
MODE_PEAK, MODE_MANUAL, MODE_AVG = 0, 1, 2
xy_mode = MODE_AVG if pvd.mesh_is_periodic(attrs0) else MODE_PEAK
xy_slice = peak_slice(aperp0)


def _xy_index(aperp):
    """z2 index the x-y panels should show, or None for the average."""
    if xy_mode == MODE_AVG:
        return None
    if xy_mode == MODE_PEAK:
        return peak_slice(aperp)
    return xy_slice


def _xy_suffix(iz2):
    if iz2 is None:
        return '  (z₂-avg)'
    # z2_unit is empty in scaled units, where the axis is dimensionless
    at = f'{z2_ax[iz2]:.4g}'
    if UNITS.z2_unit:
        at += f' {UNITS.z2_unit}'
    return f'  (z₂ slice {iz2} @ {at})'


_i0 = _xy_index(aperp0)
fi0, ii0 = field_and_intens(aperp0, UNITS.intensity_factor(iscale0), _i0)


# ── figures ───────────────────────────────────────────────────────────────────
TOOLS = 'pan,wheel_zoom,box_zoom,reset,save'
IW, IH = 470, 390   # image panel size (wide enough for the colorbar labels)

# Colorbar values are tiny (|A|) or huge (W/m²), so they always carry an
# exponent. "%.1e" keeps that exponent while staying short enough not to be
# clipped at the panel edge — the default "6.000e-4" is not.
CB_FMT = PrintfTickFormatter(format='%.1e')
SLIDER_INSET = 90   # left margin keeping the slider off the window edge

T = pvt.tokens()
curdoc().theme = pvt.bokeh_theme(T)
pvt.apply_page_style(curdoc(), T)

_lo_f, _hi_f = clim(fi0)
_lo_i, _hi_i = clim(ii0)
mapper_f = LinearColorMapper(palette=Inferno256, low=_lo_f, high=_hi_f)
mapper_i = LinearColorMapper(palette=Inferno256, low=_lo_i, high=_hi_i)

src_field  = ColumnDataSource(dict(image=[fi0],  x=[xmin], y=[ymin],
                                   dw=[xmax-xmin], dh=[ymax-ymin]))
src_intens = ColumnDataSource(dict(image=[ii0], x=[xmin], y=[ymin],
                                   dw=[xmax-xmin], dh=[ymax-ymin]))

# Sequential magnitude, so a perceptually-uniform ramp: Inferno is monotonic
# in lightness and CVD-safe (it is not a jet-style rainbow), and it is what
# readers of an FEL transverse profile expect.
# toolbar above, not overlaid: these panels carry a colorbar on the right,
# and the default in-frame toolbar sits straight on top of its tick labels.
p_field = figure(title='Field magnitude' + _xy_suffix(_i0),
                 x_axis_label=UNITS.x_label, y_axis_label=UNITS.y_label,
                 width=IW, height=IH, tools=TOOLS, toolbar_location='above')
_r = p_field.image('image', source=src_field, x='x', y='y', dw='dw', dh='dh',
                   color_mapper=mapper_f)
p_field.add_tools(HoverTool(renderers=[_r], tooltips=[
    ('x', '$x{0.00} mm'), ('y', '$y{0.00} mm'), ('|A⊥|', '@image{0.000}')]))
p_field.add_layout(ColorBar(color_mapper=mapper_f, label_standoff=8,
                             width=12, title='|A⊥|',
                             formatter=CB_FMT), 'right')

p_intens = figure(title='Intensity' + _xy_suffix(_i0),
                  x_axis_label=UNITS.x_label, y_axis_label=UNITS.y_label,
                  width=IW, height=IH, tools=TOOLS, toolbar_location='above')
_r = p_intens.image('image', source=src_intens, x='x', y='y', dw='dw', dh='dh',
                    color_mapper=mapper_i)
p_intens.add_tools(HoverTool(renderers=[_r], tooltips=[
    ('x', '$x{0.00} mm'), ('y', '$y{0.00} mm'), ('I', '@image{0.00e+0} W/m²')]))
cbar_i = ColorBar(color_mapper=mapper_i, label_standoff=8,
                  width=12, title=UNITS.intens_label, formatter=CB_FMT)
p_intens.add_layout(cbar_i, 'right')

# power vs z
PW = IW * 2 + 20
# Series for the active unit system; apply_units() swaps these on toggle.
pow_W = POW_W[UNITS.mode]
pow_J = POW_J[UNITS.mode]

# Range first: it decides the axis type, so it has to be known before the
# figure is built. Every dump can be zero power (a seeded run that has not
# gained yet) and those are masked to NaN above — so check for a finite
# value, not just a non-empty array, or nanmin warns and returns NaN.
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
# for 20 m of undulators that were never simulated.
_zs = np.concatenate([a for a in (z_vals, pow_z) if len(a)]) * UNITS.z_mul
_zpad = max((float(_zs.max()) - float(_zs.min())) * 0.02, 1e-6)
_zrange = Range1d(start=float(_zs.min()) - _zpad, end=float(_zs.max()) + _zpad)

HW = PW // 2 - 5   # half-width, for the peak/energy pair

p_power = figure(title=UNITS.power_title(pow_attrs),
                 x_axis_label=UNITS.z_label,
                 y_axis_label=UNITS.power_label,
                 x_range=_zrange,
                 width=HW, height=230, tools=TOOLS, y_axis_type=_pow_axis)
if len(pow_z):
    src_pow = ColumnDataSource(dict(x=(pow_z*UNITS.z_mul).tolist(),
                                    y=pow_W.tolist()))
    _r = p_power.line('x', 'y', source=src_pow, color=T['field'], line_width=2)
    p_power.add_tools(
        HoverTool(renderers=[_r], mode='vline', attachment='above',
                  tooltips=[('z', '@x{0.000} m'),
                            (pvd.reduce_power(np.array([1.0]), pow_attrs)[1],
                             '@y{0.00e+0} W')]),
        CrosshairTool(line_color=T['muted'], line_alpha=0.6, line_width=1))

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
    p_energy.add_tools(
        HoverTool(renderers=[_r], mode='vline', attachment='above',
                  tooltips=[('z', '@x{0.000} m'), ('E', '@y{0.000e+0} J')]),
        CrosshairTool(line_color=T['muted'], line_alpha=0.6, line_width=1))

src_evline = ColumnDataSource(dict(x0=[z_vals[0]*UNITS.z_mul], y0=[_elo],
                                   x1=[z_vals[0]*UNITS.z_mul], y1=[_ehi]))
p_energy.segment('x0', 'y0', 'x1', 'y1', source=src_evline,
                 color=T['marker'], line_dash='dashed', line_width=2)

# ── temporal profile: power vs ct-z at the current step ───────────────────────
_ip0 = _closest_int_file(z_vals[0])
if _ip0:
    _tx0, _ty0 = load_power_profile(_ip0, UNITS)
else:
    _tx0, _ty0 = np.array([0.0]), np.array([0.0])
src_tprof = ColumnDataSource(dict(x=_tx0.tolist(), y=_ty0.tolist()))

p_tprof = figure(title='Temporal profile  (at this z)',
                 x_axis_label=UNITS.z2_label, y_axis_label=UNITS.power_label,
                 width=PW, height=250, tools=TOOLS)
_r = p_tprof.line('x', 'y', source=src_tprof, color=T['field'], line_width=2)
p_tprof.add_tools(
    HoverTool(renderers=[_r], mode='vline', attachment='above',
              tooltips=[('ct−z', '@x{0.00} µm'), ('P', '@y{0.00e+0} W')]),
    CrosshairTool(line_color=T['muted'], line_alpha=0.6, line_width=1))

# Marks which z2 node the x-y panels below are showing. It is a cursor into
# the pulse, not data, so it wears the select hue.
slice_span = Span(location=float(z2_ax[_i0]) if _i0 is not None else 0.0,
                  dimension='height', line_color=T['select'],
                  line_width=2, line_dash='dashed',
                  visible=_i0 is not None)
p_tprof.add_layout(slice_span)


# ── x-y slice controls ────────────────────────────────────────────────────────
xy_mode_btn = RadioButtonGroup(
    labels=['Peak slice', 'Manual slice', 'z₂ average'],
    active=xy_mode, width=330)

xy_slice_sl = Slider(start=0, end=max(NZ2 - 1, 1), value=xy_slice, step=1,
                     # PW less the inset, the mode buttons and the gaps —
                     # a wider slider overflows the page and gets clipped.
                     title='z₂ node', width=PW - SLIDER_INSET - 330 - 60,
                     disabled=xy_mode != MODE_MANUAL,
                     margin=(0, 10, 0, 14))

xy_note = Div(text='', width=PW, margin=(0, 10, 6, SLIDER_INSET))


def _xy_note_html(iz2):
    if iz2 is None:
        return (f'<span style="font-family:{pvt.FONT}; font-size:11px; '
                f'color:{T["muted"]};">x-y panels average over all {NZ2} z₂ '
                f'nodes — a true cycle average on a periodic mesh, but on a '
                f'pulse it includes the empty window either side.</span>')
    return (f'<span style="font-family:{pvt.FONT}; font-size:11px; '
            f'color:{T["muted"]};">x-y panels show z₂ node {iz2} of {NZ2} '
            f'at {UNITS.z2_label} = {z2_ax[iz2]:.4g} (dashed cursor above).</span>')


xy_note.text = _xy_note_html(_i0)

# The current-z cue is chrome, not a series: primary ink, dashed.
src_vline = ColumnDataSource(dict(x0=[z_vals[0]*UNITS.z_mul], y0=[ylo],
                                  x1=[z_vals[0]*UNITS.z_mul], y1=[yhi]))
p_power.segment('x0', 'y0', 'x1', 'y1', source=src_vline,
                color=T['marker'], line_dash='dashed', line_width=2)

# lattice diagram
p_latt = figure(title='Lattice',
                x_axis_label=UNITS.z_label,
                x_range=p_power.x_range,   # share x axis with power plot
                width=PW, height=90,
                tools='pan,reset',
                y_range=(-0.05, 1.05))
p_latt.yaxis.visible = False
p_latt.ygrid.visible = False
p_latt.xgrid.visible = False

if latt_elements:
    # CDS-backed so the element positions can be rescaled when the unit
    # system changes; plain lists would be baked in at construction.
    src_dr = ColumnDataSource(dict(x=[], y=[], w=[], h=[]))
    src_un = ColumnDataSource(dict(x=[], y=[], w=[], h=[]))
    src_ch = ColumnDataSource(dict(x=[], y=[], w=[], h=[]))
    src_qu = ColumnDataSource(dict(x0=[], y0=[], x1=[], y1=[]))

    def _relayout_lattice():
        """Rebuild the lattice glyph positions for the current units."""
        zmul = UNITS.z_mul
        span = (latt_z_total * zmul) if latt_z_total > 0 else 1.0
        # A 2px gap in the surface colour separates touching elements;
        # convert 2px of the plot's own width into the displayed z units.
        gap = 2.0 / PW * span
        d = {k: dict(x=[], y=[], w=[], h=[]) for k in ('DR', 'UN', 'CH')}
        q = dict(x0=[], y0=[], x1=[], y1=[])
        for typ, zs, L in latt_elements:
            zs, L = zs * zmul, L * zmul
            Lw = max(L - gap, L * 0.5)   # keep the gap from eating a short element
            if typ == 'QU':
                q['x0'].append(zs); q['y0'].append(0.05)
                q['x1'].append(zs); q['y1'].append(0.95)
            elif typ in d and (L > 0 or typ == 'UN'):
                h = {'UN': 0.9, 'DR': 0.5, 'CH': 0.7}[typ]
                d[typ]['x'].append(zs + L/2); d[typ]['y'].append(0.5)
                d[typ]['w'].append(Lw);       d[typ]['h'].append(h)
        src_dr.data, src_un.data, src_ch.data = d['DR'], d['UN'], d['CH']
        src_qu.data = q

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
    if src_qu.data['x0']:
        p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_qu,
                       color=T['select'], line_width=2, legend_label='Quad')

    pvt.style_legend(p_latt, T)
    p_latt.add_layout(p_latt.legend[0], 'below')

# z marker on lattice (shared x with power plot, so same vline source works)
src_lmark = ColumnDataSource(dict(x0=[z_vals[0]*UNITS.z_mul], y0=[0.0],
                                  x1=[z_vals[0]*UNITS.z_mul], y1=[1.0]))
p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_lmark,
               color=T['marker'], line_dash='dashed', line_width=2)

# ── status + slider ───────────────────────────────────────────────────────────
def _status_html(idx):
    return pvt.header_html(
        T, f'{basename} · 3D field', z_vals[idx] * UNITS.z_mul, idx + 1, n_files,
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


# Cache the loaded step so changing only the slice mode does not re-read a
# 280 MB aperp file from disk.
_cur = {'idx': None, 'aperp': None, 'attrs': None}


def _load_step(idx):
    if _cur['idx'] != idx:
        a, at = load_aperp(aperp_files[idx])
        _cur.update(idx=idx, aperp=a, attrs=at)
    return _cur['aperp'], _cur['attrs']


def refresh_xy():
    """Redraw the x-y panels for the current step and slice mode."""
    aperp, attrs = _load_step(int(slider.value))
    iscale = intens_scale(attrs)
    x0, x1, y0, y1 = xy_extent(attrs, UNITS)

    iz2 = _xy_index(aperp)
    fi, ii = field_and_intens(aperp, UNITS.intensity_factor(iscale), iz2)

    src_field.data  = dict(image=[fi],  x=[x0], y=[y0], dw=[x1-x0], dh=[y1-y0])
    src_intens.data = dict(image=[ii], x=[x0], y=[y0], dw=[x1-x0], dh=[y1-y0])
    mapper_f.low, mapper_f.high = clim(fi)
    mapper_i.low, mapper_i.high = clim(ii)

    p_field.title.text  = 'Field magnitude' + _xy_suffix(iz2)
    p_intens.title.text = 'Intensity' + _xy_suffix(iz2)
    slice_span.visible  = iz2 is not None
    if iz2 is not None:
        slice_span.location = float(z2_ax[iz2])
        # keep the manual slider in step when Peak mode moves the cursor
        if xy_mode == MODE_PEAK and xy_slice_sl.value != iz2:
            xy_slice_sl.value = iz2
    xy_note.text = _xy_note_html(iz2)


def apply_units():
    """Re-label and re-scale every panel for the current UNITS."""
    global pow_W, pow_J, ylo, yhi, _elo, _ehi, z2_ax

    z2_ax = UNITS.z2_axis(NZ2, attrs0)
    pow_W = POW_W[UNITS.mode]
    pow_J = POW_J[UNITS.mode]

    # z axis
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
    p_tprof.xaxis.axis_label = UNITS.z2_label
    p_tprof.yaxis.axis_label = UNITS.power_label
    for f in (p_field, p_intens):
        f.xaxis.axis_label = UNITS.x_label
        f.yaxis.axis_label = UNITS.y_label
    cbar_i.title = UNITS.intens_label
    if latt_elements:
        p_latt.xaxis.axis_label = UNITS.z_label
        _relayout_lattice()   # defined only when latt_elements is non-empty

    update(None, None, None)


def _on_units(attr, old, new):
    global UNITS
    UNITS = pvd.Units(attrs0, pvd.SCALED if int(new) == 0 else pvd.SI)
    apply_units()


units_btn = RadioButtonGroup(labels=['Scaled', 'SI'], active=pvd.SI, width=170)
units_btn.on_change('active', _on_units)


def _on_xy_mode(attr, old, new):
    global xy_mode
    xy_mode = int(new)
    xy_slice_sl.disabled = xy_mode != MODE_MANUAL
    refresh_xy()


def _on_xy_slice(attr, old, new):
    global xy_slice
    xy_slice = int(new)
    if xy_mode == MODE_MANUAL:
        refresh_xy()


xy_mode_btn.on_change('active', _on_xy_mode)
xy_slice_sl.on_change('value', _on_xy_slice)


def update(attr, old, new):
    idx = int(slider.value)
    refresh_xy()

    z = z_vals[idx]
    zd = z * UNITS.z_mul          # z in the displayed units
    src_vline.data  = dict(x0=[zd], y0=[ylo],  x1=[zd], y1=[yhi])
    src_evline.data = dict(x0=[zd], y0=[_elo], x1=[zd], y1=[_ehi])
    src_lmark.data  = dict(x0=[zd], y0=[0.0],  x1=[zd], y1=[1.0])

    # temporal profile from the integrated dump nearest this z
    ip = _closest_int_file(z)
    if ip:
        tx, ty = load_power_profile(ip, UNITS)
        src_tprof.data = dict(x=tx.tolist(), y=ty.tolist())
    else:
        src_tprof.data = dict(x=[], y=[])

    status.text  = _status_html(idx)


slider.on_change('value', update)

# Playback. Registered after `update` so a timer tick redraws the panels
# before the player's own slider handler runs.
player = pvp.Player(curdoc(), slider, n_files, T)

# ── layout ────────────────────────────────────────────────────────────────────
# A deck with no lattice file (fig7a, the PhyOfPlasmas examples) would
# otherwise get an empty titled panel with just an axis in it — the power
# plot already carries the z cue, so drop it rather than show a blank.
_panels = [status,
           row(player.layout(inset=SLIDER_INSET, top=0, bottom=0),
               units_btn),
           slider]
if latt_elements:
    _panels.append(p_latt)
_panels += [row(p_power, p_energy),
            p_tprof,
            row(xy_mode_btn, xy_slice_sl,
                margin=(2, 10, 0, SLIDER_INSET)),
            xy_note,
            row(p_field, p_intens)]

curdoc().add_root(column(*_panels))
curdoc().title = f'Puffin Field Viewer — {basename}'
