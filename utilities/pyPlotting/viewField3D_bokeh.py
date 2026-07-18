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
                           ColorBar, Div, Segment)
from bokeh.layouts import column, row
from bokeh.palettes import Inferno256

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

def xy_extent_mm(attrs):
    dx = attrs['sLengthOfElmX'] * np.sqrt(attrs['Lg'] * attrs['Lc'])
    dy = attrs['sLengthOfElmY'] * np.sqrt(attrs['Lg'] * attrs['Lc'])
    nx, ny = int(attrs['nX']), int(attrs['nY'])
    return -nx/2*dx*1e3, nx/2*dx*1e3, -ny/2*dy*1e3, ny/2*dy*1e3

def load_aperp(path):
    with h5py.File(path, 'r') as f:
        aperp = f['aperp'][()]          # (2, nz2, ny, nx)
        attrs = dict(f['runInfo'].attrs)
    return aperp, attrs

def field_and_intens(aperp, iscale):
    mag = np.mean(np.sqrt(aperp[0]**2 + aperp[1]**2), axis=0)  # z2-avg |A|
    ixy = np.mean(aperp[0]**2 + aperp[1]**2, axis=0) * iscale  # z2-avg intensity
    return mag, ixy


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
pow_z, pow_W = np.array([]), np.array([])
if int_files:
    print(f'Loading power curve ({len(int_files)} files)...', end=' ', flush=True)
    pz, pw = [], []
    for p in int_files:
        with h5py.File(p, 'r') as f:
            pz.append(float(f['runInfo'].attrs['zTotal']))
            pw.append(float(np.mean(f['powerSI'][()])))
    pow_z = np.array(pz)
    pow_W = np.array(pw)
    # guard against zeros on log axis
    pow_W = np.where(pow_W > 0, pow_W, np.nan)
    print('done', flush=True)


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
if latt_arg:
    latt_file = latt_arg
else:
    candidates = (glob.glob(os.path.join(data_dir, '*.latt')) +
                  glob.glob(os.path.join(os.path.dirname(os.path.abspath(data_dir)), '*.latt')))
    latt_file = candidates[0] if candidates else None

with h5py.File(aperp_files[0], 'r') as f:
    lambda_w = float(f['runInfo'].attrs.get('lambda_w', 0.0275))

latt_elements, latt_z_total = [], 0.0
if latt_file and os.path.exists(latt_file):
    latt_elements, latt_z_total = parse_lattice(latt_file, lambda_w)
    print(f'Lattice: {len(latt_elements)} elements, total z = {latt_z_total:.3f} m', flush=True)


# ── initial field data ────────────────────────────────────────────────────────
aperp0, attrs0 = load_aperp(aperp_files[0])
iscale0 = intens_scale(attrs0)
xmin, xmax, ymin, ymax = xy_extent_mm(attrs0)
fi0, ii0 = field_and_intens(aperp0, iscale0)


# ── figures ───────────────────────────────────────────────────────────────────
TOOLS = 'pan,wheel_zoom,box_zoom,reset,save'
IW, IH = 430, 390   # image panel size

mapper_f = LinearColorMapper(palette=Inferno256, low=float(fi0.min()), high=float(fi0.max()))
mapper_i = LinearColorMapper(palette=Inferno256, low=float(ii0.min()), high=float(ii0.max()))

src_field  = ColumnDataSource(dict(image=[fi0],  x=[xmin], y=[ymin],
                                   dw=[xmax-xmin], dh=[ymax-ymin]))
src_intens = ColumnDataSource(dict(image=[ii0], x=[xmin], y=[ymin],
                                   dw=[xmax-xmin], dh=[ymax-ymin]))

p_field = figure(title='Field magnitude  (z₂-avg, scaled)',
                 x_axis_label='x (mm)', y_axis_label='y (mm)',
                 width=IW, height=IH, tools=TOOLS)
p_field.image('image', source=src_field, x='x', y='y', dw='dw', dh='dh',
              color_mapper=mapper_f)
p_field.add_layout(ColorBar(color_mapper=mapper_f, label_standoff=8,
                             width=12, title='|A⊥|'), 'right')

p_intens = figure(title='Intensity  (z₂-avg)',
                  x_axis_label='x (mm)', y_axis_label='y (mm)',
                  width=IW, height=IH, tools=TOOLS)
p_intens.image('image', source=src_intens, x='x', y='y', dw='dw', dh='dh',
               color_mapper=mapper_i)
p_intens.add_layout(ColorBar(color_mapper=mapper_i, label_standoff=8,
                              width=12, title='W/m²'), 'right')

# power vs z
PW = IW * 2 + 20
p_power = figure(title='Power vs z', x_axis_label='z (m)', y_axis_label='Power (W)',
                 width=PW, height=220, tools=TOOLS, y_axis_type='log')
if len(pow_z):
    src_pow = ColumnDataSource(dict(x=pow_z.tolist(), y=pow_W.tolist()))
    p_power.line('x', 'y', source=src_pow, color='royalblue', line_width=1.5)
    ylo = float(np.nanmin(pow_W)); yhi = float(np.nanmax(pow_W))
else:
    ylo, yhi = 1e-3, 1e10

src_vline = ColumnDataSource(dict(x0=[z_vals[0]], y0=[ylo],
                                  x1=[z_vals[0]], y1=[yhi]))
p_power.segment('x0', 'y0', 'x1', 'y1', source=src_vline,
                color='red', line_dash='dashed', line_width=1.5)

# lattice diagram
LATT_COLS = {'UN': '#2176AE', 'DR': '#CCCCCC', 'CH': '#E87D2B'}
p_latt = figure(title='Lattice',
                x_axis_label='z (m)',
                x_range=p_power.x_range,   # share x axis with power plot
                width=PW, height=90,
                tools='pan,reset',
                y_range=(-0.05, 1.05))
p_latt.yaxis.visible = False
p_latt.ygrid.visible = False
p_latt.xgrid.visible = False

if latt_elements:
    un_x, un_y, un_w, un_h     = [], [], [], []
    dr_x, dr_y, dr_w, dr_h     = [], [], [], []
    ch_x, ch_y, ch_w, ch_h     = [], [], [], []
    qu_x0, qu_y0, qu_x1, qu_y1 = [], [], [], []

    for typ, zs, L in latt_elements:
        if typ == 'UN':
            un_x.append(zs + L/2); un_y.append(0.5)
            un_w.append(L);        un_h.append(0.9)
        elif typ == 'DR' and L > 0:
            dr_x.append(zs + L/2); dr_y.append(0.5)
            dr_w.append(L);        dr_h.append(0.5)
        elif typ == 'CH' and L > 0:
            ch_x.append(zs + L/2); ch_y.append(0.5)
            ch_w.append(L);        ch_h.append(0.7)
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
    if qu_x0:
        p_latt.segment(qu_x0, qu_y0, qu_x1, qu_y1, color='#D62839',
                       line_width=2, legend_label='Quad')

    p_latt.legend.orientation = 'horizontal'
    p_latt.legend.label_text_font_size = '9pt'
    p_latt.legend.spacing = 10
    p_latt.legend.padding = 4
    p_latt.add_layout(p_latt.legend[0], 'below')

# z marker on lattice (shared x with power plot, so same vline source works)
src_lmark = ColumnDataSource(dict(x0=[z_vals[0]], y0=[0.0],
                                  x1=[z_vals[0]], y1=[1.0]))
p_latt.segment('x0', 'y0', 'x1', 'y1', source=src_lmark,
               color='red', line_dash='dashed', line_width=1.5)

# ── status + slider ───────────────────────────────────────────────────────────
def _status_html(idx):
    return (f'<span style="font-size:13px"><b>Step {idx+1}/{n_files}</b> &nbsp;|&nbsp; '
            f'file step {step_num(aperp_files[idx])} &nbsp;|&nbsp; '
            f'z = {z_vals[idx]:.4f} m</span>')

status = Div(text=_status_html(0), width=PW)

slider = Slider(start=0, end=n_files - 1, value=0, step=1,
                title=f'z = {z_vals[0]:.3f} m', width=PW)


def update(attr, old, new):
    idx = int(slider.value)
    aperp, attrs = load_aperp(aperp_files[idx])
    iscale = intens_scale(attrs)
    x0, x1, y0, y1 = xy_extent_mm(attrs)

    fi, ii = field_and_intens(aperp, iscale)

    src_field.data  = dict(image=[fi],  x=[x0], y=[y0], dw=[x1-x0], dh=[y1-y0])
    src_intens.data = dict(image=[ii], x=[x0], y=[y0], dw=[x1-x0], dh=[y1-y0])
    mapper_f.low, mapper_f.high = float(fi.min()), float(fi.max())
    mapper_i.low, mapper_i.high = float(ii.min()), float(ii.max())

    z = z_vals[idx]
    src_vline.data = dict(x0=[z], y0=[ylo], x1=[z], y1=[yhi])
    src_lmark.data = dict(x0=[z], y0=[0.0], x1=[z], y1=[1.0])

    slider.title = f'z = {z:.3f} m'
    status.text  = _status_html(idx)


slider.on_change('value', update)

# ── layout ────────────────────────────────────────────────────────────────────
curdoc().add_root(column(
    status,
    row(p_field, p_intens),
    p_power,
    p_latt,
    slider,
))
curdoc().title = f'Puffin Field Viewer — {basename}'
