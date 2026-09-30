#!/usr/bin/env python3
"""
Interactive viewer for Puffin 3D field output files (aperp_*.h5).

Usage:
    python viewField3D.py [data_dir] [basename]

    data_dir  - directory containing aperp/integrated HDF5 files (default: .)
    basename  - run name prefix (default: auto-detected)

Controls:
    Slider          - scrub through output steps; label shows z (m)
    Left/Right keys - step backward / forward one file
"""

import sys, os, glob, re
import numpy as np
import h5py
import matplotlib
# MacOSX is the interactive default here, but honour MPLBACKEND so the script
# still runs headless (CI, a Linux box, rendering a PNG).
if not os.environ.get('MPLBACKEND'):
    matplotlib.use('MacOSX')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.widgets import Slider
from matplotlib.ticker import LogLocator, ScalarFormatter

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import puffin_viz_theme as pvt
import puffin_viz_data as pvd

T = pvt.tokens()
pvt.apply_matplotlib_style(T)

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
    return [-nx/2*dx*1e3, nx/2*dx*1e3, -ny/2*dy*1e3, ny/2*dy*1e3]


def load_aperp(path):
    with h5py.File(path, 'r') as f:
        aperp = f['aperp'][()]          # (2, nz2, ny, nx)
        attrs = dict(f['runInfo'].attrs)
    return aperp, attrs


def clim(a):
    """Colour limits, guarding the degenerate all-equal case.

    The first dump of a run seeded from noise is identically zero. Passing
    min == max to set_clim makes matplotlib pad the range symmetrically, so
    a blank field lands mid-ramp — Inferno's midpoint is a strong crimson,
    and an empty mesh comes out looking like a saturated one. Anchor to
    [0, 1] instead, which puts "nothing" at the dark end of the ramp.
    """
    lo, hi = float(np.nanmin(a)), float(np.nanmax(a))
    return (lo, hi) if hi > lo else (0.0, 1.0)


# ── collect aperp files ───────────────────────────────────────────────────────

data_dir = sys.argv[1] if len(sys.argv) > 1 else '.'
basename = sys.argv[2] if len(sys.argv) > 2 else None

all_h5 = glob.glob(os.path.join(data_dir, '*_aperp_*.h5'))
if not all_h5:
    sys.exit(f'No aperp HDF5 files found in {data_dir}')

if basename is None:
    basename = re.sub(r'_aperp_\d+\.h5$', '',
                      os.path.basename(sorted(all_h5, key=step_num)[0]))

aperp_files = sorted(
    glob.glob(os.path.join(data_dir, f'{basename}_aperp_*.h5')),
    key=step_num)
n_files = len(aperp_files)
print(f'Found {n_files} aperp files  (basename="{basename}")')


# ── pre-load z positions from aperp attrs (fast — no data read) ───────────────

print('Scanning z positions...', end=' ', flush=True)
z_vals = []
for p in aperp_files:
    with h5py.File(p, 'r') as f:
        z_vals.append(float(f['runInfo'].attrs['zTotal']))
z_vals = np.array(z_vals)
print('done')


# ── pre-load power curve from integrated files ────────────────────────────────

int_files = sorted(
    glob.glob(os.path.join(data_dir, f'{basename}_integrated_*.h5')),
    key=step_num)

pow_z, pow_W = [], []
if int_files:
    print(f'Loading power curve from {len(int_files)} integrated files...',
          end=' ', flush=True)
    pow_attrs = {}
    for p in int_files:
        with h5py.File(p, 'r') as f:
            pow_attrs = dict(f['runInfo'].attrs)
            pow_z.append(float(pow_attrs['zTotal']))
            # mean over z2 on a periodic mesh, peak on a temporal one — see
            # puffin_viz_data.reduce_power
            pow_W.append(pvd.reduce_power(f['powerSI'][()], pow_attrs)[0])
    pow_z = np.array(pow_z)
    # Zero power (the seed dump before any gain) has no place on a log axis:
    # semilogy drops it silently, but it still drags the data minimum to 0 and
    # breaks any range test downstream. Mask it, as the bokeh viewers do.
    pow_W = np.where(np.array(pow_W) > 0, pow_W, np.nan)
    print('done')
else:
    print('No integrated files found — power curve unavailable')


# ── figure layout ─────────────────────────────────────────────────────────────

fig = plt.figure(figsize=(13, 8))
has_power = len(pow_z) > 0

if has_power:
    gs = gridspec.GridSpec(
        3, 2,
        figure=fig,
        height_ratios=[5, 3, 0.6],
        hspace=0.45, wspace=0.35,
        bottom=0.08, top=0.93, left=0.08, right=0.97)
    ax_field  = fig.add_subplot(gs[0, 0])
    ax_intens = fig.add_subplot(gs[0, 1])
    ax_power  = fig.add_subplot(gs[1, :])
    ax_slider = fig.add_subplot(gs[2, :])
else:
    gs = gridspec.GridSpec(
        2, 2,
        figure=fig,
        height_ratios=[10, 0.6],
        hspace=0.4, wspace=0.35,
        bottom=0.1, top=0.93, left=0.08, right=0.97)
    ax_field  = fig.add_subplot(gs[0, 0])
    ax_intens = fig.add_subplot(gs[0, 1])
    ax_slider = fig.add_subplot(gs[1, :])


# ── initial field load ────────────────────────────────────────────────────────

aperp0, attrs0 = load_aperp(aperp_files[0])
iscale0 = intens_scale(attrs0)
extent0 = xy_extent_mm(attrs0)

def xy_reduce(aperp, iscale):
    """Transverse |A| and intensity, collapsed over z2.

    Periodic mesh: average, which is the honest cycle average. Temporal
    mesh: the peak-power z2 node, because the window is mostly empty either
    side of the pulse and averaging over it understates the peak intensity
    by ~12x (fig7a) while smearing slices whose spot size varies along the
    pulse. The bokeh viewer exposes this as a control; here it is automatic.
    """
    if pvd.mesh_is_periodic(attrs0):
        return (np.mean(np.sqrt(aperp[0]**2 + aperp[1]**2), axis=0),
                np.mean(aperp[0]**2 + aperp[1]**2, axis=0) * iscale,
                None)
    iz2 = int((aperp[0]**2 + aperp[1]**2).sum(axis=(1, 2)).argmax())
    return (np.sqrt(aperp[0, iz2]**2 + aperp[1, iz2]**2),
            (aperp[0, iz2]**2 + aperp[1, iz2]**2) * iscale,
            iz2)


def xy_label(iz2):
    return 'z₂-avg' if iz2 is None else f'peak z₂ slice {iz2}'


field_img_data, intens_img_data, _iz0 = xy_reduce(aperp0, iscale0)

im_field  = ax_field.imshow(field_img_data,  origin='lower', extent=extent0,
                             cmap='inferno', aspect='equal')
im_intens = ax_intens.imshow(intens_img_data, origin='lower', extent=extent0,
                              cmap='inferno', aspect='equal')

cb0 = fig.colorbar(im_field,  ax=ax_field,  fraction=0.046, pad=0.04)
cb1 = fig.colorbar(im_intens, ax=ax_intens, fraction=0.046, pad=0.04)
cb0.set_label('|A⊥|  (scaled)')
cb1.set_label('Intensity  (W m⁻²)')
for cb in (cb0, cb1):
    cb.outline.set_visible(False)
    cb.ax.tick_params(color=T['axis'], labelcolor=T['muted'], labelsize=9)

ax_field.set_xlabel('x (mm)');  ax_field.set_ylabel('y (mm)')
ax_intens.set_xlabel('x (mm)'); ax_intens.set_ylabel('y (mm)')
ax_field.set_title('Field magnitude')
ax_intens.set_title('Intensity')
# the images carry their own scale; a grid over them is just noise
for ax in (ax_field, ax_intens):
    ax.grid(False)


# ── power vs z plot ───────────────────────────────────────────────────────────

if has_power:
    ax_power.semilogy(pow_z, pow_W, color=T['field'], lw=2.0)
    ax_power.set_xlabel('z (m)')
    ax_power.set_ylabel(pvd.power_axis_label(pow_attrs))
    ax_power.set_title(pvd.power_title(pow_attrs))
    ax_power.set_xlim(pow_z[0], pow_z[-1])
    # Major only: on a log axis 'both' draws nine minor lines per decade,
    # which turns the panel into hatching.
    ax_power.grid(True, which='major', ls='-', lw=0.8, color=T['grid'])
    ax_power.grid(False, which='minor')
    ax_power.set_axisbelow(True)

    # A short run can span less than one decade, which leaves the log axis
    # with a single labelled tick and nothing to read values against. Label
    # the 2/3/5 minors in that case.
    _lo, _hi = np.nanmin(pow_W), np.nanmax(pow_W)
    if np.isfinite(_lo) and _lo > 0 and _hi / _lo < 100:
        ax_power.yaxis.set_minor_locator(LogLocator(base=10, subs=(2, 3, 5)))
        ax_power.yaxis.set_minor_formatter(ScalarFormatter())
        ax_power.tick_params(axis='y', which='minor', labelsize=8)
    for side in ('top', 'right'):
        ax_power.spines[side].set_visible(False)
    # current-z cue: chrome, not a series
    vline = ax_power.axvline(x=z_vals[0], color=T['marker'], lw=2, ls='--')


# ── slider (z in metres) ──────────────────────────────────────────────────────

ax_slider.grid(False)
# Slider draws its value text just outside its right edge, so give that text
# room inside the figure rather than letting it run off the canvas.
_sp = ax_slider.get_position()
ax_slider.set_position([_sp.x0, _sp.y0, _sp.width * 0.92, _sp.height])
slider = Slider(ax_slider, 'z (m)', 0, n_files - 1,
                valinit=0, valstep=1, color=T['field'])
slider.valtext.set_text(f'{z_vals[0]:.3f} m')
slider.valtext.set_color(T['ink'])
slider.label.set_color(T['ink2'])

suptitle = fig.suptitle('', fontsize=11, color=T['ink'])

def update_suptitle(idx):
    suptitle.set_text(
        f'Step {idx+1}/{n_files}  |  file step {step_num(aperp_files[idx])}'
        f'  |  z = {z_vals[idx]:.4f} m')

update_suptitle(0)

current = [0]


def draw_step(idx):
    aperp, attrs = load_aperp(aperp_files[idx])
    iscale = intens_scale(attrs)
    ext    = xy_extent_mm(attrs)

    fi, ii, iz2 = xy_reduce(aperp, iscale)
    ax_field.set_title(f'Field magnitude  ({xy_label(iz2)})')
    ax_intens.set_title(f'Intensity  ({xy_label(iz2)})')

    im_field.set_data(fi);  im_field.set_extent(ext);  im_field.set_clim(*clim(fi))
    im_intens.set_data(ii); im_intens.set_extent(ext); im_intens.set_clim(*clim(ii))

    if has_power:
        vline.set_xdata([z_vals[idx], z_vals[idx]])

    slider.valtext.set_text(f'{z_vals[idx]:.3f} m')
    update_suptitle(idx)
    fig.canvas.draw_idle()


def on_slider(val):
    idx = int(slider.val)
    current[0] = idx
    draw_step(idx)

slider.on_changed(on_slider)


def on_key(event):
    idx = current[0]
    if   event.key == 'right': idx = min(idx + 1, n_files - 1)
    elif event.key == 'left':  idx = max(idx - 1, 0)
    else: return
    current[0] = idx
    slider.set_val(idx)

fig.canvas.mpl_connect('key_press_event', on_key)

draw_step(0)
plt.show()
