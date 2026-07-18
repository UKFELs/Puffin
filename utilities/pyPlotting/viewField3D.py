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
matplotlib.use('MacOSX')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.widgets import Slider

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
    for p in int_files:
        with h5py.File(p, 'r') as f:
            ri = f['runInfo'].attrs
            pow_z.append(float(ri['zTotal']))
            # periodic mesh: mean over z2 nodes
            pow_W.append(float(np.mean(f['powerSI'][()])))
    pow_z = np.array(pow_z)
    pow_W = np.array(pow_W)
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

field_img_data  = np.mean(np.sqrt(aperp0[0]**2 + aperp0[1]**2), axis=0)
intens_img_data = np.mean(aperp0[0]**2 + aperp0[1]**2, axis=0) * iscale0

im_field  = ax_field.imshow(field_img_data,  origin='lower', extent=extent0,
                             cmap='inferno', aspect='equal')
im_intens = ax_intens.imshow(intens_img_data, origin='lower', extent=extent0,
                              cmap='inferno', aspect='equal')

cb0 = fig.colorbar(im_field,  ax=ax_field,  fraction=0.046, pad=0.04)
cb1 = fig.colorbar(im_intens, ax=ax_intens, fraction=0.046, pad=0.04)
cb0.set_label('|A⊥|  (z₂-avg, scaled)')
cb1.set_label('Intensity  (W m⁻²,  z₂-avg)')

ax_field.set_xlabel('x (mm)');  ax_field.set_ylabel('y (mm)')
ax_intens.set_xlabel('x (mm)'); ax_intens.set_ylabel('y (mm)')
ax_field.set_title('Field magnitude')
ax_intens.set_title('Intensity')


# ── power vs z plot ───────────────────────────────────────────────────────────

if has_power:
    ax_power.semilogy(pow_z, pow_W, color='royalblue', lw=1.4)
    ax_power.set_xlabel('z (m)')
    ax_power.set_ylabel('Power (W)')
    ax_power.set_title('Power vs z')
    ax_power.set_xlim(pow_z[0], pow_z[-1])
    ax_power.grid(True, which='both', ls='--', alpha=0.4)
    vline = ax_power.axvline(x=z_vals[0], color='red', lw=1.5, ls='--')


# ── slider (z in metres) ──────────────────────────────────────────────────────

slider = Slider(ax_slider, 'z (m)', 0, n_files - 1,
                valinit=0, valstep=1, color='steelblue')
slider.valtext.set_text(f'{z_vals[0]:.3f} m')

suptitle = fig.suptitle('', fontsize=11)

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

    fi = np.mean(np.sqrt(aperp[0]**2 + aperp[1]**2), axis=0)
    ii = np.mean(aperp[0]**2 + aperp[1]**2, axis=0) * iscale

    im_field.set_data(fi);  im_field.set_extent(ext);  im_field.set_clim(fi.min(), fi.max())
    im_intens.set_data(ii); im_intens.set_extent(ext); im_intens.set_clim(ii.min(), ii.max())

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
