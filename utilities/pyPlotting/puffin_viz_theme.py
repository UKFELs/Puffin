#!/usr/bin/env python3
"""Shared visual theme for the Puffin field viewers.

One place for the palette, fonts and chrome so viewField1D_bokeh.py,
viewField3D_bokeh.py and viewField3D.py read as a single family.

Colour is assigned by the job it does, not by panel order:

    field   — optical / radiation quantities (intensity, spectrum, power)
    beam    — electron-beam quantities (current, phase space)
    select  — the interactive range window (a control, not data)
    drift   — lattice drift space: absence of an element, so it recedes
              into the chrome grey rather than taking a hue

So a colour means the same thing in every panel: blue is light, orange is
electrons. The three hues are the first three slots of the reference
categorical palette, which are the slots that clear the all-pairs
colour-vision gates. Validated with the skill's validator in both modes:

    light  worst all-pairs CVD ΔE 9.2 (deutan), normal-vision ΔE 24.0
    dark   worst all-pairs CVD ΔE 9.4 (deutan), normal-vision ΔE 20.9

Light-mode aqua sits at 2.74:1 against the surface, below the 3:1 line.
That is a documented relief case and is discharged here by the lattice
legend: every element type carries a visible text label, so identity never
rests on colour alone.

Set PUFFIN_VIZ_THEME=dark for the dark surface. The dark column is the
same hues re-stepped for a dark background, not an automatic inversion.
"""

import os

FONT = 'system-ui, -apple-system, "Segoe UI", sans-serif'

_LIGHT = dict(
    mode='light',
    surface='#fcfcfb',   # chart surface
    page='#f9f9f7',      # page plane behind the charts
    ink='#0b0b0b',       # primary text
    ink2='#52514e',      # secondary text
    muted='#898781',     # axis / tick labels
    grid='#e1e0d9',      # hairline gridline
    axis='#c3c2b7',      # baseline / axis line
    field='#2a78d6',     # slot 1 blue   — radiation
    beam='#eb6834',      # slot 2 orange — electrons
    select='#1baf7a',    # slot 3 aqua   — range-selection window
    drift='#c3c2b7',     # lattice drift: chrome, not a series
    marker='#0b0b0b',    # current-z cue
)

_DARK = dict(
    mode='dark',
    surface='#1a1a19',
    page='#0d0d0d',
    ink='#ffffff',
    ink2='#c3c2b7',
    muted='#898781',
    grid='#2c2c2a',
    axis='#383835',
    field='#3987e5',
    beam='#d95926',
    select='#199e70',
    drift='#52514e',
    marker='#ffffff',
)


def tokens(mode=None):
    """Return the token set for `mode` (default from PUFFIN_VIZ_THEME)."""
    mode = (mode or os.environ.get('PUFFIN_VIZ_THEME', 'light')).strip().lower()
    return dict(_DARK) if mode == 'dark' else dict(_LIGHT)


# ── Bokeh ────────────────────────────────────────────────────────────────────

def bokeh_theme(t):
    """A Bokeh Theme carrying the chrome: recessive axes, hairline grid, fonts.

    Anything set here must NOT also be set on the figure in the caller — an
    explicit value on the model beats the theme default.
    """
    from bokeh.themes import Theme
    return Theme(json={'attrs': {
        'Plot': {
            'background_fill_color': t['surface'],
            'border_fill_color': t['page'],
            'outline_line_color': None,
        },
        'Axis': {
            'axis_line_color': t['axis'],
            'major_tick_line_color': t['axis'],
            'minor_tick_line_color': None,
            'axis_label_text_color': t['ink2'],
            'axis_label_text_font': FONT,
            'axis_label_text_font_size': '12px',
            'axis_label_text_font_style': 'normal',
            'major_label_text_color': t['muted'],
            'major_label_text_font': FONT,
            'major_label_text_font_size': '11px',
        },
        'Grid': {
            'grid_line_color': t['grid'],
            'grid_line_width': 1,
            'grid_line_dash': 'solid',
            'grid_line_alpha': 1.0,
        },
        'Title': {
            'text_color': t['ink'],
            'text_font': FONT,
            'text_font_size': '13px',
            'text_font_style': 'bold',
        },
        'Legend': {
            'label_text_color': t['ink2'],
            'label_text_font': FONT,
            'label_text_font_size': '11px',
            'background_fill_color': t['surface'],
            'background_fill_alpha': 0.85,
            'border_line_color': None,
            'glyph_height': 12,
            'glyph_width': 12,
        },
        'ColorBar': {
            'background_fill_color': t['surface'],
            'major_label_text_color': t['muted'],
            'major_label_text_font': FONT,
            'major_label_text_font_size': '10px',
            'title_text_color': t['ink2'],
            'title_text_font': FONT,
            'title_text_font_size': '11px',
            'title_text_font_style': 'normal',
            'major_tick_line_color': t['axis'],
            'bar_line_color': None,
        },
    }})


def apply_page_style(doc, t):
    """Style the plane behind the plots.

    Bokeh prepends `{% extends base %}` to a string template itself, so this
    must not carry its own — a second one raises "extended multiple times".
    """
    doc.template = ("""{% block preamble %}
<style>
  html, body {
    background: """ + t['page'] + """;
    margin: 0;
    padding: 0 0 40px 0;
    font-family: """ + FONT + """;
  }
</style>
{% endblock %}
""")


def header_html(t, run, z, step, n_steps, extra='', unit='m'):
    """Header block: run name, then z as the hero figure with a step counter.

    The z readout uses tabular figures. The reference calls for proportional
    figures on a hero number, but this one is rewritten in place on every
    slider move — proportional digits make it visibly jitter as the width
    of the string changes, so the columns rule applies here instead.
    """
    return f"""
<div style="font-family:{FONT}; padding:2px 0 0 0;">
  <div style="font-size:11px; letter-spacing:.09em; text-transform:uppercase;
              color:{t['muted']};">{run}</div>
  <div style="display:flex; align-items:baseline; gap:14px; margin-top:2px;">
    <span style="font-size:30px; font-weight:600; color:{t['ink']};
                 font-variant-numeric:tabular-nums; line-height:1.1;">
      {z:.4f}<span style="font-size:15px; font-weight:400;
                          color:{t['ink2']}; margin-left:3px;">{unit}</span></span>
    <span style="font-size:12px; color:{t['ink2']};">
      step <b style="font-variant-numeric:tabular-nums;">{step}</b>
      <span style="color:{t['muted']};">of {n_steps}</span>{extra}</span>
  </div>
</div>"""


def style_legend(fig, t):
    """Legend chrome the theme cannot reach (it is created lazily by glyphs)."""
    if not fig.legend:
        return
    lg = fig.legend[0]
    lg.orientation = 'horizontal'
    lg.spacing = 12
    lg.padding = 4
    lg.margin = 2
    lg.label_text_font_size = '11px'


# ── matplotlib (viewField3D.py) ──────────────────────────────────────────────

def apply_matplotlib_style(t):
    """Same chrome for the matplotlib viewer, so the family stays consistent."""
    import matplotlib as mpl
    mpl.rcParams.update({
        'figure.facecolor':  t['page'],
        'axes.facecolor':    t['surface'],
        'axes.edgecolor':    t['axis'],
        'axes.labelcolor':   t['ink2'],
        'axes.titlecolor':   t['ink'],
        'axes.titlesize':    11,
        'axes.titleweight':  'bold',
        'axes.labelsize':    10,
        'axes.linewidth':    0.8,
        'axes.grid':         True,
        'grid.color':        t['grid'],
        'grid.linewidth':    0.8,
        'grid.linestyle':    '-',
        'xtick.color':       t['muted'],
        'ytick.color':       t['muted'],
        'xtick.labelsize':   9,
        'ytick.labelsize':   9,
        'text.color':        t['ink'],
        'font.size':         10,
        'lines.linewidth':   2.0,
        'lines.solid_capstyle':  'round',
        'lines.solid_joinstyle': 'round',
    })
