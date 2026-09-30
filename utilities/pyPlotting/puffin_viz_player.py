#!/usr/bin/env python3
"""Playback controls for the Bokeh field viewers.

Drives an existing Slider on a timer so a run animates instead of being
scrubbed by hand. Two groups of controls:

    |◄  ► Play  ►|        transport — step back, play/pause, step forward
    −  4 fps  +           speed

The arrows belong to frame stepping rather than to speed: ◄◄/►► read as
rewind and fast-forward, which is what a reader would expect them to do to
the *position*, not to the rate. Speed gets −/+ beside its readout, where
there is nothing to misread.

Kept separate from puffin_viz_theme (which is only about how things look)
so both viewers share one implementation of the behaviour.
"""

from bokeh.models import Button, Div, InlineStyleSheet
from bokeh.layouts import row

import puffin_viz_theme as pvt

# Frames per second. The server side of a step is cheap (~16 ms for the 1D
# viewer, ~1 ms for 3D); what actually limits the top end is pushing a
# ~10^5-point phase-space scatter over the websocket and drawing it, so the
# ladder stops at 16 and the default sits mid-range.
SPEEDS = [0.5, 1.0, 2.0, 4.0, 8.0, 16.0]
DEFAULT_SPEED = 3          # → 4 fps


def _btn_css(t, accent=False):
    """Button chrome from the theme tokens, so dark mode carries too."""
    bg     = t['field'] if accent else t['surface']
    fg     = '#ffffff'   if accent else t['ink2']
    border = t['field'] if accent else t['axis']
    hover  = t['field'] if accent else t['grid']
    return InlineStyleSheet(css=f"""
      .bk-btn {{
        background: {bg};
        color: {fg};
        border: 1px solid {border};
        border-radius: 6px;
        font-family: {pvt.FONT};
        font-size: 12px;
        font-weight: 500;
        padding: 5px 12px;
        transition: background 120ms ease, color 120ms ease;
      }}
      .bk-btn:hover {{ background: {hover}; color: {t['ink'] if not accent else '#ffffff'}; }}
      .bk-btn:disabled {{ opacity: .4; }}
    """)


class Player:
    """Timer-driven playback bound to `slider`.

    Looping is deliberate: for a demo you want the gain curve to run round
    again rather than stopping dead on the last dump.
    """

    def __init__(self, doc, slider, n_steps, t):
        self.doc, self.slider, self.n, self.t = doc, slider, n_steps, t
        self.i_speed = DEFAULT_SPEED
        self.cb = None
        self._programmatic = False

        self._css_idle   = _btn_css(t, accent=False)
        self._css_accent = _btn_css(t, accent=True)

        self.b_back = Button(label='|◄', width=46, stylesheets=[self._css_idle])
        self.b_play = Button(label='► Play', width=92,
                             stylesheets=[self._css_accent])
        self.b_fwd  = Button(label='►|', width=46, stylesheets=[self._css_idle])
        self.b_slow = Button(label='−', width=40, stylesheets=[self._css_idle])
        self.b_fast = Button(label='+', width=40, stylesheets=[self._css_idle])
        self.readout = Div(text=self._speed_html(), width=64,
                           margin=(9, 0, 0, 0))

        self.b_back.on_click(lambda: self._step(-1))
        self.b_play.on_click(self.toggle)
        self.b_fwd.on_click(lambda: self._step(+1))
        self.b_slow.on_click(lambda: self._nudge(-1))
        self.b_fast.on_click(lambda: self._nudge(+1))

        # Grabbing the slider by hand should take over from the timer rather
        # than fight it. Programmatic ticks set a flag so they don't self-pause.
        slider.on_change('value', self._on_slider)

        self._live = n_steps >= 2           # a single dump has nothing to animate
        for b in (self.b_play, self.b_back, self.b_fwd):
            b.disabled = not self._live
        self._sync_speed_buttons()

    # ── widgets ──────────────────────────────────────────────────────────
    def layout(self, inset=0, top=0, bottom=6):
        # Transport group, then a gap, then the speed group — so the two
        # jobs read as two clusters rather than one undifferentiated strip.
        self.b_slow.margin = (0, 0, 0, 26)
        return row(self.b_back, self.b_play, self.b_fwd,
                   self.b_slow, self.readout, self.b_fast,
                   spacing=6, margin=(top, 10, bottom, inset))

    def _speed_html(self):
        fps = SPEEDS[self.i_speed]
        txt = f'{fps:g} fps'
        return (f'<span style="font-family:{pvt.FONT}; font-size:12px; '
                f'color:{self.t["ink2"]}; font-variant-numeric:tabular-nums;">'
                f'{txt}</span>')

    def _sync_speed_buttons(self):
        # Must respect _live too: this runs after the initial disable, so
        # without the guard it would re-enable the speed buttons on a
        # single-dump run where there is nothing to play.
        self.b_slow.disabled = not self._live or self.i_speed == 0
        self.b_fast.disabled = not self._live or self.i_speed == len(SPEEDS) - 1

    # ── playback ─────────────────────────────────────────────────────────
    def _period_ms(self):
        return 1000.0 / SPEEDS[self.i_speed]

    def _tick(self):
        nxt = self.slider.value + 1
        self._programmatic = True
        try:
            self.slider.value = 0 if nxt >= self.n else nxt   # loop
        finally:
            self._programmatic = False

    def _step(self, d):
        """Move one frame, wrapping at either end.

        Deliberately not flagged as programmatic: stepping by hand is manual
        interaction, so it drops out of playback via _on_slider, exactly as
        dragging the slider does.
        """
        if not self._live:
            return
        self.slider.value = (self.slider.value + d) % self.n

    def _on_slider(self, attr, old, new):
        if self.playing and not self._programmatic:
            self.stop()

    @property
    def playing(self):
        return self.cb is not None

    def start(self):
        if self.playing or self.n < 2:
            return
        # Parked on the last frame: restart from the top rather than
        # immediately wrapping.
        if self.slider.value >= self.n - 1:
            self._programmatic = True
            try:
                self.slider.value = 0
            finally:
                self._programmatic = False
        self.cb = self.doc.add_periodic_callback(self._tick, self._period_ms())
        self.b_play.label = '❚❚ Pause'
        self.b_play.stylesheets = [self._css_idle]

    def stop(self):
        if not self.playing:
            return
        self.doc.remove_periodic_callback(self.cb)
        self.cb = None
        self.b_play.label = '► Play'
        self.b_play.stylesheets = [self._css_accent]

    def toggle(self):
        self.stop() if self.playing else self.start()

    def _nudge(self, d):
        i = min(max(self.i_speed + d, 0), len(SPEEDS) - 1)
        if i == self.i_speed:
            return
        self.i_speed = i
        self.readout.text = self._speed_html()
        self._sync_speed_buttons()
        if self.playing:            # re-arm the timer at the new period
            self.doc.remove_periodic_callback(self.cb)
            self.cb = self.doc.add_periodic_callback(self._tick,
                                                     self._period_ms())
