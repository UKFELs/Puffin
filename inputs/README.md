## Input Files

Please see the user manual (in $PUFFIN/doc/) for an explanation of the input 
file decks.

These subdirectories contain example input decks in the simple format (in subdirectory simple/) and using the dist input (in dist/).

### Period-averaged (SVEA) mode

`simple/1D/averaged/` and `simple/3D/averaged/` are starting points for a
period-averaged run — the field integrated as a slowly varying envelope about
the resonant carrier, so that neither the z2 mesh nor the step has to resolve
the undulator period:

```sh
mpirun -n 2 puffin averaged.in
```

The 1D one is planar and takes about 6 s on 2 ranks; the 3D one is helical,
with diffraction and a matched beam, and takes about 46 s. Both are seeded and
noise-free.

Each deck's header documents `qAveraged` and `lambdarPerCell`, what the mode
refuses at setup rather than approximate (elliptical polarisation, resuming
from a dump, reading a resolved field, a seed detuned outside the envelope
mesh's band), and — in the 3D deck — the absorbing-boundary trap, which is
sized in nodes and so is a much larger fraction of a coarse envelope mesh.

The measured accuracy of this mode, against the unaveraged solver on these
same parameters, is in `benchmark/averaged/README.md`. What is not modelled yet
is in `AVERAGED_MODE_ROADMAP.md`.
