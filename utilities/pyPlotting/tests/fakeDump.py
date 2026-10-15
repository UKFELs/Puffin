# Copyright (c) 2012-2026, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""Stand-ins for an opened Puffin field dump, and a small check harness.

The polarisation helpers read very little of a dump: the aperp array, the
nFieldComp and kz2Carrier attributes of /runInfo, and - in
plotPolarizationHilbert.py - the z2 bounds and cell count. The classes here
supply that much from arrays held in memory, so a test can hand the helpers a
field whose polarisation is known analytically without writing an HDF5 file or
running the solver at all.

The layout built here is the one the plotting scripts expect,
(nx, ny, nz2, 2*nFieldComp), with the component axis last. Note that this is
*not* the layout Puffin writes: the dumps are Fortran-major,
(2*nFieldComp, nz2, ny, nx), and have to go through ReorderFieldfast.py
first. Run against a raw dump these scripts do not fail, they silently read
the component axis as x.
"""

import importlib
import os
import sys
import types

import numpy


def stubPlottingImports():
    """Satisfy the plotting scripts' module-scope imports that we do not use.

    Both scripts import pytables and matplotlib to write and draw their
    output, neither of which these tests exercise, so stub them and let the
    tests run wherever numpy is installed. scipy is deliberately left alone:
    the Hilbert test calls the real transform as its reference.
    """
    tables = types.ModuleType('tables')
    tables.open_file = lambda *a, **kw: None
    tables.copy_file = lambda *a, **kw: None
    sys.modules['tables'] = tables

    matplotlib = types.ModuleType('matplotlib')
    pyplot = types.ModuleType('matplotlib.pyplot')
    gridspec = types.ModuleType('matplotlib.gridspec')
    matplotlib.pyplot, matplotlib.gridspec = pyplot, gridspec
    sys.modules['matplotlib'] = matplotlib
    sys.modules['matplotlib.pyplot'] = pyplot
    sys.modules['matplotlib.gridspec'] = gridspec


def importPlotting(moduleName):
    """Import one of the plotting scripts for its helper functions.

    These are scripts rather than modules: each ends in a command-line block
    that runs on import. Given no arguments that block takes its usage
    branch, which is harmless but prints, so import it with argv trimmed and
    stdout swallowed, and put both back afterwards.
    """
    sys.path.insert(0, os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))
    argv, stdout = sys.argv, sys.stdout
    sys.argv = [moduleName + '.py']
    sys.stdout = open(os.devnull, 'w')
    try:
        return importlib.import_module(moduleName)
    finally:
        sys.stdout.close()
        sys.argv, sys.stdout = argv, stdout


class Attrs(object):
    """A node's attribute set, as pytables' _v_attrs presents it."""

    def __init__(self, **kw):
        for key, value in kw.items():
            setattr(self, key, value)


class Node(object):
    """A node of the file: an array, an attribute set, or both."""

    def __init__(self, data=None, attrs=None):
        self._data = data
        self._v_attrs = attrs

    def __getitem__(self, index):
        return self._data[index]

    @property
    def shape(self):
        return self._data.shape

    def read(self):
        return self._data


class Dump(object):
    """An opened dump, holding as much of the tree as the helpers read.

    /runInfo carries only the attributes passed, so leaving one out tests the
    fallback for dumps written before it existed, and passing neither leaves
    the node out altogether. The z2 mesh is only built when asked for, since
    only the Hilbert script reads it.
    """

    def __init__(self, aperp, nFieldComp=None, kz2Carrier=None, z2=None):
        class Root(object):
            pass

        self.root = Root()
        self.root.aperp = Node(numpy.asarray(aperp))

        runInfo = {}
        if nFieldComp is not None:
            runInfo['nFieldComp'] = nFieldComp
        if kz2Carrier is not None:
            runInfo['kz2Carrier'] = kz2Carrier
        if runInfo:
            self.root.runInfo = Node(attrs=Attrs(**runInfo))

        if z2 is not None:
            z2 = numpy.asarray(z2)
            self.root.globalLimits = Node(
                attrs=Attrs(vsLowerBounds=[0., 0., z2[0]],
                            vsUpperBounds=[0., 0., z2[-1]]))
            self.root.meshScaled = Node(
                attrs=Attrs(vsNumCells=[1, 1, z2.size - 1]))


def envelopeDump(Ax, Ay=None, kz2Carrier=-100., z2=None):
    """One transverse point of an averaged dump, from complex envelopes.

    Components are laid out (Re Ax, Im Ax, Re Ay, Im Ay). With Ay omitted the
    dump carries the single envelope a planar run writes (nFieldComp = 1),
    which has no polarisation of its own. The default carrier is the one
    rho = 0.005 gives, kz2Carrier = -1/(2 rho).
    """
    Ax = numpy.asarray(Ax, dtype=complex)
    nFieldComp = 1 if Ay is None else 2
    aperp = numpy.zeros((1, 1, Ax.size, 2 * nFieldComp))
    aperp[0, 0, :, 0] = Ax.real
    aperp[0, 0, :, 1] = Ax.imag
    if Ay is not None:
        Ay = numpy.asarray(Ay, dtype=complex)
        aperp[0, 0, :, 2] = Ay.real
        aperp[0, 0, :, 3] = Ay.imag
    return Dump(aperp, nFieldComp=nFieldComp, kz2Carrier=kz2Carrier, z2=z2)


def resolvedDump(Ax, negAy=None, z2=None):
    """One transverse point of an unaveraged dump.

    There one complex pair holds both polarisations, its two halves being
    (A_x, -A_y) - hence the second argument's name. nFieldComp is 1 and
    kz2Carrier is exactly zero, which is how an unaveraged dump says so.
    """
    Ax = numpy.asarray(Ax, dtype=float)
    aperp = numpy.zeros((1, 1, Ax.size, 2))
    aperp[0, 0, :, 0] = Ax
    if negAy is not None:
        aperp[0, 0, :, 1] = numpy.asarray(negAy, dtype=float)
    return Dump(aperp, nFieldComp=1, kz2Carrier=0., z2=z2)


class Checker(object):
    """Runs named numerical checks and reports them all in one pass.

    Stopping at the first failure would hide how many of the recovered
    quantities are wrong, and that count is most of what tells a sign error
    apart from a layout error.
    """

    def __init__(self):
        self.failed = []

    def check(self, name, got, want, tol=1e-12):
        worst = numpy.max(numpy.abs(numpy.asarray(got) - want))
        passed = worst <= tol
        if not passed:
            self.failed.append(name)
        print('  %s %-50s max dev %.3e  (tol %.0e)'
              % ('ok  ' if passed else 'FAIL', name, worst, tol))
        return passed

    def checkRaises(self, name, exception, call, *args, **kw):
        try:
            call(*args, **kw)
        except exception:
            print('  ok   %s' % name)
            return True
        self.failed.append(name)
        print('  FAIL %s: it was accepted instead' % name)
        return False

    def report(self):
        """Prints the verdict and returns a process exit status."""
        print()
        if self.failed:
            print('FAILED: ' + ', '.join(self.failed))
            return 1
        print('all checks passed')
        return 0
