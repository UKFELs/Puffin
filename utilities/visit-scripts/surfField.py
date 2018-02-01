# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell & Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

"""
This script creates a plot of the transverse intensity profile from the Puffin
field data files.
"""

import sys
import getDBNames
import visitLoc

localVisItDir, localPythonPackageDir = visitLoc.visitLoc()
sys.path.insert(0,localPythonPackageDir)

import visit


def surfIntens(fDB):
  visit.OpenDatabase(fDB, 0)
  visit.AddPlot("Pseudocolor", "operators/DataBinning/2D/meshScaled", 1, 1)
  DataBinningAtts = visit.DataBinningAttributes()
  DataBinningAtts.numDimensions = DataBinningAtts.Two  # One, Two, Three
  DataBinningAtts.dim1BinBasedOn = DataBinningAtts.X  # X, Y, Z, Variable
  DataBinningAtts.dim1Var = "default"
  DataBinningAtts.dim1SpecifyRange = 0
  DataBinningAtts.dim1MinRange = 0
  DataBinningAtts.dim1MaxRange = 1
  DataBinningAtts.dim1NumBins = 85
  DataBinningAtts.dim2BinBasedOn = DataBinningAtts.Y  # X, Y, Z, Variable
  DataBinningAtts.dim2Var = "default"
  DataBinningAtts.dim2SpecifyRange = 0
  DataBinningAtts.dim2MinRange = 0
  DataBinningAtts.dim2MaxRange = 1
  DataBinningAtts.dim2NumBins = 85
  DataBinningAtts.dim3BinBasedOn = DataBinningAtts.Variable  # X, Y, Z, Variable
  DataBinningAtts.dim3Var = "default"
  DataBinningAtts.dim3SpecifyRange = 0
  DataBinningAtts.dim3MinRange = 0
  DataBinningAtts.dim3MaxRange = 1
  DataBinningAtts.dim3NumBins = 50
  DataBinningAtts.outOfBoundsBehavior = DataBinningAtts.Clamp  # Clamp, Discard
  DataBinningAtts.reductionOperator = DataBinningAtts.Average  # Average, Minimum, Maximum, StandardDeviation, Variance, Sum, Count, RMS, PDF
  DataBinningAtts.varForReduction = "intensityScaled"
  DataBinningAtts.emptyVal = 0
  DataBinningAtts.outputType = DataBinningAtts.OutputOnBins  # OutputOnBins, OutputOnInputMesh
  DataBinningAtts.removeEmptyValFromCurve = 1
  visit.SetOperatorOptions(DataBinningAtts, 1)
  View2DAtts = visit.View2DAttributes()
  View2DAtts.fullFrameActivationMode = View2DAtts.On
#  View2DAtts.fullFrameAutoThreshold = 100
  visit.SetView2D(View2DAtts)
  visit.ResetView()
  PseudocolorAtts = visit.PseudocolorAttributes()
  PseudocolorAtts.centering = PseudocolorAtts.Nodal  # Natural, Nodal, Zonal
  visit.SetPlotOptions(PseudocolorAtts)
  visit.DrawPlots()




if __name__ == '__main__':
    pBaseName=sys.argv[1]
    eDB, fDB, iDB, localPowerAllDB = getDBNames.getDBNames(pBaseName)
    visit.Launch(vdir=localVisItDir)
    surfIntens(fDB)
    visit.OpenGUI()