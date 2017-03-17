# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell & Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

"""
Plots the radiated energy as a function of distance through the undulator.
Plotted on a log scale.
The script reads in the grouped power and energy data file produced by 
the powPrep.py script, so this must be run before calling this script.
"""

import sys
import getDBNames
import tables
import visitLoc


localVisItDir, localPythonPackageDir = visitLoc.visitLoc()
sys.path.insert(0,localPythonPackageDir)

import visit

def plotEnergy(localPowerAllDB):
  # Get upper and lower limits for energy plots
  h5in=tables.open_file(localPowerAllDB,'r')
  minZ = h5in.root.zSeries._v_attrs.vsLowerBounds
  maxZ = h5in.root.zSeries._v_attrs.vsUpperBounds
  h5in.close()
  
  data=visit.OpenDatabase(localPowerAllDB,0,'Vs')
  visit.AddPlot('Curve','Energy',1,1) # For log scale equivalent plot
  ViewCurveAtts = visit.ViewCurveAttributes()
  ViewCurveAtts.domainScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  ViewCurveAtts.rangeScale = ViewCurveAtts.LOG  # LINEAR, LOG
  visit.SetViewCurve(ViewCurveAtts) 
  visit.DrawPlots()

  ViewCurveAtts.domainCoords = (minZ, maxZ)
  ViewCurveAtts.rangeCoords = (-12, 2)
  ViewCurveAtts.viewportCoords = (0.2, 0.95, 0.15, 0.95)
  visit.SetViewCurve(ViewCurveAtts) 
  visit.DrawPlots()
  
  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.axes2D.yAxis.grid = 1
  AnnotationAtts.axes2D.xAxis.grid = 1
  AnnotationAtts.axes2D.xAxis.title.userTitle = 1
  AnnotationAtts.axes2D.xAxis.title.userUnits = 1
  AnnotationAtts.axes2D.xAxis.title.title = "z"
  AnnotationAtts.axes2D.xAxis.title.units = "m"
  AnnotationAtts.axes2D.yAxis.title.userTitle = 1
  AnnotationAtts.axes2D.yAxis.title.userUnits = 1
  AnnotationAtts.axes2D.yAxis.title.title = "Energy"
  AnnotationAtts.axes2D.yAxis.title.units = "J"
  AnnotationAtts.userInfoFlag = 0
  AnnotationAtts.databaseInfoFlag = 0
  AnnotationAtts.legendInfoFlag = 0
  visit.SetAnnotationAttributes(AnnotationAtts)
  visit.DrawPlots()
  CurveAtts = visit.CurveAttributes()
  CurveAtts.showLines = 1
  CurveAtts.lineStyle = CurveAtts.SOLID  # SOLID, DASH, DOT, DOTDASH
  CurveAtts.lineWidth = 2
  CurveAtts.curveColorSource = CurveAtts.Custom  # Cycle, Custom
  CurveAtts.curveColor = (51, 153, 102, 255)
  CurveAtts.showLegend = 0
  CurveAtts.showLabels = 0
  visit.SetPlotOptions(CurveAtts)



if __name__ == '__main__':
    pBaseName=sys.argv[1]
    eDB, iDB, localPowerAllDB = getDBNames.getDBNames(pBaseName)
    visit.Launch(vdir=localVisItDir)
    plotEnergy(localPowerAllDB)
    visit.OpenGUI()
    
    