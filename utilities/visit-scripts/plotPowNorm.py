# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell & Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

"""
Plots the radiated temporal power profile evolving as a function of distance through 
the undulator.
"""

import sys
import getDBNames
import visitLoc

localVisItDir, localPythonPackageDir = visitLoc.visitLoc()

sys.path.insert(0,localPythonPackageDir)

import visit

def plotPowNorm(localPowerAllDB):
  data=visit.OpenDatabase(localPowerAllDB,0,'Vs')
  visit.AddPlot('Pseudocolor','power_SI_Norm')  
  visit.DrawPlots()
  View2DAtts = visit.View2DAttributes()
#  View2DAtts.windowCoords = (0, 30, 0, 15.0796)
#  View2DAtts.viewportCoords = (0.2, 0.95, 0.15, 0.95)
#  View2DAtts.fullFrameAutoThreshold = 100
  View2DAtts.xScale = View2DAtts.LINEAR  # LINEAR, LOG
  View2DAtts.yScale = View2DAtts.LINEAR  # LINEAR, LOG
  View2DAtts.windowValid = 1
  View2DAtts.fullFrameActivationMode = View2DAtts.On  # On, Off, Auto
  #View2DAtts.fullFrameAutoThreshold = 100
  visit.SetView2D(View2DAtts)
  visit.ResetView()
  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.userInfoFlag = 0
  AnnotationAtts.databaseInfoFlag = 0
  AnnotationAtts.axes2D.xAxis.title.userTitle = 1
  AnnotationAtts.axes2D.xAxis.title.userUnits = 1
  AnnotationAtts.axes2D.xAxis.title.title = "ct-z"
  AnnotationAtts.axes2D.xAxis.title.units = "m"
  AnnotationAtts.axes2D.yAxis.title.userTitle = 1
  AnnotationAtts.axes2D.yAxis.title.userUnits = 1
  AnnotationAtts.axes2D.yAxis.title.title = "z"
  AnnotationAtts.axes2D.yAxis.title.units = "m"
  # AnnotationAtts.legendInfoFlag = 0
  visit.SetAnnotationAttributes(AnnotationAtts)
  
if __name__ == '__main__':
    pBaseName=sys.argv[1]
    eDB, fDB, iDB, localPowerAllDB = getDBNames.getDBNames(pBaseName)
    visit.Launch(vdir=localVisItDir)
    plotPowNorm(localPowerAllDB)
    visit.OpenGUI()
    