import os
import sys
import tables

# Set local visit and visit's python package locations
# laptop:-
#localVisItDir = "/home/tml/tmp/visit/visit2_10_3.linux-x86_64"
#localPythonPackageDir = "/home/tml/tmp/visit/visit2_10_3.linux-x86_64/2.10.3/linux-x86_64/lib/site-packages" 
# desktop:-
localVisItDir = "/home/mightylorenzo/bin/visit/visit2_10_0.linux-x86_64"
localPythonPackageDir = "/home/mightylorenzo/bin/visit/visit2_10_0.linux-x86_64/2.10.0/linux-x86_64/lib/site-packages" 
sys.path.insert(0,localPythonPackageDir)
import visit

def SetupSerialLauncher():
  lp2=visit.LaunchProfile()
  lp2.SetTimeout(60)
  lp2.SetTimeLimitSet(1)
  lp2.SetTimeLimit("60")
  lp2.SetProfileName("serial")
  lp2.SetActive(1)
  lp2.SetArguments("-debug 5")
  lp2.SetLaunchMethod('serial')
  return lp2

def SetupPhase2():
  phase2=visit.MachineProfile()
  phase2.SetTunnelSSH(1)
  phase2.SetUserName('tml')
  phase2.SetDirectory('/gpfs/stfc/local/HCP084/bwm06/shared/visit2_10_0.linux-x86_64')
  phase2.SetActiveProfile(1)
  phase2.SetHost('phase2.wonder.hartree.stfc.ac.uk')
  phase2.SetClientHostDetermination(2)
  lp2=SetupSerialLauncher()
  phase2.AddLaunchProfiles(lp2)
  phase2.SetActiveProfile(0)
  ce=visit.OpenComputeEngine(phase2)
  return phase2

def SaveWindow(filename):
  SaveWindowAtts = visit.SaveWindowAttributes()
  SaveWindowAtts.outputToCurrentDirectory = 1
  SaveWindowAtts.outputDirectory = "."
  SaveWindowAtts.fileName = filename
  SaveWindowAtts.family = 0
  SaveWindowAtts.format = SaveWindowAtts.PNG  # BMP, CURVE, JPEG, OBJ, PNG, POSTSCRIPT, POVRAY, PPM, RGB, STL, TIFF, ULTRA, VTK, PLY
  SaveWindowAtts.width = 1024
  SaveWindowAtts.height = 1024
  SaveWindowAtts.screenCapture = 0
  SaveWindowAtts.saveTiled = 0
  SaveWindowAtts.quality = 80
  SaveWindowAtts.progressive = 0
  SaveWindowAtts.binary = 0
  SaveWindowAtts.stereo = 0
  SaveWindowAtts.compression = SaveWindowAtts.PackBits  # None, PackBits, Jpeg, Deflate
  SaveWindowAtts.forceMerge = 0
  SaveWindowAtts.resConstraint = SaveWindowAtts.ScreenProportions  # NoConstraint, EqualWidthHeight, ScreenProportions
  SaveWindowAtts.advancedMultiWindowSave = 0
  visit.SetSaveWindowAttributes(SaveWindowAtts)
  visit.SaveWindow()


# either load parameters from file, or enter them afresh using serialize/pickle
# LocalSettings

##localPythonPackageDir = "/home/tml/tmp/visit/visit2_10_3.linux-x86_64/2.10.3/linux-x86_64/lib/site-packages" 
runRemotely=0
##remoteTimeSeriesAstraDB='phase2.wonder.hartree.stfc.ac.uk:/gpfs/stfc/local/HCP084/bwm06/shared/testastra/working/test_analyzeBeam.h5'
##remoteTimeSeriesEleSigmaDB='phase2.wonder.hartree.stfc.ac.uk:/gpfs/stfc/local/HCP084/bwm06/shared/testele/phase100/clara.sig-page1.h5'
##remoteElePhaseSpaceDB='phase2.wonder.hartree.stfc.ac.uk:/gpfs/stfc/local/HCP084/bwm06/shared/testele/clara_track_electrons_*.vsh5 database'
#remoteElePhaseSpaceDB='phase2.wonder.hartree.stfc.ac.uk:/gpfs/stfc/local/HCP084/bwm06/shared/testele/clara_track_electrons_1.vsh5'
# currDir="/home/tml/tmp/test/test-fftw3/visit"


pBaseName=sys.argv[1]
# pBaseName = "f2main"

currDir = os.getcwd()

# Database suffixes for power, electron macroparticle and integrated data respectively

pFileSffx = "_integrated_0_all.vsh5"
eFileSffx = "_electrons_* database"
iFileSffx = "_integrated_0_* database"


# Full database paths

eDB = "localhost:" + currDir + "/" + pBaseName + eFileSffx
iDB = "localhost:" + currDir + "/" + pBaseName + iFileSffx
localPowerAllDB=currDir + "/" + pBaseName + pFileSffx


# Get upper and lower limits for energy plots
h5in=tables.open_file(localPowerAllDB,'r')
minZ = h5in.root.zSeries._v_attrs.vsLowerBounds
maxZ = h5in.root.zSeries._v_attrs.vsUpperBounds
h5in.close()



#Remote settings
def isOpenDatabase(filename):
  pass
  return 0

def PhaseSpace(x1,x2,dumpNo):
   visit.AddWindow()
   ScatAttrs = visit.ScatterAttributes()
   ScatAttrs.var1 = x1
   ScatAttrs.var2 = x2
#   ScatAttrs.var1Role = visit.ScatterAtts.Coordinate0
#   ScatAttrs.var2Role = visit.ScatterAtts.Coordinate1
#   ScatAttrs.var3Role = visit.ScatterAtts.None
#   ScatAttsr.var4Role = visit.ScatterAtts.None
   ScatAttrs.SetVar1Role(0) #coord 0
   ScatAttrs.SetVar2Role(1) #coord 1
   ScatAttrs.SetVar3Role(4) #none
   ScatAttrs.SetVar4Role(4) #none   
   ScatAttrs.scaleCube = 0
   visit.AddPlot('Scatter',x1,1,1)
   visit.SetPlotOptions(ScatAttrs)
   visit.SetTimeSliderState(dumpNo)
   visit.DrawPlots() 

def PhaseSpaceXY(elementNo=1):
  if isOpenDatabase(''):
    PhaseSpace('electrons_x','electrons_y',1)
  else:
#    psData=visit.OpenDatabase(remoteElePhaseSpaceDB,elementNo,'Vs')
    psData=visit.OpenDatabase(remoteElePhaseSpaceDB,0,'Vs')
    PhaseSpace('electrons_x','electrons_y',elementNo)

def TimeSeriesS1():
  visit.AddPlot('Curve','s1')
  visit.AddPlot('Curve','s2')
  visit.DrawPlots()
  visit.AddWindow()
  visit.AddPlot('Curve','pAverage')
  visit.DrawPlots()
  visit.SetActiveWindow(1)
  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.axes2D.yAxis.grid = 1
  AnnotationAtts.axes2D.xAxis.grid = 1
  AnnotationAtts.axes2D.xAxis.title.userTitle = 1
  AnnotationAtts.axes2D.xAxis.title.userUnits = 1
  AnnotationAtts.axes2D.xAxis.title.title = "Position along the machine s"
  AnnotationAtts.axes2D.xAxis.title.units = "m"
  visit.SetAnnotationAttributes(AnnotationAtts)
  visit.DrawPlots()

def plotEnergyLinear():
  visit.AddPlot('Curve','Energy')
  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.axes2D.yAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.grid = 0
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
  CurveAtts.curveColor = (255, 0, 0, 255)
  CurveAtts.showLabels = 0
  CurveAtts.showLegend = 0
  visit.SetPlotOptions(CurveAtts)


def plotEnergy():
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


def plotPowNorm():
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



def binPhase():
  visit.OpenDatabase(eDB, 0)
  visit.AddPlot("Pseudocolor", "operators/DataBinning/2D/electrons", 1, 1)
  DataBinningAtts = visit.DataBinningAttributes()
  DataBinningAtts.numDimensions = DataBinningAtts.Two  # One, Two, Three
  DataBinningAtts.dim1BinBasedOn = DataBinningAtts.Variable  # X, Y, Z, Variable
  DataBinningAtts.dim1Var = "electrons_z"
  DataBinningAtts.dim1SpecifyRange = 1
  DataBinningAtts.dim1MinRange = 0
  DataBinningAtts.dim1MaxRange = 31
  DataBinningAtts.dim1NumBins = 512
  DataBinningAtts.dim2BinBasedOn = DataBinningAtts.Variable  # X, Y, Z, Variable
  DataBinningAtts.dim2Var = "electrons_gamma"
  DataBinningAtts.dim2SpecifyRange = 1
  DataBinningAtts.dim2MinRange = 0.995
  DataBinningAtts.dim2MaxRange = 1.005
  DataBinningAtts.dim2NumBins = 512
  DataBinningAtts.outOfBoundsBehavior = DataBinningAtts.Clamp  # Clamp, Discard
  DataBinningAtts.reductionOperator = DataBinningAtts.Sum  # Average, Minimum, Maximum, StandardDeviation, Variance, Sum, Count, RMS, PDF
  DataBinningAtts.varForReduction = "electrons_chargeSI"
  DataBinningAtts.emptyVal = 0
  DataBinningAtts.outputType = DataBinningAtts.OutputOnBins  # OutputOnBins, OutputOnInputMesh
  DataBinningAtts.removeEmptyValFromCurve = 1
  visit.SetOperatorOptions(DataBinningAtts, 1)
  visit.SetTimeSliderState(0)
  visit.SetTimeSliderState(1)
  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.axes2D.yAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.title.userTitle = 1
  AnnotationAtts.axes2D.xAxis.title.userUnits = 1
  AnnotationAtts.axes2D.xAxis.title.title = "z2bar"
  AnnotationAtts.axes2D.xAxis.title.units = "cooperation lengths"
  AnnotationAtts.axes2D.yAxis.title.userTitle = 1
  AnnotationAtts.axes2D.yAxis.title.userUnits = 1
  AnnotationAtts.axes2D.yAxis.title.title = "gamma"
  AnnotationAtts.axes2D.yAxis.title.units = ""
  AnnotationAtts.userInfoFlag = 0
  AnnotationAtts.databaseInfoFlag = 0
  # AnnotationAtts.legendInfoFlag = 0
  visit.SetAnnotationAttributes(AnnotationAtts)
  visit.DrawPlots()




def bunching():
  #visit.OpenDatabase("localhost:/home/tml/tmp/test/build/examples/simple/1D/OptCommV165pp65-70/fig2/f2main_bunching1st_0_* database", 0)
  visit.OpenDatabase(iDB, 0)
  visit.AddPlot("Curve", "bunchingFundamental", 1, 1)
  visit.SetTimeSliderState(0)
  # Begin spontaneous state
  ViewCurveAtts = visit.ViewCurveAttributes()
  ViewCurveAtts.domainCoords = (0, 29.8745)
  ViewCurveAtts.rangeCoords = (0, 0.75)
  ViewCurveAtts.viewportCoords = (0.2, 0.95, 0.15, 0.95)
  ViewCurveAtts.domainScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  ViewCurveAtts.rangeScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  visit.SetViewCurve(ViewCurveAtts)
  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.axes2D.yAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.title.userTitle = 1
  AnnotationAtts.axes2D.xAxis.title.userUnits = 1
  AnnotationAtts.axes2D.xAxis.title.title = "z2bar"
  AnnotationAtts.axes2D.xAxis.title.units = "cooperation lengths"
  AnnotationAtts.axes2D.yAxis.title.userTitle = 1
  AnnotationAtts.axes2D.yAxis.title.userUnits = 1
  AnnotationAtts.axes2D.yAxis.title.title = "bunching"
  AnnotationAtts.axes2D.yAxis.title.units = ""
  AnnotationAtts.userInfoFlag = 0
  AnnotationAtts.databaseInfoFlag = 0
  AnnotationAtts.legendInfoFlag = 0
  visit.SetAnnotationAttributes(AnnotationAtts)
  CurveAtts = visit.CurveAttributes()
  CurveAtts.showLines = 1
  CurveAtts.lineStyle = CurveAtts.SOLID  # SOLID, DASH, DOT, DOTDASH
  CurveAtts.lineWidth = 2
  CurveAtts.curveColorSource = CurveAtts.Custom  # Cycle, Custom
  CurveAtts.curveColor = (51, 153, 102, 255)
  CurveAtts.showLegend = 0
  CurveAtts.showLabels = 0
  visit.SetPlotOptions(CurveAtts)
  visit.DrawPlots()




def current():
  visit.OpenDatabase(iDB, 0)
  visit.AddPlot("Curve", "beamCurrent", 1, 1)
  visit.DrawPlots()
  # Begin spontaneous state
  ViewCurveAtts = visit.ViewCurveAttributes()
  ViewCurveAtts.domainCoords = (-0.0240932, 29.972)
  ViewCurveAtts.rangeCoords = (0, 22000)
  ViewCurveAtts.viewportCoords = (0.2, 0.95, 0.15, 0.95)
  ViewCurveAtts.domainScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  ViewCurveAtts.rangeScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  visit.SetViewCurve(ViewCurveAtts)
  # End spontaneous state
  ViewCurveAtts = visit.ViewCurveAttributes()
  ViewCurveAtts.domainCoords = (-0.0240932, 29.972)
  ViewCurveAtts.rangeCoords = (0, 22000)
  ViewCurveAtts.viewportCoords = (0.2, 0.95, 0.15, 0.95)
  ViewCurveAtts.domainScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  ViewCurveAtts.rangeScale = ViewCurveAtts.LINEAR  # LINEAR, LOG
  visit.SetViewCurve(ViewCurveAtts)

  AnnotationAtts = visit.AnnotationAttributes()
  AnnotationAtts.axes2D.yAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.grid = 0
  AnnotationAtts.axes2D.xAxis.title.userTitle = 1
  AnnotationAtts.axes2D.xAxis.title.userUnits = 1
  AnnotationAtts.axes2D.xAxis.title.title = "z2bar"
  AnnotationAtts.axes2D.xAxis.title.units = "cooperation lengths"
  AnnotationAtts.axes2D.yAxis.title.userTitle = 1
  AnnotationAtts.axes2D.yAxis.title.userUnits = 1
  AnnotationAtts.axes2D.yAxis.title.title = "Current"
  AnnotationAtts.axes2D.yAxis.title.units = "A"
  AnnotationAtts.userInfoFlag = 0
  AnnotationAtts.databaseInfoFlag = 0
  AnnotationAtts.legendInfoFlag = 0
  visit.SetAnnotationAttributes(AnnotationAtts)
  CurveAtts = visit.CurveAttributes()
  CurveAtts.showLines = 1
  CurveAtts.lineStyle = CurveAtts.SOLID  # SOLID, DASH, DOT, DOTDASH
  CurveAtts.lineWidth = 2
  CurveAtts.curveColorSource = CurveAtts.Custom  # Cycle, Custom
  CurveAtts.curveColor = (51, 153, 102, 255)
  CurveAtts.showLegend = 0
  CurveAtts.showLabels = 0
  visit.SetPlotOptions(CurveAtts)

#  visit.SetViewAxisArray(ViewAxisArrayAtts)

  

visit.Launch(vdir=localVisItDir)

if runRemotely:
  p2=SetupPhase2()
#  data2=visit.OpenDatabase(remoteTimeSeriesAstraDB,0,'Vs')
  data2=visit.OpenDatabase(remoteTimeSeriesEleSigmaDB,0,'Vs')

else:
  data=visit.OpenDatabase(localPowerAllDB,0,'Vs')

plotEnergyLinear()

visit.AddWindow()
plotEnergy()

visit.AddWindow()
plotPowNorm()

visit.AddWindow()
binPhase()

visit.AddWindow()
bunching()

visit.AddWindow()
current()
visit.SetWindowLayout(6)
#TimeSeriesS1()
#PhaseSpaceXY(2)
#SaveWindow('phase2.png')
#PhaseSpaceXY(3)
#SaveWindow('phase3.png')
#PhaseSpaceXY(4)
#SaveWindow('phase4.png')

#visit.OpenCLI()
visit.OpenGUI()