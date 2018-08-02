### R code from vignette source 'Notes_on_RNMGraphics.Rnw'

###################################################
### code chunk number 1: Notes_on_RNMGraphics.Rnw:29-30
###################################################
options(width=70)


###################################################
### code chunk number 2: Notes_on_RNMGraphics.Rnw:38-39
###################################################
require(RNMGraphics)


###################################################
### code chunk number 3: Notes_on_RNMGraphics.Rnw:67-68
###################################################
nmBarChart(mtcars, xVars = "cyl", yVars = "gear", bVars = "vs")


###################################################
### code chunk number 4: Notes_on_RNMGraphics.Rnw:79-85
###################################################
run1 <- importNm("TestData1.ctl", path = 
  system.file("unittests", "testdata", "TestRun", 
  package = "RNMGraphics") )
class(run1)
timeEventSPlot(run1, title = "Time/event", xLab = "Time", 
  yLab = "Concentration", subjectNum = 2:4)


