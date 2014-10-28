# Program: AAL.r
#          Create AAL Coordinate
# 
#          In slice Type, you can choose "Saggital", "Coronal", "Horizontal".
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141028 skylikewater - first release
#

 SliceNum = 30
 SliceType = "Horizontal"

AAL <- function(SliceNum, SliceType) {
  Dim = c(181, 217, 181) # from AAL in MRIcron
  DimLimit = matrix(c(90, 91, 109, -90, -125, -71), nrow = 2, ncol = 3, byrow = TRUE)
  RadiusLimit = 5

  # check Type
  if (SliceType == "Saggital") {
    DimNum = 1
    ProjectDim = DimLimit[,2:3]
	SliceRange = DimLimit[,1]
  } else if (SliceType == "Coronal") {
    DimNum = 2
    ProjectDim = cbind(DimLimit[,1], DimLimit[,3])
    SliceRange = DimLimit[,2]
  } else if (SliceType == "Horizontal") {
    DimNum = 3
    ProjectDim = DimLimit[,1:2]
    SliceRange = DimLimit[,3]
  }
  
  # check SlickNum out of range
  if ((SliceNum > max(SliceRange))|(SliceNum < min(SliceRange))) {
    print('Your SliceNum is out of range !!!')
  }
  
  Path = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\"
  AALROIPath = paste(Path, "AALROI.RData", sep = "")
  load(AALROIPath)
  
  ROIName = ROI[, 1]
  X = matrix(0, dim(ROI)[1], 1)
  Y = matrix(0, dim(ROI)[1], 1)
  Radius = matrix(0, dim(ROI)[1], 1)
  ROIRegion = ROI[, 6]
  Remove = 0
  Remove1st = 0
  for (ROINow in 1:dim(ROI)[1]) {
    XY = as.numeric(as.character(ROI[ROINow, 3:5]))
	XY = XY[-DimNum]
    X[ROINow, 1] = XY[1]
	Y[ROINow, 1] = XY[2]
    RadiusNow = ((as.numeric(as.character(ROI[ROINow, 2]))^2) - ((as.numeric(as.character(ROI[ROINow, (DimNum + 2)])) - SliceNum)^2))^(1/2)
	Radius[ROINow, 1] = RadiusNow
	if ((is.nan(RadiusNow))||(RadiusNow <= RadiusLimit)) {
	  if (Remove1st == 0) {
	    Remove1st = 1
		Remove = as.character(ROINow)
	  } else {
	    Remove = paste(Remove, ",", as.character(ROINow), sep = "")
	  }
	}
  }
  if (Remove != 0) {
    VarName = c("ROIName", "X", "Y", "Radius", "ROIRegion")
    for (VarNum in 1:length(VarName)) {
      RemoveStr = paste(VarName[VarNum], " = ", VarName[VarNum], "[-c(", Remove, ")]", sep = "")
	  eval(parse(text = RemoveStr))
	}
  }
  
  GraphDF = data.frame(ROIName = ROIName, X = X, Y = Y, Radius = Radius, Radius80Per = Radius*0.8, ROIRegion = ROIRegion)
  
  Graph = ggplot(GraphDF, aes(X, Y, label = ROIName))
  Graph = Graph + geom_point(aes(colour = ROIRegion, size = Radius)) + scale_size_continuous(range = c(min(Radius), max(Radius)))
  Graph = Graph + geom_point(size = GraphDF$Radius80Per, colour = "white")
  Graph = Graph + geom_text()
  Graph = Graph + theme_bw()
  Graph
}