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

# Test :
# AAL(23.5, "Horizontal")
# AAL(23.5, "Saggital")
# AAL(23.5, "Coronal")

AAL <- function(SliceNum, SliceType) {
  require(ggplot2)

  Dim = c(181, 217, 181) # from AAL in MRIcron
  DimLimit = matrix(c(90, 91, 109, -90, -125, -71), nrow = 2, ncol = 3, byrow = TRUE)
  RadiusLimit = 5
  AxisName = c("Left-Right", "Anterior-Posterior", "Superior-Inferior")
  Zoom = 0.7

  # check Type
  if (SliceType == "Sagittal") {
    DimNum = 1
    ProjectDim = DimLimit[,2:3]*Zoom 
	SliceRange = DimLimit[,1]
  } else if (SliceType == "Coronal") {
    DimNum = 2
    ProjectDim = cbind(DimLimit[,1], DimLimit[,3])*Zoom 
    SliceRange = DimLimit[,2]
  } else if (SliceType == "Horizontal") {
    DimNum = 3
    ProjectDim = DimLimit[,1:2]*Zoom 
    SliceRange = DimLimit[,3]
  }
  AxisName = AxisName[-DimNum]
  
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
  Graph = Graph + geom_point(aes(colour = ROIRegion, size = Radius))
  Graph = Graph + geom_point(size = GraphDF$Radius80Per, colour = "white")
  Graph = Graph + scale_size_continuous(guide = FALSE, range = c(min(Radius), max(Radius)))
  Graph = Graph + geom_text(size = 4, fontface="bold")
  Guides = guide_legend(title = "Lobe",
					    keywidth = 3, keyheight = 3,
					    title.position = "top",
					    title.theme = element_text(size = 20, angle = 0),
					    label.theme = element_text(size = 15, angle = 0),
					    override.aes = list(size = 7.5))
  Graph = Graph + guides(colour = Guides)
  Graph = Graph + scale_x_continuous(limits = c(ProjectDim[2,1], ProjectDim[1,1])) + scale_y_continuous(limits = c(ProjectDim[2,2], ProjectDim[1,2]))
  SliceStr = paste(SliceType, " slice at ", SliceNum, " mm", sep = "")
  Graph = Graph + ggtitle(SliceStr)
  Graph = Graph + xlab(AxisName[1]) + ylab(AxisName[2])
  Graph = Graph + theme_bw() # should blank first than theme
  Graph = Graph + theme(plot.title = element_text(face = "bold", size = 20, hjust = 0, vjust = 0),
                        axis.title.x = element_text(face = "bold", size = 20),
                        axis.title.y = element_text(face = "bold", size = 20))
  #Graph
  
  OutputPath = "D:\\Dropbox\\JTWorkspace\\Data\\NTU112\\"
  OutputGraphPath = paste(OutputPath, SliceType, as.character(SliceNum), "mm.png", sep = "")
  ggsave(filename = OutputGraphPath, dpi = 300)
}