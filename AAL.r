# Program: AAL.r
#          Create AAL Coordinate
# 
#          In slice Type, you can choose "Saggital", "Coronal", "Horizontal".
#          You can set higher or lower RadiusLimit in Line 31, to make too small region don't show
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141028 skylikewater - first release
#

# Test :
# AAL(23.5, "Horizontal")
# AAL(23.5, "Sagittal")
# AAL(23.5, "Coronal")
# source("D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\AAL.r")

AAL <- function(SliceNum, SliceType) {
  # check SliceType
  if ((SliceType == "Sagittal")|(SliceType == "Coronal")|(SliceType == "Horizontal")) {
  } else {
    stop('Your SliceType should be Sagittal, Coronal, or Horizontal !!!')
  }

  require(ggplot2)

  Dim = c(181, 217, 181) # from AAL in MRIcron
  DimLimit = matrix(c(90, 91, 109, -90, -125, -71), nrow = 2, ncol = 3, byrow = TRUE)
  RadiusLimit = 5
  AxisName = c("Left-Right", "Anterior-Posterior", "Superior-Inferior")
  Zoom = 0.7
  # Colour-blind friendly
  # Cerebellum, Frontal, Insula, Limbic, Occipital, Parietal, Subcortical, Temporal
  ColorPalette = c("#999999", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#CC79A7", "#0072B2", "#56B4E9")

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
    stop('Your SliceNum is out of range !!!')
  }
  
  Path = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\"
  AALROIPath = paste(Path, "AALROI.RData", sep = "")
  load(AALROIPath)
  rm(AALROIPath)
  
  ROIName = as.character(ROI[, 1])
  X = matrix(0, dim(ROI)[1], 1)
  Y = matrix(0, dim(ROI)[1], 1)
  Radius = matrix(0, dim(ROI)[1], 1)
  ROIRegion = as.character(ROI[, 6])
  Remove = matrix(0, dim(ROI)[1], 1)

  XYZ = ROI[, 3:5]
  Depth = as.numeric(as.character(XYZ[, DimNum]))
  XY = XYZ[, -DimNum]
  X = as.numeric(as.character(XY[, 1]))
  Y = as.numeric(as.character(XY[, 2]))
  rm(XY, XYZ)
  Radius = as.numeric(as.character(ROI[, 2]))
  Radius = (Radius^2 - (Depth - matrix(SliceNum, nrow = dim(ROI)[1], ncol = 1, byrow = TRUE))^2)^(1/2)
  for (ROINow in 1:dim(ROI)[1]) {
	if ((is.nan(Radius[ROINow]))||(Radius[ROINow] <= RadiusLimit)) {
	  Remove[ROINow] = 0
	} else {
	  Remove[ROINow] = 1
	}
  }
  rm(ROI)
  
  # Remove would not show
  VarName = c("ROIName", "X", "Y", "Radius", "ROIRegion", "Depth")
  for (VarNum in 1:length(VarName)) {
    RemoveStr = paste(VarName[VarNum], " = ", VarName[VarNum], "[Remove == 1]", sep = "")
    eval(parse(text = RemoveStr))
  }
  
  # Simplify ROI name
  if (SliceType == "Sagittal") {
    ROINameSim = matrix(0, length(ROIName), 1)
  }
  for (ROINow in 1:length(ROIName)) {
    Last2 = substr(ROIName[ROINow], start = (nchar(ROIName[ROINow]) - 1), stop = nchar(ROIName[ROINow]))
    if (SliceType == "Sagittal") {
	  if ((Last2 %in% "_L")||(Last2 %in% "_R")) {
	    ROINameSim[ROINow] = substr(ROIName[ROINow], start = 1, stop = (nchar(ROIName[ROINow]) - 2))
	  } else {
	    ROINameSim[ROINow] = ROIName[ROINow]
	  }
	} else {
	  if ((Last2 %in% "_L")||(Last2 %in% "_R")) {
        ROIName[ROINow] = substr(ROIName[ROINow], start = 1, stop = (nchar(ROIName[ROINow]) - 2))
	  }
    }
  }
  if (SliceType == "Sagittal") {
    for (ROINow in 1:length(ROIName)) {
	  if (any(ROINameSim[ROINow, 1] == ROINameSim[-ROINow, 1])) {
		ROINameSim[which(ROINameSim == ROINameSim[ROINow, 1])[which(ROINameSim == ROINameSim[ROINow, 1]) != ROINow]] = ROIName[which(ROINameSim == ROINameSim[ROINow, 1])[which(ROINameSim == ROINameSim[ROINow, 1]) != ROINow]]
	    ROINameSim[ROINow] = ROIName[ROINow]
	  }
	}
	ROIName = ROINameSim
	rm(ROINameSim)
  }
  
  Radius80Per = as.numeric(as.character(Radius))*0.8
  # Color
  Color = c(1, 2, 3, 4, 5, 6, 7, 8)
  # Cerebellum, Frontal, Insula, Limbic, Occipital, Parietal, Subcortical, Temporal
  if (!any(ROIRegion == "Temporal")) {
    Color = Color[-8]
  }
  if (!any(ROIRegion == "Subcortical")) {
    Color = Color[-7]
  }
  if (!any(ROIRegion == "Parietal")) {
    Color = Color[-6]
  }
  if (!any(ROIRegion == "Occipital")) {
    Color = Color[-5]
  } 
  if (!any(ROIRegion == "Limbic")) {
    Color = Color[-4]
  }
  if (!any(ROIRegion == "Insula")) {
    Color = Color[-3]
  }
  if (!any(ROIRegion == "Frontal")) {
    Color = Color[-2]
  }
  if (!any(ROIRegion == "Cerebellum")) {
    Color = Color[-1]
  }
  ColorPalette = ColorPalette[Color]
  GraphDF = data.frame(ROIName = ROIName, X = X, Y = Y, Radius = Radius, Radius80Per = Radius80Per, ROIRegion = ROIRegion, Depth = Depth)
  GraphDF = GraphDF[order(Depth), ] # Sort by Depth
  rm(ROIName, X, Y, Radius, Radius80Per, ROIRegion, Depth)
  
  Graph = ggplot(GraphDF, aes(x = X, y = Y, label = ROIName))
  Graph = Graph + geom_point(aes(colour = ROIRegion, size = Radius))
  Graph = Graph + geom_point(size = GraphDF$Radius80Per, colour = "white")
  Graph = Graph + scale_size_continuous(guide = FALSE, range = c(min(GraphDF$Radius), max(GraphDF$Radius)))
  Graph = Graph + geom_text(size = 3, fontface = "bold")
  Guides = guide_legend(title = "Lobe",
					    keywidth = 3, keyheight = 3,
					    title.position = "top",
					    title.theme = element_text(size = 20, angle = 0),
					    label.theme = element_text(size = 15, angle = 0),
					    override.aes = list(size = 7.5))
  Graph = Graph + guides(colour = Guides)
  Graph = Graph + scale_x_continuous(limits = c(ProjectDim[2,1], ProjectDim[1,1])) + scale_y_continuous(limits = c(ProjectDim[2,2], ProjectDim[1,2]))
  Graph = Graph + scale_colour_manual(values = ColorPalette, 
                                      breaks = c("Frontal", "Temporal", "Parietal", "Occipital", "Insula", "Limbic", "Subcortical", "Cerebellum"))
  SliceStr = paste(SliceType, " slice at ", SliceNum, " mm", sep = "")
  Graph = Graph + ggtitle(SliceStr)
  Graph = Graph + xlab(AxisName[1]) + ylab(AxisName[2])
  if (SliceType == "Sagittal") {
    if (SliceNum == 0) {
	  SideStr = "Mid"
	} else if (SliceNum > 0) {
	  SideStr = "R"
	} else if (SliceNum < 0) {
	  SideStr = "L"
	}
    Graph = Graph + annotate("text", x = 0.8*ProjectDim[1, 1], y = 0.8*ProjectDim[1, 2], label = SideStr, face = "bold", size = 20)
  } else {
    Graph = Graph + annotate("text", x = 0.8*ProjectDim[2, 1], y = 0.8*ProjectDim[1, 2], label = "L", face = "bold", size = 20)
	Graph = Graph + annotate("text", x = 0.8*ProjectDim[1, 1], y = 0.8*ProjectDim[1, 2], label = "R", face = "bold", size = 20)
  }
  Graph = Graph + theme_bw() # should blank first than theme
  Graph = Graph + theme(plot.title = element_text(face = "bold", size = 20, hjust = 0, vjust = 0),
                        axis.title.x = element_text(face = "bold", size = 20),
                        axis.title.y = element_text(face = "bold", size = 20))
  Dim = Dim[-DimNum]
  Graph = Graph + coord_fixed(Dim[1]/Dim[2]) # aspect ratio
  #Graph
  
  OutputPath = "D:\\Dropbox\\JTWorkspace\\Data\\NTU112\\"
  if ((SliceNum %% 1) == 0) {
    SliceStr = paste(as.character(abs(SliceNum)), ".0", sep = "")
  } else {
    SliceStr = as.character(round(abs(SliceNum), 1))
  }
  if (SliceNum < -10) {
    SliceStr = paste("-", SliceStr, sep = "")
  } else if (SliceNum < 0) {
    SliceStr = paste("-0", SliceStr, sep = "")
  } else if (SliceNum < 10) {
    SliceStr = paste("0", SliceStr, sep = "")
  }
  if (SliceNum == 0) {
    SliceStr = paste("Mid", SliceStr, sep = "")
  } else if (SliceNum > 0) {
	SliceStr = paste("Pos", SliceStr, sep = "")
  } else if (SliceNum < 0) {
	SliceStr = paste("Neg", SliceStr, sep = "")
  }
  OutputGraphPath = paste(OutputPath, SliceType, SliceStr, "mm.png", sep = "")
  ggsave(filename = OutputGraphPath, width = 10, height = 10, dpi = 300)
}