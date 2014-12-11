# Program: CircleProject.r
#          Project your roi on circle
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141126 skylikewater - first release
#

CircleProject <- function(Dom, DomXY, CircleCenter, NTU112_74Tracts, CircleProjectRadius, ExtendRatio = 1.2, TextExtendRatio = 1.25,
	                      Path = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\") {
  DomLevels = levels(as.factor(as.matrix(DomXY$Name[which(as.character(DomXY$MajSub) %in% "Maj")]))) # Maj only
  AllLevels = levels(as.factor(as.matrix(DomXY$Name)))
  DomLevelsNum = length(DomLevels)
  AllLevelsNum = length(AllLevels)

  # Project
  Project = matrix(0, DomLevelsNum, 3)
  for (ProjectNum in 1:DomLevelsNum) {
    NowROI = as.character(DomLevels[ProjectNum])
  	NowLevelsNum = which(CircleCenter[, 1] %in% NowROI)
  	NowAngle = atan2(as.numeric(CircleCenter[NowLevelsNum, 3]), as.numeric(CircleCenter[NowLevelsNum, 2]))
  	Project[ProjectNum, 1] = NowROI
    Project[ProjectNum, 2] = as.numeric(as.character(cos(NowAngle)*CircleProjectRadius*ExtendRatio))
    Project[ProjectNum, 3] = as.numeric(as.character(sin(NowAngle)*CircleProjectRadius*ExtendRatio))
  }
  rm(NowLevelsNum, NowROI, NowAngle, ProjectNum)

  ProjectXPosi = matrix(Project[which(Project[, 2] >= 0), ], ncol = 3)
  ProjectXNega = matrix(Project[which(Project[, 2] < 0), ], ncol = 3)
  # sort by X
  ProjectXPosi = matrix(ProjectXPosi[order(as.numeric(as.character(ProjectXPosi[, 3]))), ], ncol = 3)
  ProjectXNega = matrix(ProjectXNega[order(as.numeric(as.character(ProjectXNega[, 3])), decreasing = TRUE), ], ncol = 3)
  Project = rbind(ProjectXPosi, ProjectXNega)
  rm(ProjectXPosi, ProjectXNega)

  ProjectSlot = matrix(0, DomLevelsNum, 3)
  for (ProjectSlotNum in 1:DomLevelsNum) {
  	NowAngle = ((2*pi*ProjectSlotNum)/DomLevelsNum) + (pi/2) # add 90 degree so start from top.
    ProjectSlot[ProjectSlotNum, 2] = cos(NowAngle)*CircleProjectRadius*ExtendRatio
    ProjectSlot[ProjectSlotNum, 3] = sin(NowAngle)*CircleProjectRadius*ExtendRatio
  }
  rm(NowAngle, ProjectSlotNum)

  # find top
  MinNum = 0
  NowMin = 10000000
  source(paste(Path, "AALSlicing\\EucDist.r", sep = ""))
  SlotXY = as.numeric(ProjectSlot[1, 2:3])
  for (ProjectNum in 1:DomLevelsNum) {
    NowXY = as.numeric(Project[ProjectNum, 2:3])
	  NowDist = EucDist(SlotXY, NowXY)
	  if (NowDist < NowMin) {
	    MinNum = ProjectNum
	    NowMin = NowDist
	  }
	  rm(NowXY, NowDist)
  }
  rm(SlotXY, NowMin, ProjectNum)
  if (DomLevelsNum == 1) {
  	NameOrder = 1
  } else if (MinNum == 1) {
  	NameOrder = 1:DomLevelsNum
  } else if (MinNum == DomLevelsNum) {
  	NameOrder = c(DomLevelsNum, 1:(DomLevelsNum - 1))
  } else { # center
    NameOrder = c(MinNum:DomLevelsNum, 1:(MinNum - 1))
  }
  ProjectSlot[, 1] = as.character(Project[NameOrder, 1])
  rm(Project)

  ProjectSlotAll = matrix(0, AllLevelsNum, 3)
  for (ProjectSlotAllNum in 1:AllLevelsNum) {
    NowROI = as.character(AllLevels[ProjectSlotAllNum])
    ProjectSlotAll[ProjectSlotAllNum, 1] = NowROI
    NowMajSub = as.character(DomXY$MajSub[min(which(as.character(DomXY$Name) %in% NowROI))])
    if (NowMajSub %in% "Maj") {
      MajNum = which(as.character(ProjectSlot[, 1]) %in% NowROI)
      ProjectSlotAll[ProjectSlotAllNum, 2] = ProjectSlot[MajNum, 2]
      ProjectSlotAll[ProjectSlotAllNum, 3] = ProjectSlot[MajNum, 3]
      rm(MajNum)
    } else if (NowMajSub %in% "Sub") {
      NowDom = Dom[which(as.character(Dom[, 1]) %in% NowROI), 2]
      MajNum = which(as.character(ProjectSlot[, 1]) %in% NowDom)
      DomX = as.numeric(ProjectSlot[MajNum, 2])
      DomY = as.numeric(ProjectSlot[MajNum, 3])
      DomSubDeltaX = as.numeric(CircleCenter[which(as.character(CircleCenter[, 1]) %in% NowROI), 2]) -
                     as.numeric(CircleCenter[which(as.character(CircleCenter[, 1]) %in% NowDom), 2])
      DomSubDeltaY = as.numeric(CircleCenter[which(as.character(CircleCenter[, 1]) %in% NowROI), 3]) -
                     as.numeric(CircleCenter[which(as.character(CircleCenter[, 1]) %in% NowDom), 3])
      ProjectSlotAll[ProjectSlotAllNum, 2] = DomX + DomSubDeltaX
      ProjectSlotAll[ProjectSlotAllNum, 3] = DomY + DomSubDeltaY
      rm(NowDom, MajNum, DomX, DomY, DomSubDeltaX, DomSubDeltaY)
    }
    rm(NowROI, NowMajSub)
  }
  rm(ProjectSlotAllNum)

  # Test:
  # if(!require(ggplot2)) {
  #   install.packages("ggplot2")
  # } else {
  #   require(ggplot2)
  # }
  # GraphDF = as.data.frame(ProjectSlot)
  # GraphDF$V2 = as.numeric(as.character(GraphDF$V2))
  # GraphDF$V3 = as.numeric(as.character(GraphDF$V3))
  # Graph = ggplot(GraphDF, aes(x = V2, y = V3)) + geom_point()
  # for (ROINum in 1:DomLevelsNum) {
  #   Graph = Graph + annotate("text", x = as.numeric(GraphDF$V2[ROINum]),
  #                                    y = as.numeric(GraphDF$V3[ROINum]),
  #                                label = as.character(GraphDF$V1[ROINum]),
  #                                 face = "bold", size = 3)
  # }
  # rm(ROINum)
  # print(Graph)

  DistROI = matrix(0, AllLevelsNum, 3)
  DistROI[, 1] = as.character(AllLevels)

  # translate
  Tracts = NTU112_74Tracts
  ROIStartX = as.numeric(Tracts$StartX)
  ROIStartY = as.numeric(Tracts$StartY)
  ROIEndX = as.numeric(Tracts$EndX)
  ROIEndY = as.numeric(Tracts$EndY)
  for (TractsNum in 1:dim(Tracts)[1]) {
  	NowROIStart = as.character(Tracts[TractsNum, 2])
    NowROIStartOriX = as.numeric(ROIStartX[TractsNum])
    NowROIStartOriY = as.numeric(ROIStartY[TractsNum])
    if (as.character(DomXY$MajSub[min(which(DomXY$Name %in% NowROIStart))]) %in% "Maj") {
      NowROIStartNewX = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIStart), 2])
      NowROIStartNewY = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIStart), 3])
      MajSubDistX = 0
      MajSubDistY = 0
    } else if (as.character(DomXY$MajSub[min(which(DomXY$Name %in% NowROIStart))]) %in% "Sub") {
      NowROIMaj = as.character(Dom[which(Dom[, 1] %in% NowROIStart), 2])
      MajSubDistX = as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIStart), 2]) -
                    as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIMaj), 2])
      MajSubDistY = as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIStart), 3]) -
                    as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIMaj), 3])
      NowROIStartNewX = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIMaj), 2]) + MajSubDistX
      NowROIStartNewY = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIMaj), 3]) + MajSubDistY
      rm(NowROIMaj)
    }
    if (NowROIStartOriX != NowROIStartNewX) {
      DistROI[which(DistROI[, 1] %in% NowROIStart), 2] = NowROIStartNewX - NowROIStartOriX
    }
    if (NowROIStartOriY != NowROIStartNewY) {
      DistROI[which(DistROI[, 1] %in% NowROIStart), 3] = NowROIStartNewY - NowROIStartOriY
    }
    ROIStartX[TractsNum] = NowROIStartNewX
    ROIStartY[TractsNum] = NowROIStartNewY
    rm(NowROIStart, NowROIStartOriX, NowROIStartOriY, NowROIStartNewX, NowROIStartNewY, MajSubDistX, MajSubDistY)

  	NowROIEnd = as.character(Tracts[TractsNum, 3])
    NowROIEndOriX = as.numeric(ROIEndX[TractsNum])
    NowROIEndOriY = as.numeric(ROIEndY[TractsNum])
    if (as.character(DomXY$MajSub[min(which(DomXY$Name %in% NowROIEnd))]) %in% "Maj") {
      NowROIEndNewX = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIEnd), 2])
      NowROIEndNewY = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIEnd), 3])
      MajSubDistX = 0
      MajSubDistY = 0
    } else if (as.character(DomXY$MajSub[min(which(DomXY$Name %in% NowROIEnd))]) %in% "Sub") {
      NowROIMaj = as.character(Dom[which(Dom[, 1] %in% NowROIEnd), 2])
      MajSubDistX = as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIEnd), 2]) -
                    as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIMaj), 2])
      MajSubDistY = as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIEnd), 3]) -
                    as.numeric(CircleCenter[which(CircleCenter[, 1] %in% NowROIMaj), 3])
      NowROIEndNewX = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIMaj), 2]) + MajSubDistX
      NowROIEndNewY = as.numeric(ProjectSlot[which(ProjectSlot[, 1] %in% NowROIMaj), 3]) + MajSubDistY
      rm(NowROIMaj)
    }
    if (NowROIEndOriX != NowROIEndNewX) {
      DistROI[which(DistROI[, 1] %in% NowROIEnd), 2] = NowROIEndNewX - NowROIEndOriX
    }
    if (NowROIEndOriY != NowROIEndNewY) {
      DistROI[which(DistROI[, 1] %in% NowROIEnd), 3] = NowROIEndNewY - NowROIEndOriY
    }
    ROIEndX[TractsNum] = NowROIEndNewX
    ROIEndY[TractsNum] = NowROIEndNewY
    rm(NowROIEnd, NowROIEndOriX, NowROIEndOriY, NowROIEndNewX, NowROIEndNewY, MajSubDistX, MajSubDistY)
  }
  rm(TractsNum)
  Tracts$StartX = ROIStartX
  Tracts$StartY = ROIStartY
  Tracts$EndX = ROIEndX
  Tracts$EndY = ROIEndY
  rm(ROIStartX, ROIStartY, ROIEndX, ROIEndY)

  # Project Text labels
  ProjectText = matrix(0, AllLevelsNum, 4)
  for (ProjectNum in 1:AllLevelsNum) {
    NowROI = as.character(AllLevels[ProjectNum])
    NowSlotAllNum = which(as.character(ProjectSlotAll[, 1]) %in% NowROI)
    NowAngle = atan2(as.numeric(ProjectSlotAll[NowSlotAllNum, 3]), as.numeric(ProjectSlotAll[NowSlotAllNum, 2]))

    ProjectText[ProjectNum, 1] = NowROI
    ProjectText[ProjectNum, 2] = as.numeric(as.character(cos(NowAngle)*CircleProjectRadius*ExtendRatio*TextExtendRatio))
    ProjectText[ProjectNum, 3] = as.numeric(as.character(sin(NowAngle)*CircleProjectRadius*ExtendRatio*TextExtendRatio))
    ProjectText[ProjectNum, 4] = NowAngle
  }
  rm(NowROI, NowSlotAllNum, NowAngle, ProjectNum)

  XY = DomXY
  PathX = as.numeric(XY[, 2])
  PathY = as.numeric(XY[, 3])
  for (XYNum in 1:dim(XY)[1]) {
  	NowROI = as.character(XY[XYNum, 1])
  	NowMajSub = as.character(XY[XYNum, 4])

    PathX[XYNum] = PathX[XYNum] + as.numeric(DistROI[which(DistROI[, 1] %in% NowROI), 2])
    PathY[XYNum] = PathY[XYNum] + as.numeric(DistROI[which(DistROI[, 1] %in% NowROI), 3])
    rm(NowROI, NowMajSub)
  }
  rm(XYNum)
  XY[, 2] = PathX
  XY[, 3] = PathY
  rm(PathX, PathY)

  # Test:
  # if(!require(ggplot2)) {
  #   install.packages("ggplot2")
  # } else {
  #   require(ggplot2)
  # }
  # GraphDF = as.data.frame(XY)
  # GraphDF$X = as.numeric(as.character(GraphDF$X))
  # GraphDF$Y = as.numeric(as.character(GraphDF$Y))
  # DomLevels = as.matrix(levels(as.factor(Dom[, 2])))
  # Graph = ggplot(GraphDF, aes(x = X, y = Y))
  # for (TractsNum in 1:dim(Tracts)[1]) {
  #   if (as.numeric(Tracts$PValue[TractsNum]) < PValueLimit) {
  #     TractsColour = "red"
  #   } else {
  #     TractsColour = "grey"
  #   }
  #   Graph = Graph + annotate("segment", x = as.numeric(Tracts$StartX[TractsNum]),
  #                                       y = as.numeric(Tracts$StartY[TractsNum]),
  #                                    xend = as.numeric(Tracts$EndX[TractsNum]),
  #                                    yend = as.numeric(Tracts$EndY[TractsNum]),
  #                                    size = 3*as.numeric(Tracts$FAMean[TractsNum]),
  #                                  colour = TractsColour, alpha = 0.8)
  #  rm(TractsColour)
  # }
  # rm(TractsNum)
  # DomLevelsDepth = matrix(0, dim(DomLevels)[1], 1)
  # for (DomLevelsNum in 1:dim(DomLevels)[1]) {
  #   DomLevelsDepth[DomLevelsNum] = GraphDF$Depth[
  #                                  min(which(XY$Name %in% DomLevels[DomLevelsNum]))]
  # }
  # rm(DomLevelsNum)
  # DomLevelsDepth = sort(DomLevelsDepth, decreasing = FALSE)
  # for (DepthLevelsNum in 1:length(DomLevelsDepth)) {
  #   NowDom = DomLevels[DepthLevelsNum]
  #   DomSub = Dom[which(Dom[, 2] %in% NowDom), 1]
  #   Graph = Graph + geom_polygon(data = GraphDF[intersect(which(GraphDF$MajSub %in% "Maj"),
  #                                                         which(GraphDF$Name %in% DomSub)), ],
  #                                aes(fill = ROIRegion, alpha = 0.5))
  #   Graph = Graph + geom_path(data = GraphDF[which(GraphDF$Name %in% DomSub), ],
  #                             aes(size = MajSub, colour = "white"))
  #   rm(NowDom, DomSub)
  # }
  # rm(DomLevelsDepth, DepthLevelsNum)
  # Guides = guide_legend(title = "Lobe",
  #                       keywidth = 3, keyheight = 3,
  #                       title.position = "top",
  #                       title.theme = element_text(size = 20, angle = 0),
  #                       label.theme = element_text(size = 15, angle = 0),
  #                       override.aes = list(size = 7.5))
  # Graph = Graph + guides(colour = FALSE, fill = Guides, size = FALSE, alpha = FALSE)
  # Graph = Graph + scale_colour_manual(values = c("white"))
  # Graph = Graph + scale_size_manual(values=c(2, 1.25))
  # for (ProjectSlotNum in 1:dim(ProjectText)[1]) {
  #   CircleLabel = as.character(ProjectText[ProjectSlotNum, 1])
  #   CircleLabelY = as.numeric(ProjectText[ProjectSlotNum, 3])
  #   if (CircleLabel %in% "Pole") {
  #     CircleLabelY = CircleLabelY + 5
  #   } else if (CircleLabel %in% "SupPole") {
  #     CircleLabelY = CircleLabelY - 5
  #   }
  #   CircleLabelAngle = (as.numeric(as.character(ProjectText[ProjectSlotNum, 4]))*180)/pi
  #   if (abs(CircleLabelAngle) > 90) {
  #     if (CircleLabelAngle > 0) {
  #       CircleLabelAngle = CircleLabelAngle - 180
  #     } else if (CircleLabelAngle < 0) {
  #       CircleLabelAngle = CircleLabelAngle + 180
  #     }
  #   }
  #   Graph = Graph + annotate("text", x = as.numeric(ProjectText[ProjectSlotNum, 2]),
  #                                    y = CircleLabelY,
  #                                label = CircleLabel,
  #                                 size = 3, angle = CircleLabelAngle,
  #                                hjust = 0.5, vjust = 0.5)
  #   rm(CircleLabel, CircleLabelY, CircleLabelAngle)
  # }
  # for (TractsNum in 1:dim(Tracts)[1]) {
  #   if (as.numeric(Tracts$PValue[TractsNum]) < PValueLimit) {
  #     TractsColour = "red"
  #   } else {
  #     TractsColour = "black"
  #   }
  #   TractsAngles = (atan2((as.numeric(Tracts$EndY[TractsNum]) -
  #                          as.numeric(Tracts$StartY[TractsNum])),
  #                         (as.numeric(Tracts$EndX[TractsNum]) -
  #                          as.numeric(Tracts$StartX[TractsNum])))
  #                         *180)/pi
  #   TractsX = mean(c(as.numeric(Tracts$StartX[TractsNum]),
  #                    as.numeric(Tracts$EndX[TractsNum])))
  #   if (abs(TractsAngles) > 90) {
  #     if (abs(TractsAngles) > 0) {
  #       TractsAngles = TractsAngles - 180
  #     } else {
  #       TractsAngles = TractsAngles + 180
  #     }
  #   }
  #   Graph = Graph + annotate("text", x = mean(c(as.numeric(Tracts$StartX[TractsNum]),
  #                                               as.numeric(Tracts$EndX[TractsNum]))),
  #                                    y = mean(c(as.numeric(Tracts$StartY[TractsNum]),
  #                                               as.numeric(Tracts$EndY[TractsNum]))),
  #                                label = as.character(Tracts$TractsName[TractsNum]),
  #                                 face = "bold", size = 3, colour = TractsColour,
  #                                angle = TractsAngles,
  #                                hjust = 0.5, vjust = 1)
  #   rm(TractsColour, TractsAngles)
  # }
  # print(Graph)

  CircleProjectResults = list(XY = XY, Tracts = Tracts, Text = ProjectText)
  return(CircleProjectResults)
}