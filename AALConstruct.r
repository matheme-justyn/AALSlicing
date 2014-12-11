# Program: AALConstruct.r
#          Create AAL Coordinate
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141028 skylikewater - first release
#

# source("D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\AALConstruct.r")

AALConstruct <- function() {
  DimLimit = matrix(c(90, 91, 109, -90, -125, -71), nrow = 2, ncol = 3, byrow = TRUE)

  Path = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\"
  AALROICordPath = paste(Path, "AALROICord.csv", sep = "")
  AALROICord = read.table(AALROICordPath, header = TRUE, sep = ",")
  AALROISizePath = paste(Path, "AALROISize.csv", sep = "")
  AALROISize = read.table(AALROISizePath, header = TRUE, sep = ",")

  ROITotal = dim(AALROICord)[1]
  ROI = matrix(0, ROITotal, 6)
  colnames(ROI) = c("ROIName", "Radius", "XCord", "YCord", "ZCord", "ROIRegion")
  ROI[, 1] = as.character(AALROICord[, 1])
  ROI[, 6] = as.character(AALROICord[, 8])
  
  # calculate Radius
  for (ROINow in 1:ROITotal) {
    ROIName = ROI[ROINow,1]
	Last2 = substr(ROIName, start = (nchar(ROIName) - 1), stop = nchar(ROIName))
	ROISimName = substr(ROIName, start = 1, stop = (nchar(ROIName) - 2))
	ROILR = substr(ROIName, start = nchar(ROIName), stop = nchar(ROIName))
	if ((Last2 %in% "_L")||(Last2 %in% "_R")) {
	  if (ROILR %in% "L") {
	    ROILRNum = 3
	  } else if (ROILR %in% "R") {
	    ROILRNum = 4
	  }
	  ROI[ROINow, 2] = (AALROISize[AALROISize[,1] == ROISimName, ROILRNum])^(1/3)
	} else {
	  ROI[ROINow, 2] = (AALROISize[AALROISize[,1] == ROIName, 2])^(1/3)
	}
  }
  ROI[, 3] = ((as.numeric(AALROICord[,2]) + DimLimit[2, 1]) + (as.numeric(AALROICord[,3]) + DimLimit[2, 1]))/2
  ROI[, 4] = ((as.numeric(AALROICord[,4]) + DimLimit[2, 2]) + (as.numeric(AALROICord[,5]) + DimLimit[2, 2]))/2
  ROI[, 5] = ((as.numeric(AALROICord[,6]) + DimLimit[2, 3]) + (as.numeric(AALROICord[,7]) + DimLimit[2, 3]))/2
  
  save(ROI, file = paste(Path, "AALROI.RData", sep = ""))
  write.table(ROI, file = paste(Path, "AALROI.csv", sep = ""), sep = ",")
}