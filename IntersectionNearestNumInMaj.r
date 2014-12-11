# Program: IntersectionNearestNumInMaj.r
#          track nearest num in major circle
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141117 skylikewater - first release
# 141119 skylikewater - set condition in Min2ndDist for avoid some strange error
#

IntersectionNearestNumInMaj <- function(XYSub, IntersectionPoint, MajXY,
                                        AALSlicingPath = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\") {

  source(paste(AALSlicingPath, "EucDist.r", sep = ""))
  Dist = matrix(0, dim(XYSub)[1], 1)
  for (XYNum in 1:dim(XYSub)[1]) {
    Dist[XYNum, 1] = EucDist(XYSub[XYNum, 1:2], IntersectionPoint[1:2])
  }
  Min = min(Dist)
  MinDist = EucDist(XYSub[which(Dist == Min), 1:2], MajXY[1:2])
  Min2ndDist = EucDist(XYSub[which(Dist == min(Dist[ - which(Dist == Min)])), 1:2], MajXY[1:2])
  if (!(exists("Min2ndDist"))) {
    Min2ndDist = MinDist
  }

  if (MinDist < Min2ndDist) { # Impossible equal
    return(which(Dist == Min))
  } else {
    return(which(Dist == min(Dist[-which(Dist == Min)])))
  }
}