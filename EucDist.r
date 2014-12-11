# Program: EucDist.r
#          Calculate eulerian distance
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141117 skylikewater - first release
#

EucDist <- function(CirPoint = c(0, 0), Point = c(1, 1)) {
  EucDist = ((CirPoint[1] - Point[1])^2
	       + (CirPoint[2] - Point[2])^2)^(1/2)
  return(EucDist)
}