# Program: CircleCircleIntersectionInv.r
#          use area size to estimate new radius
#
#          Circle-Circle Intersection:
#          Cited : http://mathworld.wolfram.com/Circle-CircleIntersection.html
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141117 skylikewater - first release
# 141119 skylikewater - add bounded
#

CircleCircleIntersectionInv <- function(Dist, MajorRadius, SubRadius, Area) {
  # options(warn=-1)
  R1 = MajorRadius
  R2 = SubRadius
  d = Dist

  # control value bounded in c(-1, 1)
  AcosR1 = (d^2 + R1^2 - R2^2)/(2*d*R1)
  AcosR2 = (d^2 + R2^2 - R1^2)/(2*d*R2)

  if ((AcosR1 >   1) || (AcosR2 >   1) ||
      (AcosR1 < - 1) || (AcosR2 < - 1)) {
    return(0)
  } else {
    AcosR1 = acos(AcosR1)
    AcosR2 = acos(AcosR2)
    return(R1^2 * AcosR1
         + R2^2 *AcosR2
         - 2*sqrt((-d + R1 + R2)*(d + R1 - R2)*(d - R1 + R2)*(d + R1 +R2))
         - Area)
  }
}