# Program: CircleFormula.r
#          Calculate circle formula for optimize
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141118 skylikewater - first release
#

CircleFormula <- function(X, Beta0, Beta1, CircleCenterX, CircleCenterY, Radius, Status) {
  # options(warn = -1)
  Est = ((X - CircleCenterX)^2 +
         (Beta0 + Beta1*X - CircleCenterY)^2 -
         (Radius^2))

  if (Status %in% "Max") {
    if (Est > 0) {
      return( - 100000) # small number
    } else {
      return(Est)
    }
  } else if (Status %in% "Min") {
    if (Est < 0) {
      return(100000) # large number
    } else {
      return(Est)
    }
  }
}