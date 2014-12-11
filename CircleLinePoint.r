# Program: CircleLinePoint.r
#          Calculate circle and line intersection point
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141117 skylikewater - first release
# 141118 skylikewater - use Trigonometric functions
#

CircleLinePoint <- function(CirPoint, LinePoint, Radius,
                            AALSlicingPath = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\") {
  if (CirPoint[1] == LinePoint[1]) {
  	# cannot use formula bcz NaN
  	X = CirPoint[1]
  	if (CirPoint[2] > LinePoint[2]) {
      Y = CirPoint[2] - Radius
  	} else if (CirPoint[2] < LinePoint[2]) {
      Y = CirPoint[2] + Radius
  	} else {
  	  # for check
  	  X = 0
  	  Y = 0
  	}
    XY = c(X, Y)
  } else {
  	# Line formula : y = Beta0 + Beta1*x
    Beta1 = (CirPoint[2] - LinePoint[2])/(CirPoint[1] - LinePoint[1])
    Beta0 = CirPoint[2] - Beta1*CirPoint[1]

    # Circle formula : (x - CircleCenterX)^2 + (y - CircleCenterY)^2 = Radius^2
    # => (x - CircleCenterX)^2 + (Beta0 + Beta1*x - CircleCenterY)^2 = Radius^2
    source(paste(AALSlicingPath, "CircleFormula.r", sep = ""))
    # error ...

    if (CirPoint[1] > LinePoint[1]) {
      EstInterval = c((CirPoint[1] - Radius), (CirPoint[1]))
      EstTol = (EstInterval[2] - EstInterval[1])/10000
      X = optimize(CircleFormula, maximum = TRUE,
                   interval = EstInterval,
                   tol = EstTol,
                   Beta0 = Beta0, Beta1 = Beta1,
                   CircleCenterX = CirPoint[1], CircleCenterY = CirPoint[2],
                   Radius = Radius, Status = "Max")
      X = X$maximum
    } else if (CirPoint[1] < LinePoint[1]) {
      EstInterval = c((CirPoint[1]), (CirPoint[1] + Radius))
      EstTol = (EstInterval[2] - EstInterval[1])/10000
      X = optimize(CircleFormula, maximum = FALSE,
                   interval = EstInterval,
                   tol = EstTol,
                   Beta0 = Beta0, Beta1 = Beta1,
                   CircleCenterX = CirPoint[1], CircleCenterY = CirPoint[2],
                   Radius = Radius, Status = "Min")
      X = X$minimum
    }


    Y = Beta0 + Beta1*X
    XY = c(X, Y)
  }
  return(XY)
}