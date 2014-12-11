# Program: CircleSubArc.r
#          Draw Circle and Intercepted arc
#          Cited : http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#          c(ROIName, XCord, YCord, Radius)
#          Circle-Circle Intersection:
#          Cited : http://mathworld.wolfram.com/Circle-CircleIntersection.html
#          Circle-Circle Intersection Cord:
#          http://www.ambrsoft.com/TrigoCalc/Circles2/Circle2.htm
#
# Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
#              justin666666@gmail.com
#
# History:
# 141108 skylikewater - first release
# 141112 skylikewater - add sub
# 141120 skylikewater - add ThetaSqrt
#

CircleSubArc <- function(MajCircle = c("Major", 0, 0, 3.141593, "Test", 2),
                         SubCircle = 0,
                         EstPointNum = 500,
                         RadiusLimit = 0,
                         AALSlicingPath = "D:\\Dropbox\\JTWorkspace\\Script\\R\\NTU112\\AALSlicing\\") {
  # check EstPointNum
  if (EstPointNum < 0) {
    stop('EstPoint Num should larger than 0 !!!')
  }
  if ((!(is.numeric(EstPointNum)))||(EstPointNum %% 1 != 0)) {
    stop('Radius Limit should be natural number !!! if you do not want to set it, ignore it.')
  }

  # check RadiusLimit
  if (RadiusLimit < 0) {
    stop('Radius Limit should larger than 0 !!!')
  }
  if (!(is.numeric(RadiusLimit))) {
    stop('Radius Limit should be positive number !!! if you do not want to set it, ignore it or set it 0.')
  }

  # ignore Warning messages from NaN in acos in CircleCircleIntersectionInv
  #options(warn=-1)

  source(paste(AALSlicingPath, "CircleCircleIntersectionInv.r", sep = ""))
  source(paste(AALSlicingPath, "IntersectionNearestNumInMaj.r", sep = ""))
  source(paste(AALSlicingPath, "EucDist.r", sep = ""))
  source(paste(AALSlicingPath, "CircleLinePoint.r", sep = ""))

  # if maj default, open sub default
  if ((MajCircle[1] %in% "Major")&&(MajCircle[4] == 3.141593)) {
    SubCircle = c("ROIA", 0, 0, 3, "Test", 3,
                  "ROIB", 5, 5, 4, "Test", 7)
    SubCircle = matrix(SubCircle, nrow = 2,  ncol = 6, byrow = TRUE)
  }

  # start load maj
  MajX = as.numeric(MajCircle[2])
  MajY = as.numeric(MajCircle[3])
  MajXY = c(MajX, MajY)
  CircleCenter = matrix(c(MajCircle[1:3]), 1, 3)
  MajRadius = as.numeric(MajCircle[4])

  SplitAngle = seq(0, 2*pi, length.out = EstPointNum)
  X = matrix(c(MajX + MajRadius * cos(SplitAngle)), nrow = EstPointNum,  ncol = 1)
  Y = matrix(c(MajY + MajRadius * sin(SplitAngle)), nrow = EstPointNum,  ncol = 1)

  Name = matrix(as.character(MajCircle[1]), nrow = EstPointNum,  ncol = 1)
  MajSub = matrix("Maj", nrow = EstPointNum,  ncol = 1)
  MajCircleSize = pi*(MajRadius^2)
  ROIRegion = matrix(as.character(MajCircle[5]), nrow = EstPointNum,  ncol = 1)
  Depth = matrix(as.numeric(MajCircle[6]), nrow = EstPointNum,  ncol = 1)

  # Set RadiusLimit
  RadiusLimitCircleSize = pi*(RadiusLimit^2)
  if (MajCircleSize < RadiusLimitCircleSize) {
    RadiusLimitMajRatio = RadiusLimit/MajRadius
    if (!(is.numeric(SubCircle))) {
      for (SubNum in 1:dim(SubCircle)[1]) {
        SubCircle[SubNum, 4] = as.numeric(SubCircle[SubNum, 4])*RadiusLimitMajRatio
      }
    }
    MajRadius = RadiusLimit
  }

  # Sub
  if (!(is.numeric(SubCircle))) {
    for (SubNum in 1:dim(SubCircle)[1]) {
      SubXNow = as.numeric(SubCircle[SubNum, 2])
	    SubYNow = as.numeric(SubCircle[SubNum, 3])
      SubXYNow = cbind(SubXNow, SubYNow)
      SubRadiusNow = as.numeric(SubCircle[SubNum, 4])

	    MajDistSub = EucDist(MajXY, SubXYNow)
      AddRadius = MajRadius + SubRadiusNow
      MinusRadius = abs(MajRadius - SubRadiusNow)
      if (MajDistSub > MinusRadius) {
        # MajDistSub <= MinusRadius :
        # internally seperation or tangent
        # MajDistSub > MinusRadius
        # intersects or outer seperation or tangent
        # Control Slice Size in 2/3
        SubCircleSizeNow = pi*(SubRadiusNow^2)
        if (SubCircleSizeNow >= ((2*MajCircleSize)/3)) { # Control Slice Size in 2/3
          SubRadiusNow = MajRadius*sqrt(2/3)
        }
      }

      # Re-estimate
      AddRadius = MajRadius + SubRadiusNow
      # resetting subradius
      if (MajDistSub >= AddRadius) {
        # outer seperation or tangent
        # give new radius for sub-circle by its size
        EstTol = 0.01
        SubRadiusNow = optimize(CircleCircleIntersectionInv,
                                interval = c((MinusRadius + EstTol),
                                             (MajDistSub - EstTol)), # for outer seperation
                                tol = EstTol,
                                Dist = MajDistSub, MajorRadius = MajRadius, Area = SubCircleSizeNow)
        SubRadiusNow = SubRadiusNow$minimum
        rm(EstTol, SubCircleSizeNow)
      }

      # same
      XSub = matrix(c(SubXNow + SubRadiusNow * cos(SplitAngle)), nrow = EstPointNum,  ncol = 1)
      YSub = matrix(c(SubYNow + SubRadiusNow * sin(SplitAngle)), nrow = EstPointNum,  ncol = 1)

      # Re
      MinusRadius = abs(MajRadius - SubRadiusNow)
      if (MajDistSub > MinusRadius) {
        # intersects or outer seperation or tangent
        # calculate section with major circle
        ThetaSqrt = (MajDistSub + MajRadius + SubRadiusNow)*
                    (MajDistSub + MajRadius - SubRadiusNow)*
                    (MajDistSub - MajRadius + SubRadiusNow)*
                 ( - MajDistSub + MajRadius + SubRadiusNow)
        if (ThetaSqrt < 0) {
          ThetaSqrt = 0 # ok, I know it is tricky
        }
        Theta = (1/4)*sqrt(ThetaSqrt)
        XComponent = ((MajX + SubXNow)/2) + ((SubXNow- MajX)*(MajRadius^2 - SubRadiusNow^2))/(2*(MajDistSub^2))
        YComponent = ((MajY + SubYNow)/2) + ((SubYNow- MajY)*(MajRadius^2 - SubRadiusNow^2))/(2*(MajDistSub^2))
        XAddMinus = (2*(MajY - SubYNow)*Theta)/(MajDistSub^2)
        YAddMinus = (2*(MajX - SubXNow)*Theta)/(MajDistSub^2)

        XCross1 = XComponent + XAddMinus
        XCross2 = XComponent - XAddMinus
        YCross1 = YComponent - YAddMinus
        YCross2 = YComponent + YAddMinus
        rm(ThetaSqrt, Theta, XAddMinus, YAddMinus, XComponent, YComponent)

        XYSub = cbind(XSub, YSub)
        IntersectionPoint1 = c(XCross1, YCross1)
        IntersectionPoint2 = c(XCross2, YCross2)
        CrossNum1 = IntersectionNearestNumInMaj(XYSub, IntersectionPoint1, MajXY, AALSlicingPath = AALSlicingPath)
        CrossNum2 = IntersectionNearestNumInMaj(XYSub, IntersectionPoint2, MajXY, AALSlicingPath = AALSlicingPath)
        rm(XYSub)
        rm(XCross1, YCross1, IntersectionPoint1, XCross2, YCross2, IntersectionPoint2)
        MinCrossNum = min(CrossNum1, CrossNum2)
        MaxCrossNum = max(CrossNum1, CrossNum2)
        if (EucDist(MajXY, c(XSub[MinCrossNum + 1], YSub[MinCrossNum + 1])) < MajRadius) {
          # inner in sequence
          XSub = as.matrix(XSub[MinCrossNum:MaxCrossNum])
          YSub = as.matrix(YSub[MinCrossNum:MaxCrossNum])
        } else {
          # avoid half circle be linked
          if (EucDist(MajXY, c(XSub[MinCrossNum], YSub[MinCrossNum])) >= MajRadius) {
            MinCrossNum = MinCrossNum - 1
          }
          if (EucDist(MajXY, c(XSub[MaxCrossNum], YSub[MaxCrossNum])) >= MajRadius) {
            MaxCrossNum = MaxCrossNum + 1
          }
          # outer in sequence
          XSub = rbind(as.matrix(XSub[MaxCrossNum:length(XSub)]), as.matrix(XSub[1:MinCrossNum]))
          YSub = rbind(as.matrix(YSub[MaxCrossNum:length(YSub)]), as.matrix(YSub[1:MinCrossNum]))
        }
        rm(CrossNum1, CrossNum2, MinCrossNum, MaxCrossNum)
        rm(SubXNow, SubYNow, SubRadiusNow, MajDistSub, AddRadius, MinusRadius)
      }

      # renew circle center coordination for print text
      SubXYNow = CircleLinePoint(CirPoint = MajXY,
                                 LinePoint = SubXYNow,
                                 Radius = MajRadius,
                                 AALSlicingPath = AALSlicingPath)

      NameSub = matrix(as.character(SubCircle[SubNum, 1]), nrow = dim(XSub)[1],  ncol = 1)
	    MajSubSub = matrix("Sub", nrow = dim(XSub)[1], ncol = 1)
      ROIRegionSub = matrix(as.character(SubCircle[SubNum, 5]), nrow = dim(XSub)[1],  ncol = 1)
      DepthSub = matrix(as.numeric(SubCircle[SubNum, 6]), nrow = dim(XSub)[1],  ncol = 1)
	    if (SubNum == 1) {
	      XSubAll = XSub
	      YSubAll = YSub
	      NameSubAll = NameSub
	      MajSubSubAll = MajSubSub
        ROIRegionSubAll = ROIRegionSub
        DepthSubAll = DepthSub
	    } else {
	      XSubAll = rbind(XSubAll, XSub)
	      YSubAll = rbind(YSubAll, YSub)
	      NameSubAll = rbind(NameSubAll, NameSub)
	      MajSubSubAll = rbind(MajSubSubAll, MajSubSub)
        ROIRegionSubAll = rbind(ROIRegionSubAll, ROIRegionSub)
        DepthSubAll = rbind(DepthSubAll, DepthSub)
  	  }
      rm(XSub, YSub, NameSub, MajSubSub, ROIRegionSub, DepthSub)

      CircleCenterSub = matrix(c(SubCircle[SubNum, 1], SubXYNow), 1, 3)
      CircleCenter = rbind(CircleCenter, CircleCenterSub)
      rm(SubXYNow, CircleCenterSub)
    }
    X = rbind(X, XSubAll)
    Y = rbind(Y, YSubAll)
    Name = rbind(Name, NameSubAll)
    MajSub = rbind(MajSub, MajSubSubAll)
    ROIRegion = rbind(ROIRegion, ROIRegionSubAll)
    Depth = rbind(Depth, DepthSubAll)
    rm(XSubAll, YSubAll, NameSubAll, MajSubSubAll, ROIRegionSubAll, DepthSubAll)
  }
  XY = data.frame(Name = Name, X = X, Y = Y, MajSub = MajSub, ROIRegion = ROIRegion, Depth = Depth)
  CircleSubResults = list(XY = XY, CircleCenter = CircleCenter)
  rm(MajXY)

  # Test:
  # if(!require(ggplot2)) {
  #   install.packages("ggplot2")
  # } else {
  #   require(ggplot2)
  # }
  # Graph = ggplot(CircleSubResults$XY, aes(x = X, y = Y, group = Name, size = MajSub)) + geom_path()
  # for (CircleCenterNum in 1:dim(CircleSubResults$CircleCenter)[1]) {
  #   Graph = Graph + annotate("text", x = as.numeric(CircleSubResults$CircleCenter[CircleCenterNum, 2]),
  #                                    y = as.numeric(CircleSubResults$CircleCenter[CircleCenterNum, 3]),
  #                                label = as.character(CircleSubResults$CircleCenter[CircleCenterNum, 1]),
  #                                 face = "bold", size = 20)
  # }
  # rm(CircleCenterNum)
  # Graph = Graph + coord_fixed(1)
  # print(Graph)
  return(CircleSubResults)
}