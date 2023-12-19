source('otherRScripts/acm.R')
source('otherRScripts/gaussianMethod.R')

x1 <- c(2,5,7)
y1 <- c(1,8,3)
inp1 < list(x1,y1)

x2 <- c(1,2.5,8.75,11.25)
y2 <- c(2,9,23,25.9)
inp2 < list(x2,y2)

QuadraticSplineInterpolation <- function(x,varVecs){
  
  numdp <- length(varVecs[[1]]) #number of data points
  
} #Quadratic Spline Interpolation