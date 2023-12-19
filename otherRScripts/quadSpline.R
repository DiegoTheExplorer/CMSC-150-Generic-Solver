source('otherRScripts/acm.R')
source('otherRScripts/gaussianMethod.R')


QuadraticSplineInterpolation <- function(x,varVecs){
  
  numdp <- length(varVecs[[1]])     #number of data points
  numintv <- numdp - 1              #number of intervals
  numunk <- numintv * 3             #number of unknowns
  
  funcFormFinal <- ("function(")    #function formal parameters for final output
  funcFormTemp <- ("function(")     #temporary function formal parameters
  abc <- c("a","b","c")
  functionStrings <- c()
  
  for (i in 1:numintv){             #creates a string of formal parameters in the pattern (an,bn,cn,an+1,bn+1,cn+1,...) 
    for (j in 1:3){ 
      if (!(abc[j] == "a" && i == 1)){
        funcFormFinal <- paste(funcFormFinal,paste(abc[j],as.character(i),sep=""),sep="")
        if (!(i == numintv && j == 3))
          funcFormFinal <- paste(funcFormFinal,",",sep="")
        else
          funcFormFinal <- paste(funcFormFinal,")",sep="")
      }
    }
  }
  
  for (i in 1:(numunk - 1)){
    funcFormTemp <- paste(funcFormTemp,paste("x",i,sep = ""),sep = "")
    if(i != (numunk - 1))
      funcFormTemp <- paste(funcFormTemp,",", sep = "")
    else
      funcFormTemp <- paste(funcFormTemp,")", sep = "")
  }
  
  for (i in 2:numintv){
    
  }
  
  
} #Quadratic Spline Interpolation

x1 <- c(2,5,7)
y1 <- c(1,8,3)
inp1 <- list(x1,y1)

x2 <- c(1,2.5,8.75,11.25)
y2 <- c(2,9,23,25.9)
inp2 <- list(x2,y2)

x3 <- c(1,2.5,8.75,11.25)
y3 <- c(2,9,23,25.9)
inp3 <- list(x3,y3)

QuadraticSplineInterpolation(3,inp1)
#QuadraticSplineInterpolation(6,inp1)
QuadraticSplineInterpolation(7,inp2)
QuadraticSplineInterpolation(6,inp3)

