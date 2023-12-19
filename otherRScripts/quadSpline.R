source('otherRScripts/acm.R')
source('otherRScripts/gaussianMethod.R')


QuadraticSplineInterpolation <- function(x,varVecs){
  
  xvals <- unlist(varVecs[[1]])
  yvals <- unlist(varVecs[[2]])
  
  numdp <- length(varVecs[[1]])     #number of data points
  numintv <- numdp - 1              #number of intervals
  numunk <- numintv * 3             #number of unknowns
  
  funcFormABC <- ("(")              #function formal parameters that use an bn cn
  funcFormX <- ("(")                #function formal parameters that use x1, x2, x3,...,xn
  abc <- c("a","b","c")
  functionStrings <- list()
  
  for (i in 1:numintv){             #creates a string of formal parameters in the pattern (an,bn,cn,an+1,bn+1,cn+1,...) 
    for (j in 1:3){ 
      if (!(abc[j] == "a" && i == 1)){
        funcFormABC <- paste(funcFormABC,paste(abc[j],as.character(i),sep=""),sep="")
        if (!(i == numintv && j == 3))
          funcFormABC <- paste(funcFormABC,",",sep="")
        else
          funcFormABC <- paste(funcFormABC,")",sep="")
      }
    }
  }
  
  for (i in 1:(numunk - 1)){        #creates a string of formal parameters in the pattern (x1,x2,x3,x4,...,xn) 
    funcFormX <- paste(funcFormX,paste("x",i,sep = ""),sep = "")
    if(i != (numunk - 1))
      funcFormX <- paste(funcFormX,",", sep = "")
    else
      funcFormX <- paste(funcFormX,")", sep = "")
  }
  
  for (i in 2:numintv){             #creates function strings for condition 1
    func1 <- paste("function",funcFormABC,sep = "")
    func2 <- paste("function",funcFormABC,sep = "")
    tempabc1 <- abc
    tempabc2 <- abc
    
    for (j in 1:3){
      tempabc1[j] <- paste(tempabc1[j],as.character(i-1),sep = "")
      tempabc2[j] <- paste(tempabc2[j],as.character(i),sep = "")
    }
    
    at1 <- paste(as.character(xvals[i] ^ 2),tempabc1[1], sep = " * ")
    at2 <- paste(as.character(xvals[i] ^ 2),tempabc2[1], sep = " * ")
    bt1 <- paste(xvals[i],tempabc1[2], sep = " * ")
    bt2 <- paste(xvals[i],tempabc1[2], sep = " * ")
    
    # func1 <- paste(func1, at1)                                          #function() xi-1 * a
    # func2 <- paste(func2, at2)
    # func1 <- paste(func1, bt1, sep = " + ")                             #function() xi-1 ^ 2 * a + xi-1 * b
    # func2 <- paste(func2, bt2, sep = " + ")
    # func1 <- paste(func1, tempabc1[3], sep = " + ")                     #function() xi-1 ^ 2 * a + xi-1 * b + c
    # func2 <- paste(func2, tempabc2[3], sep = " + ")
    # func1 <- paste(func1, as.character(yvals[i] * -1), sep = " + ")     #function() xi-1 ^ 2 * a + xi-1 * b + c
    # func2 <- paste(func2, as.character(yvals[i] * -1), sep = " + ")
    
    terms1 <- paste(at1,bt1,tempabc1[3],as.character(yvals[i] * -1), sep = " + ")
    terms2 <- paste(at2,bt2,tempabc2[3],as.character(yvals[i] * -1), sep = " + ")
    
    func1 <- paste(func1,terms1)
    func2 <- paste(func2,terms2)
    
    functionStrings <- append(functionStrings,func1,0)
    functionStrings <- append(functionStrings,func2,0)
    
    temp1 <- paste()
  }
  print(functionStrings)
  
} #Quadratic Spline Interpolation

x1 <- c(2,5,7)
y1 <- c(1,8,3)
inp1 <- list(x1,y1)

x2 <- c(1,2.5,8.75,11.25)
y2 <- c(2,9,23,25.9)
inp2 <- list(x2,y2)

x3 <- c(3,4.5,7,9)
y3 <- c(2.5,1,2.5,0.5)
inp3 <- list(x3,y3)

#QuadraticSplineInterpolation(3,inp1)
#QuadraticSplineInterpolation(6,inp1)
#QuadraticSplineInterpolation(7,inp2)
QuadraticSplineInterpolation(6,inp3)

