source('otherRScripts/acm.R')
source('otherRScripts/gaussianMethod.R')

genericQuadratic <- function(x,a,b,c) a * x ^ 2 + b * x + c

QuadraticSplineInterpolation <- function(x,varVecs){
  
  xvals <- unlist(varVecs[[1]])
  yvals <- unlist(varVecs[[2]])
  
  numdp <- length(varVecs[[1]])     #number of data points
  numintv <- numdp - 1              #number of intervals
  numunk <- numintv * 3             #number of unknowns
  
  funcFormABC <- ("(")              #function formal parameters that use an bn cn
  funcFormX <- ("(")                #function formal parameters that use x1, x2, x3,...,xn
  abcVars <- c()
  xVars <- c()
  abc <- c("a","b","c")
  functionStrings <- c()
  system <- list()
  
  for (i in 1:numintv){             #creates a string of formal parameters in the pattern (an,bn,cn,an+1,bn+1,cn+1,...) 
    for (j in 1:3){ 
      if (!(abc[j] == "a" && i == 1)){
        var <- paste(abc[j],as.character(i),sep="")
        funcFormABC <- paste(funcFormABC,var,sep="")
        abcVars <- append(abcVars,var)
        if (!(i == numintv && j == 3))
          funcFormABC <- paste(funcFormABC,", ",sep="")
        else
          funcFormABC <- paste(funcFormABC,")",sep="")
      }
    }
  }

  for (i in 1:(numunk - 1)){        #creates a string of formal parameters in the pattern (x1,x2,x3,x4,...,xn) 
    var <- paste("x",i,sep = "")
    funcFormX <- paste(funcFormX,var,sep = "")
    xVars <- append(xVars,var)
    if(i != (numunk - 1))
      funcFormX <- paste(funcFormX,", ", sep = "")
    else
      funcFormX <- paste(funcFormX,")", sep = "")
  }

  for (i in 1:numintv){                                                             #Function creation for conditions 1 and 2
    func1 <- paste("function",funcFormABC,sep = " ")
    func2 <- paste("function",funcFormABC,sep = " ")
    tempabc1 <- abc
    tempabc2 <- abc
    
    if (i == 1){
      for (j in 1:3){                                                               #creates function strings for condition 2
        tempabc1[j] <- paste(tempabc1[j],as.character(1),sep = "")
        tempabc2[j] <- paste(tempabc2[j],as.character(numintv),sep = "")
      }
      
      at1 <- paste(as.character(xvals[1] ^ 2),tempabc1[1], sep = " * ")                 #x0 ^ 2 * a1
      at2 <- paste(as.character(xvals[numdp] ^ 2),tempabc2[1], sep = " * ")             #xn ^ 2 * an
      bt1 <- paste(xvals[1],tempabc1[2], sep = " * ")                                   #x0 * b1
      bt2 <- paste(xvals[numdp],tempabc2[2], sep = " * ")                               #xn * bn
      ct1 <- paste(1,tempabc1[3],sep = " * ")
      ct2 <- paste(1,tempabc2[3],sep = " * ")
      
      terms1 <- paste(at1,bt1,ct1,as.character(yvals[1] * -1), sep = " + ")     #x0 ^ 2 * a1 + x0 * b1 + cn - f(x0)
      terms2 <- paste(at2,bt2,ct2,as.character(yvals[numdp] * -1), sep = " + ") #xn ^ 2 * an + xn * bn + cn - f(xn)
      
      terms1 <- sub("[0-9]+ \\* a1 \\+ ","", terms1)
      func1 <- paste(func1,terms1, sep = " ")
      func2 <- paste(func2,terms2, sep = " ")
      
      functionStrings <- append(functionStrings,func1,length(functionStrings))
      functionStrings <- append(functionStrings,func2,length(functionStrings))                      
    }else{  
      for (j in 1:3){                                                               #creates function strings for condition 1
        tempabc1[j] <- paste(tempabc1[j],as.character(i-1),sep = "")
        tempabc2[j] <- paste(tempabc2[j],as.character(i),sep = "")
      }
      
      at1 <- paste(as.character(xvals[i] ^ 2),tempabc1[1], sep = " * ")             #xi-1 ^ 2 * an
      at2 <- paste(as.character(xvals[i] ^ 2),tempabc2[1], sep = " * ")
      bt1 <- paste(xvals[i],tempabc1[2], sep = " * ")                               #xi-1 * bn
      bt2 <- paste(xvals[i],tempabc2[2], sep = " * ")
      ct1 <- paste(1,tempabc1[3],sep = " * ")                                       #1 * cn
      ct2 <- paste(1,tempabc2[3],sep = " * ")
      
      terms1 <- paste(at1,bt1,ct1,as.character(yvals[i] * -1), sep = " + ")         #xi-1 ^ 2 * an + xi-1 * bn + cn - f(xi-1)
      terms2 <- paste(at2,bt2,ct2,as.character(yvals[i] * -1), sep = " + ")
      
      terms1 <- sub("[0-9].+ \\* a1 \\+ ","", terms1)
      func1 <- paste(func1,terms1, sep = " ")
      func2 <- paste(func2,terms2, sep = " ")
      
      functionStrings <- append(functionStrings,func1,length(functionStrings) - 2)
      functionStrings <- append(functionStrings,func2,length(functionStrings) - 2)
      
    }
  } #Function creation for conditions 1 and 2
  
  for (i in 2:numintv){                                                             #Function creation for condition 3
    func1 <- paste("function",funcFormABC,sep = " ")
    tempabc1 <- abc
    tempabc2 <- abc
    for (j in 1:3){                                                            
      tempabc1[j] <- paste(tempabc1[j],as.character(i-1),sep = "")
      tempabc2[j] <- paste(tempabc2[j],as.character(i),sep = "")
    }
    
    at1 <- paste(as.character(xvals[i] * 2),tempabc1[1], sep = " * ")             
    at2 <- paste(as.character(xvals[i] * -2),tempabc2[1], sep = " * ")
    bt1 <- paste(1,tempabc1[2], sep = " * ")                               
    bt2 <- paste("-1",tempabc2[2], sep = " * ")
    
    terms1 <- paste(at1,bt1,at2,bt2, sep = " + ")
    terms1 <- sub("[0-9]+ \\* a1 \\+ ","", terms1)
    func1 <- paste(func1,terms1, sep = " ")
    functionStrings <- append(functionStrings,func1,length(functionStrings))
  }
  
  for (i in 1:(length(functionStrings))){ 
    #print(functionStrings[i])
    #print(system[i])
  }
  
  for (i in 1:(numunk - 1)){                                #substitute abc variables for xn
    for (j in 1:(numunk - 1))
      functionStrings[i] <- gsub(abcVars[j],xVars[j],functionStrings[i])        
  }
  
  for (i in 1:(length(functionStrings))){                   #parse text to r expressions
    #print(functionStrings[i])
    system[i] <- parse(text = functionStrings[i])
    #print(system[i])
  }
  
  acm <- AugCoeffMatrix(system)                             #convert to augemented coefficient matrix
  gauss <- GaussianMethod(xVars,acm$augcoeffmatrix)         #solve acm through gaussian
  
  #Initializing the final quadratic equations
  funcStringTemplate <- "function(x) a * x ^ 2 + b * x + c"
  outputFuncStrings <- list()
  
  finalCoeffs <- list(1:(numintv))
  temp <- append(gauss$solution,0,0)
  for (i in 1:numintv){
    finalCoeffs[[i]] <- c(1:3)                              #stores the final coefficients in this format:                               
    for (j in 1:3){                                         #((a1,b1,c1),(a2,b2,c2),...,(an,bn,cn)) 
      if (i == 1 && j == 1)
        finalCoeffs[[i]][j] <- 0
      else{
        finalCoeffs[[i]][j] <- temp[((i - 1) * 3) + j]
      }
    }
  }
  
  for (i in 1:numintv){
    temp <- funcStringTemplate                              #make string versions of the functions for each interval
    for (j in 1:3){
      subChar <- paste(" ",abc[j], sep = "")
      temp <- sub(subChar,as.character(finalCoeffs[[i]][j]),temp)
    }
    outputFuncStrings <- append(outputFuncStrings,temp)
  }
  
  rangeLabels <- c(1:numintv)
  for (i in 1:numintv){
    temp <- paste(xvals[i],"< x <",xvals[i + 1])
    rangeLabels[i] <- temp
  }
  setNames(outputFuncStrings,rangeLabels)
  
  targetInt = 1
  for (i in 1:numintv){                                     
    if (x > xvals[i])                                       #Choose the correct interval to use
      targetInt <- targetInt + 1
    else{
      targetInt <- targetInt - 1
      break
    }
  }

  estimate <- genericQuadratic(x,finalCoeffs[[targetInt]][1],finalCoeffs[[targetInt]][2],finalCoeffs[[targetInt]][3])
  returnList <- list(interpolating_functions = outputFuncStrings, estimate = estimate, xvals = xvals, yvals = yvals)
  
  return(returnList)
  
} #Quadratic Spline Interpolation