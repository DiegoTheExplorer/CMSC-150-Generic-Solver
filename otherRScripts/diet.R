nvt <- read.csv("csv/nutriVals.csv") #Nutritional Value Table

SimplexDietSolver <- function(poods){
  
  # Start Objective Function Creation
  numpoods <- length(poods)
  poodsInd <- c(1:numpoods)
  xVars <- c()
  funcFormX <- ("(")
  
  for (i in 1:numpoods){
    var <- paste("x",i,sep = "")                        #creates a string of formal parameters in the pattern (x1,x2,x3,x4,...,xn)
    funcFormX <- paste(funcFormX,var,sep = "")
    xVars <- append(xVars,var)
    if(i != (numpoods))
      funcFormX <- paste(funcFormX,", ", sep = "")
    else
      funcFormX <- paste(funcFormX,")", sep = "")
    
    poodsInd[i] <- match(poods[i],nvt["Foods"])
  }
  print(nvt["Foods"])
  print(poods)
  print(poodsInd)
  
  objFuncString <- funcFormX                            #creates a string of the objective function
  for(i in 1:numpoods){
    if(i == 1){
      objFuncString <- paste(objFuncString, xVars[1])
    }else{

    }
  }
  print(objFuncString)
  # End Objective Function Creation
  retList <- (objective_function = objFuncString)
}