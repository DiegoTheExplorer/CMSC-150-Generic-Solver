nvt <- read.csv("csv/nutriVals.csv") #Nutritional Value Table

SimplexDietSolver <- function(poods){
  
  # Start Objective Function Creation
  numpoods <- length(poods)
  poodsInd <- c(1:numpoods)
  xVars <- c()
  funcFormX <- ("(")
  
  
  
  for (i in 1:numpoods){                                #creates a string of formal parameters in the pattern (x1,x2,x3,x4,...,xn) 
    var <- paste("x",i,sep = "")
    browser()
    funcFormX <- paste(funcFormX,var,sep = "")
    xVars <- append(xVars,var)
    if(i != (numpoods))
      funcFormX <- paste(funcFormX,", ", sep = "")
    else
      funcFormX <- paste(funcFormX,")", sep = "")
  }
  print(nvt["PricePerServing"])
  
  objFuncString <- funcFormX                            #creates a string of the objective function
  for(i in 1:numpoods){
    if(i == 1){
      funcString <- paste(funcString, xVars[1])
    }else{

    }
  }
  print(objFuncString)
  # End Objective Function Creation
  retList <- (objective_function = objFuncString)
}