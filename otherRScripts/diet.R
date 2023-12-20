nvt <- read.csv("csv/nutriVals.csv") #Nutritional Value Table

SimplexDietSolver <- function(poods){
  
  # Start Objective Function Creation
  numpoods <- length(poods)
  poodsInd <- c(1:numpoods)
  poodsCost <- c(1:numpoods)
  foodNames <- unlist(nvt["Foods"])
  foodCost <- unlist(nvt["PricePerServing"])
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
    
    foodInd <- grep(poods[i],foodNames)               #stores the data from row index of each selected food 
    poodsInd[i] <- foodInd
    poodsCost[i] <- foodCost[poodsInd[i]]              #stores the price per serving of each selected food
  }
  
  objFuncString <- funcFormX                           #creates a string of the objective function
  for(i in 1:numpoods){
    if(i == 1){
      term <- paste(xVars[i],poodsCost[i], sep = " * ")
      objFuncString <- paste(objFuncString, term, sep = " ")
    }else{
      term <- paste(xVars[i],poodsCost[i], sep = " * ")
      objFuncString <- paste(objFuncString, term, sep = " + ")
    }
  }
  objFuncString <- paste("function",objFuncString,sep = " ")
  # End Objective Function Creation
  retList <- list(objective_function = objFuncString)
  return(retList)
}