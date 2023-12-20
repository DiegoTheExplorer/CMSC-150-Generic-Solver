nvt <- read.csv("csv/nutriVals.csv") #Nutritional Value Table

SimplexDietSolver <- function(poods){
  
  print(poods)
  
  numpoods <- length(poods)
  xVars <- c()
  funcFormX <- ("(")
  
  for (i in 1:numpoods){        #creates a string of formal parameters in the pattern (x1,x2,x3,x4,...,xn) 
    var <- paste("x",i,sep = "")
    funcFormX <- paste(funcFormX,var,sep = "")
    xVars <- append(xVars,var)
    if(i != (numpoods - 1))
      funcFormX <- paste(funcFormX,", ", sep = "")
    else
      funcFormX <- paste(funcFormX,")", sep = "")
  }
  
  print(xVars)
  # Start Objective Function Creation
  
  # End Objective Function Creation
}