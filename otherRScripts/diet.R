nvt <- read.csv("csv/nutriVals.csv") #Nutritional Value 

SimplexDietSolver <- function(poods){
  
  # Start Objective Function Creation
  numpoods <- length(poods)
  poodsInd <- c(1:numpoods)
  poodsCost <- c(1:numpoods)
  foodNames <- unlist(nvt["Foods"])
  foodCost <- unlist(nvt["PricePerServing"])
  xVars <- c()
  funcFormX <- ("(")
  maxNutConstraints <- c(1:11)
  minNutConstraints <- c(1:11)
  maxConsRHS <- c(-2250,-300,-65,-2400,-300,-100,-100,-50000,-20000,-1600,-30)
  minConsRHS <- c(2000,0,0,0,0,25,50,5000,50,800,10)
  maxServCons <- c(1:numpoods)
  
  poodsInfo <- matrix(data = 0, nrow = numpoods, ncol = 12)
  #nutVals column index guide:
  # 1 <- Calories
  # 2 <- Cholesterol
  # 3 <- Total Fat
  # 4 <- Sodium
  # 5 <- Carbohydrates
  # 6 <- Dietary Fiber
  # 7 <- Protein
  # 8 <- Vitamin A
  # 9 <- Vitamin C
  # 10 <- Calcium
  # 11 <- Iron
  # 12 <- Cost
  
  for (i in 1:numpoods){
    var <- paste("x",i,sep = "")                               #creates a string of formal parameters in the pattern (x1,x2,x3,x4,...,xn)
    funcFormX <- paste(funcFormX,var,sep = "")
    xVars <- append(xVars,var)
    if(i != (numpoods))
      funcFormX <- paste(funcFormX,", ", sep = "")
    else
      funcFormX <- paste(funcFormX,")", sep = "")
    
    foodInd <- grep(poods[i],foodNames)                        #stores the data from row index of each selected food 
    poodsInd[i] <- foodInd
    poodsInfo[i,12] <- foodCost[poodsInd[i]]
    
    for (j in 1:11){
      poodsInfo[i,j] <- nvt[poodsInd[i],(j + 3)]                   
    }
    
  }
  
  
  objFuncString <- funcFormX                                   #creates a string of the objective function
  minObjFuncString <- funcFormX
  for(i in 1:numpoods){
    if(i == 1){
      term <- paste(poodsInfo[i,12], xVars[i], sep = " * ")
      objFuncString <- paste(objFuncString, term, sep = " ")
      term <- paste(-1 * poodsInfo[i,12], xVars[i], sep = " * ")
      minObjFuncString <- paste(minObjFuncString, term, sep = " ")
    }else{
      term <- paste(poodsInfo[i,12], xVars[i], sep = " * ")
      objFuncString <- paste(objFuncString, term, sep = " + ")
      term <- paste(-1 * poodsInfo[i,12], xVars[i], sep = " * ")
      minObjFuncString <- paste(minObjFuncString, term, sep = " + ")
    }
  }
  objFuncString <- paste("function",objFuncString,sep = " ")
  minObjFuncString <- paste("function",minObjFuncString,sep = " ")
  # End Objective Function Creation

  
  #Start of Function Creation for Constraints
  
  for(i in 1:11){# iterate through nutrients
    maxfuncString <- funcFormX
    minfuncString <- funcFormX
    for(j in 1:numpoods){#iterate through poods
      if(j == 1){                                                           #Creates max and min constraint functions 
        termMax <- paste(poodsInfo[j,i], xVars[j], sep = " * ")             #for each food for every nutrient
        termMin <- paste(-1 * poodsInfo[j,i], xVars[j], sep = " * ")
        
        maxfuncString <- paste(maxfuncString, termMax, sep = " ")
        minfuncString <- paste(minfuncString, termMin, sep = " ")
      }else{
        termMax <- paste(poodsInfo[j,i], xVars[j], sep = " * ")
        termMin <- paste(-1 * poodsInfo[j,i], xVars[j], sep = " * ")
        
        maxfuncString <- paste(maxfuncString, termMax, sep = " + ")
        minfuncString <- paste(minfuncString, termMin, sep = " + ")
      }
    }
    maxfuncString <- paste(maxfuncString, maxConsRHS[i], sep = " + ")       #Adds the constant for the current function
    minfuncString <- paste(minfuncString, minConsRHS[i], sep = " + ")
    
    maxNutConstraints[i] <- paste("function",maxfuncString,sep = " ")
    minNutConstraints[i] <- paste("function",minfuncString,sep = " ")
  }
  
  print("Maximization Constraints:")
  print(maxNutConstraints)
  print("Minimization Constraints:")
  print(minNutConstraints)
  
  for (i in 1:numpoods){
    maxfuncString <- funcFormX
    term <- paste("1 *",xVars[i])
    maxfuncString <- paste("function",maxfuncString, term, "+ -10")
    maxServCons[i] <- maxfuncString
  }
  #End of Function Creation for constraints
  
  #Start of Simplex Solving
  
    #Combine all the constraints into 1 vector
    sys <- list(maxNutConstraints,minNutConstraints,maxServCons,minObjFuncString)
    sys <- unlist(sys)
    print(sys)
    #Call simplex
    #solution <- SimplexMinimization(as.list(sys))
  #End of Simplex Solving
  retList <- list(objective_function = objFuncString)
  return(retList)
}