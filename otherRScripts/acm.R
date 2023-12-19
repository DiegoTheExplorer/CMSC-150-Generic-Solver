AugCoeffMatrix <- function(sys){
  
  #Check if list is empty
  if(length(sys) == 0){
    print("List of equations is empty")
    return(NA);
  }
  
  deparsedSys <- lapply(sys,deparse,width.cutoff = 500)
  numEq <- length(deparsedSys)
  unknowns <- ""

  #Separating the unknowns into their own list
  for (i in 1:numEq){
    print(deparsedSys[[i]][[1]])
    currLen = nchar(deparsedSys[[i]][[1]])
    #check if number of unknowns is not even to the previous
    if(i > 1){
      browser()
      if(unknownsLen != currLen){
        print("Number of unknowns was not equal")
        return(NA)
      }
    }
    unknownsLen <- currLen
    
    #Store the list of unknowns
    if(i == 1){
      justSymbols <- substring(deparsedSys[[i]][[1]], 11, unknownsLen - 2)
      justSymbols <- strsplit(justSymbols, ", ")
      unknowns <- justSymbols
    }
    
  }
  
  #Initialize the N:1 matrix for the Right Hand Side of each equation
  RHS <- matrix(data=0, nrow=numEq, ncol = 1)
  colnames(RHS) <- c("RHS")
  numUnknowns <- length(unknowns[[1]])
  
  #Creating the coefficient matrix
  tempMatrix <- matrix(c(0), nrow = numEq, ncol = numEq)
  for(i in 1:numEq){
    
    #Check if the deparsed expresion is split into multiple strings
    if(length(deparsedSys) > 2){
      
    }
    temp <- strsplit(deparsedSys[[i]][[2]], " \\+ ")
    len <- length(temp[[1]])
    #browser()
    for(j in 1:(length(temp[[1]]))){
      #Check if current the term is not a constant
      print(temp[[1]][[j]])
      print(grepl("\\*", temp[[1]][[j]]))
      if(grepl("\\*", temp[[1]][[j]])){
        temp[[1]][[j]] <- strsplit(temp[[1]][[j]], " \\* ")
        temp[[1]][[j]][[1]][[2]] <- substring(temp[[1]][[j]][[1]][[2]],2,2)
        col = as.numeric(temp[[1]][[j]][[1]][[2]])
        val = as.numeric(temp[[1]][[j]][[1]][[1]])
        tempMatrix[i,col] = val
      }else{
        RHS[i][1] <- as.numeric(temp[[1]][[j]]) * -1
      }
    }
  }
  
  colnames(tempMatrix) <- unlist(unknowns)
  rownames(tempMatrix) <- c(1:numEq)
  return(list(variables= unlist(unknowns),augcoeffmatrix=cbind(tempMatrix,RHS)))
}