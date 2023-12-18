a <- c(20,20,25,27,30,30,33,35,35,40)
b <- c(8.75,9.43,12.87,14.24,16.89,18.94,25.48,30.11,36.07,51.27)

partialPivot <- function(acm, pivot, curr)
{
  
  tempRow <- acm[pivot,]
  acm[pivot,] <- acm[curr,]
  acm[curr,] <- tempRow
  return(acm)
  
}

GaussianMethod <- function(vars, acm)
{
  numUnknowns = length(vars)
  solution = c(1:numUnknowns)
  
  #Forward elmination
  for (i in 1:(numUnknowns - 1))
  {
    #Find the index of the pivot Row
    pivotRow = which.max(abs(acm[i:numUnknowns, i]))
    pivotRow = pivotRow + i - 1#adjust index to be used on the matrix
    if (acm[pivotRow, i] == 0) 
    {
      print("no unique solution exists\n")
      return(list(variables=vars, augcoeffmatrix=acm, solution=NA))
    }
    #swap rows at indices i and pivotRow
    acm <- partialPivot(acm,pivotRow,i)
    for (j in (i + 1):numUnknowns)
    {
      pivotElem = acm[i,i]
      multiplier = acm[j,i] / pivotElem
      normalizedRow = multiplier * acm[i,]
      acm[j,] <- acm[j,] - normalizedRow
    }
  }
  
  #Backwards Substitution
  
  #solve last row
  acm[numUnknowns,numUnknowns + 1] <- (acm[numUnknowns,numUnknowns+1] / acm[numUnknowns, numUnknowns])
  acm[numUnknowns,numUnknowns] <- acm[numUnknowns,numUnknowns] / acm[numUnknowns,numUnknowns]
  solution[numUnknowns] <- acm[numUnknowns,numUnknowns + 1]
  
  #Backwards substitution the other rows
  for (i in (numUnknowns - 1):1)
  {
    for (j in numUnknowns:(i+1))
      acm[i,j] <- acm[i,j] * solution[j]
    acm[i,numUnknowns+1] <- (acm[i,numUnknowns+1] - sum(acm[i,numUnknowns:(i+1)])) / acm[i,i]
    solution[i] <- acm[i,numUnknowns+1]
  }
  
  solution <- acm[1:numUnknowns,numUnknowns+1]
  names(solution) <- vars
  
  return(list(variables=vars, augcoeffmatrix=acm, solution=solution))
}

PolynomialRegression <- function(n, varVecs){
  #n = degree of the polynomial
  acm <- matrix(data = 0, nrow = (n + 1), ncol = (n + 1))
  RHS <- c(1:(n+1))
  vars <- RHS
  
  #Calculate values for the (n+1) by (n+1) matrix
  for(i in 1:(n + 1)){
    for(j in 1:(n + 1)){
      temp <- unlist(varVecs[1]) 
      acm[i,j] <-  sum(temp ^ ((j-1) + (i - 1)))
    }
  }
  
  #Calculate the values for the RHS of the augcoeffmatrix
  for(i in 1:(n+1)){
    b <- unlist(varVecs[1])
    y <- unlist(varVecs[2])
    RHS[i] <- sum(b ^ (i-1) * y)
    
    #create the variable vector
    vars[i] <- paste("x", as.character(i), sep="")
  }
  acm <- cbind(acm,RHS)
  coeffs <- unname(unlist(GaussianMethod(vars,acm)["solution"]))
  
  #Create the function string
  funcString <- "function(x)"
  for(i in 1:(n+1)){
    if(i == 1){
      funcString <- paste(funcString, as.character(coeffs[i]))
    }else{
      temp <- paste(paste("* x", paste("^", as.character(i - 1))))
      coeff <- paste("+", as.character(coeffs[i]))
      term <- paste(coeff, temp)
      funcString <- paste(funcString, term)
    }
  }
  
  #convert from string to R function
  func <- eval(parse(text=funcString))
  
  return(list(augcoeffmatrix=acm, coefficients=coeffs, polynomial_string=funcString, polynomial_function=func))
}

print(PolynomialRegression(1, list(a,b)))
print(PolynomialRegression(2, list(a,b)))
print(PolynomialRegression(3, list(a,b)))