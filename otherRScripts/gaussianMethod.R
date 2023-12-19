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
