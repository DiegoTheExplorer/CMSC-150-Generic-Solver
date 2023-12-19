source('otherRScripts/gaussianMethod.R')

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