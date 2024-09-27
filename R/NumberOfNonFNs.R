# function to calculate number of non-FNs for each variable

NumberOfNonFNs <- function(inputMatrix,trapezoidal)
{
  # number of all variables
  
  parameterTrapezoidal <- ifelse(trapezoidal,4,3)
  
  varNumber <- ncol(inputMatrix) / parameterTrapezoidal
  
  # cat("varNumber: ", varNumber, "\n")
  
  # output vector
  
  output <- rep(NA,times=varNumber+1)
  
  names(output) <- c(noquote(paste("V", 1:varNumber, sep="")),"mean")
  
  # cat("output: ", output, "\n")
  
  # for loop for all variables
  
  for (i in 1:varNumber) {
    
    # cat("i: ", i, "\n")
    
    # find the right range for each variable
    
    rangeToCheck <- c((parameterTrapezoidal*(i-1)+1):(parameterTrapezoidal*i))
    
    # cat("rangeToCheck: ", rangeToCheck, "\n")
    
    # select only this variable for calculation of non-FNs
    
    outputSingleVar <- which(apply(inputMatrix[,rangeToCheck], MARGIN=1, FUN=IsFuzzy, trapezoidal=trapezoidal) == FALSE)
    
    # number of these non-FNs
    
    outputSingleVar <- length(outputSingleVar)

    
    # input to the output vector
    
    output[i] <- outputSingleVar
    
  }
  
  output["mean"] <- mean(output[c(1:varNumber)])
  
  return(output)
  
  
  
  
}
