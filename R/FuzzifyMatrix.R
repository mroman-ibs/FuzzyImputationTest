#' Fuzzyfing the crisp values.
#'
#' @description
#' `FuzzifyMatrix` converts real-valued variables into fuzzy numbers.
#'
#' @details
#' The procedure generates trapezoidal fuzzy numbers (when the default \code{trapezoidal=TRUE} is set) or triangular
#' ones (for \code{trapezoidal=FALSE}) based on the real-valued data from the given matrix or the data frame.
#' To do this, for each variable the standard deviation is calculated.
#' Then, the left and right increments of the core (in the case of trapezoidal fuzzy numbers) are
#' randomly generated using the original value plus/minus two random values from
#' the uniform distribution on the interval [0,\code{coreFactor}*(standard deviation)].
#' In the case of triangular fuzzy numbers, the cores are equal to the original real values.
#' In the same manner, the left and right increments of the support are randomly generated with
#' two random values from the uniform distribution on the interval [0,\code{supportFactor}*(standard deviation)].
#' 
#'
#'
#' @return
#' The output is given as a matrix with three (in the case of triangular fuzzy numbers) or four (for trapezoidal
#' fuzzy numbers) columns for each input variable.
#'
#'
#'
#'
#'
#' @param crispMatrix Name of the input matrix (or data frame) with real-valued variables to fuzzify.
#'
#'
#' @param coreFactor Value used as the multiplier for the right end of the interval of the uniform distribution applied to
#' randomly generated increments of the core.
#' 
#' @param supportFactor Value used as the multiplier for the right end of the interval of the uniform distribution applied to
#' randomly generated increments of the support.
#' 
#' @param trapezoidal Logical value that indicates if trapezoidal (or triangular, otherwise) fuzzy numbers should be generated.
#' 
#' @param varNames Names of the input variables.
#'
#' @param ... Additional parameters passed to other functions.
#'
#' @examples
#'
#' # set seed for the random generator
#' 
#' set.seed(12345)
#'
#' # let's look at the beginning of the iris dataset (four numeric variables)
#' 
#' head(iris[,1:4])
#' 
#' # and fuzzify these variables
#'
#' fuzzyOutput <- FuzzifyMatrix(iris[,1:4])
#'
#' head(fuzzyOutput)
#' 
#'
#' @export






# fuzzify data

FuzzifyMatrix <- function(crispMatrix,coreFactor=0.2,supportFactor=0.2,trapezoidal=TRUE,varNames=colnames(crispMatrix),...)
{
  
  if(is.data.frame(crispMatrix))
  {
    crispMatrix <- data.matrix(crispMatrix)
    
  } 
  
  varNumb <- ncol(crispMatrix)
  
  obsNumb <- nrow(crispMatrix)
  
  # prepare matrix
  
  newNames <- c()
  
  if(trapezoidal)
  {
    
    for (i in 1:varNumb) {
      
      newNames <- c(newNames,paste(varNames[i],".X", 1:4, sep=""))
      
    }
    
    output <- matrix(data = NA,nrow = nrow(crispMatrix),ncol = 4*varNumb)
    
    
  } else {
    
    for (i in 1:varNumb) {
      
      newNames <- c(newNames,paste(varNames[i],".X", 1:3, sep=""))
      
    }
    
    output <- matrix(data = NA,nrow = nrow(crispMatrix),ncol = 3*varNumb)
    
  }
  
  colnames(output) <- newNames
  
  # calculate sample statistics using columns
  
  sds <- apply(crispMatrix,MARGIN = 2,FUN = stats::sd)
  
  # generate fuzzified values
  
  if(trapezoidal)
  {
    
    for(i in 1:varNumb)
    {
      # start from the core
      
      ranNumbers <- stats::runif(n=obsNumb,min = 0,max=coreFactor*sds[i])
      
      output[,paste(varNames[i],".X2", sep="")] <- crispMatrix[,varNames[i]] - ranNumbers
      
      ranNumbers <- stats::runif(n=obsNumb,min = 0,max=coreFactor*sds[i])
      
      output[,paste(varNames[i],".X3", sep="")] <- crispMatrix[,varNames[i]] + ranNumbers
      
      # left end of the support
      
      ranNumbers <- stats::runif(n=obsNumb,min = 0,max=supportFactor*sds[i])
      
      output[,paste(varNames[i],".X1", sep="")] <- output[,paste(varNames[i],".X2", sep="")] - ranNumbers
      
      # right end of the support
      
      ranNumbers <- stats::runif(n=obsNumb,min = 0,max=supportFactor*sds[i])
      
      output[,paste(varNames[i],".X4", sep="")] <- output[,paste(varNames[i],".X3", sep="")] + ranNumbers
      
      
    }
    
    
    
  } else {
    
    for(i in 1:varNumb)
    {
      # start from the core
      
      output[,paste(varNames[i],".X2", sep="")] <- crispMatrix[,varNames[i]]
      
      # left end of the support
      
      ranNumbers <- stats::runif(n=obsNumb,min = 0,max=supportFactor*sds[i])
      
      output[,paste(varNames[i],".X1", sep="")] <- output[,paste(varNames[i],".X2", sep="")] - ranNumbers
      
      # right end of the support
      
      ranNumbers <- stats::runif(n=obsNumb,min = 0,max=supportFactor*sds[i])
      
      output[,paste(varNames[i],".X3", sep="")] <- output[,paste(varNames[i],".X2", sep="")] + ranNumbers
      
      
    }
    
    
  }
  
  output
  
  
}
