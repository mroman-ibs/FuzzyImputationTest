#' Removing values that are not fuzzy numbers.
#'
#' @description
#' `RemoveNotFuzzy` removes all values that are not proper fuzzy numbers and restores the previous ones.
#'
#' @details
#' The procedure checks all the values in the given matrix (or data frame) specified by \code{imputedData}
#' and if some of them are not proper fuzzy numbers (e.g., their cores are outside the supports), they are removed.
#' Instead of these erroneous values, the previous ones from the input matrix (a data frame, or a list) \code{trueData} are restored.
#' These matrices (or data frames) should consist of fuzzy numbers (triangular fuzzy numbers if \code{trapezoidal=FALSE} is set,
#'  or trapezoidal ones if the default \code{trapezoidal=TRUE} is used).
#' The output is given as a matrix where each row is related to fuzzy numbers (with 3 values for the triangular fuzzy numbers,
#' or 4 values in the case of trapezoidal ones) for the consecutive variables.
#' The input has to consist of fuzzy numbers of the same type (i.e., mixing triangular and trapezoidal fuzzy numbers is not allowed).
#'
#'
#' @return
#' The output is given as a matrix.
#'
#'
#'
#'
#'
#' @param trueData Name of the input matrix (data frame(or data frame)) that is used to restore erroneous fuzzy numbers.
#'
#'
#' @param imputedData Name of the input matrix (data frame) with fuzzy numbers to check their correctness.
#' 
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#' 
#' @param ... Additional parameters passed to other functions.
#'
#'
#' @examples
#'
#' # matrix with proper values of triangular fuzzy numbers
#' 
#' matrixOK <- matrix(c(1,2,3,7,10,12,8,10,11),ncol=3,byrow = TRUE)
#'
#' # matrix with the wrong third value of fuzzy triangular number (its core is greater than
#' # the left end of its support) 
#' 
#' matrixFalse <- matrix(c(1,2,3,7,10,12,8,20,11),ncol=3,byrow = TRUE)
#' 
#' # remove the third value and restore the previous one
#'
#' RemoveNotFuzzy(matrixOK,matrixFalse,trapezoidal = FALSE)
#' 
#'
#' @export




# function to remove nonFNs

RemoveNotFuzzy <- function(trueData,imputedData,trapezoidal=TRUE,...)
{
  # checking parameters
  
  if(!(is.data.frame(trueData) | is.matrix(trueData) | is.list(trueData)))
  {
    stop("Parameter trueData should be a data frame, a matrix or a list!")
  }
  
  
  if(!(is.data.frame(imputedData) | is.matrix(imputedData) ))
  {
    stop("Parameter imputedData should be a data frame or a matrix!")
  }
  
  if ((length(trapezoidal)!=1 || (is.na(trapezoidal)) || (!is.logical(trapezoidal))))
  {
    stop("Parameter trapezoidal should be a single logical value!")
  }
  
  # conversions
  
  if(is.data.frame(trueData))
  {
    trueData <- as.matrix(trueData)
    
  } 
  
  if(is.list(trueData) && !is.data.frame(trueData))
  {
    # conversion to matrix
    
    trueData <- FuzzyNumbersToMatrix(trueData,trapezoidal = trapezoidal,...)
    
  } 
  
  
  if(is.data.frame(imputedData))
  {
    imputedData <- as.matrix(imputedData)
    
  } 
  
  if(is.list(imputedData) && !is.data.frame(imputedData))
  {
    # conversion to matrix
    
    imputedData <- FuzzyNumbersToMatrix(imputedData,trapezoidal = trapezoidal,...)
    
  } 
  
  # checking parameters
  
  if (!is.numeric(trueData))
  {
    stop("Parameter trueData should have numerical values!")
  }
  
  
  if (!is.numeric(imputedData))
  {
    stop("Parameter imputedData should have numerical values!")
  }
  
  
  if (!((ncol(trueData) %% 4) == 0) & trapezoidal)
  {
    stop("For trapezoidal fuzzy numbers, the parameter trueData should have a multiple of 4 columns!")
  }
  
  
  if (!((ncol(imputedData) %% 4) == 0) & trapezoidal)
  {
    stop("For trapezoidal fuzzy numbers, the parameter imputedData should have a multiple of 4 columns!")
  }
  
  
  if (!((ncol(trueData) %% 3) == 0) & !trapezoidal)
  {
    stop("For triangular fuzzy numbers, the parameter trueData should have a multiple of 3 columns!")
  }
  
  
  if (!((ncol(imputedData) %% 3) == 0) & !trapezoidal)
  {
    stop("For triangular fuzzy numbers, the parameter imputedData should have a multiple of 3 columns!")
  }
  
  # number of all variables
  
  parameterTrapezoidal <- ifelse(trapezoidal,4,3)
  
  varNumber <- ncol(imputedData) / parameterTrapezoidal
  
  # matrix for results of checking
  
  checkMatrix <- imputedData
  
  for (i in 1:varNumber) {
    
    # seek range for each variable
    
    rangeToCheck <- c((parameterTrapezoidal*(i-1)+1):(parameterTrapezoidal*i))
    
    # cat("rangeToCheck: ", rangeToCheck, "\n")
    
    # use only this variable
    
    dataSingleVar <- imputedData[,rangeToCheck]
    
    # print(dataSingleVar)
    
    # rows for FNs
    
    rowsFNs <- apply(dataSingleVar,MARGIN = 1,FUN=IsFuzzy,trapezoidal=trapezoidal)
    
    # change nonFNs to their previous values
    
    checkMatrix[!rowsFNs,rangeToCheck] <- trueData[!rowsFNs,rangeToCheck]
    
  }
  
  return(checkMatrix)
  
  
}
