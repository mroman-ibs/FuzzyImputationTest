#' Introducing NAs to the specified matrix.
#'
#' @description
#' `IntroducingNA` introduces missing values (NAs) to the specified data matrix. 
#'
#' @details
#' The procedure changes randomly some values in the specified matrix to "missing values" (denoted by NA).
#' Number of these missing values in each row is given by the parameter \code{percentage}.
#' If the input is a list of fuzzy numbers or the data frame, then it is automatically converted to a matrix.
#'
#'
#' @return
#' The output is given as a matrix.
#'
#'
#'
#'
#'
#' @param dataMatrix Name of the input matrix.
#'
#'
#' @param percentage Desired percentage of missing values (NAs) in each row.
#'
#'
#' @examples
#'
#' 
#' 
#' # prepare matrix with 3 columns and 3 rows
#'
#' matrix1 <- matrix(c(1,3,5,2,5,7,1,4,5),ncol=3,byrow = TRUE)
#' 
#' # add 1 NA in each row
#' 
#' set.seed(12345)
#'
#' IntroducingNA(matrix1,percentage = 0.33)
#'
#'
#' @export


# function to impute NAs

IntroducingNA <- function(dataMatrix,percentage=0.05)
{
  
  # conversions
  
  if(is.data.frame(dataMatrix))
  {
    dataMatrix <- data.matrix(dataMatrix)
    
  } 
  
  if(is.list(dataMatrix) && !is.data.frame(dataMatrix))
  {
    # conversion to matrix
    
    dataMatrix <- FuzzyNumbersToMatrix(dataMatrix,trapezoidal = trapezoidal,...)
    
  } else {
    
    
    dataMatrix <- dataMatrix
    
  }
  
  variableNumber <- ncol(dataMatrix)
  
  obsNumber <- nrow(dataMatrix)
  
  # matrix for NAs
  
  matrixFuzzyWithMask <- dataMatrix
  
  
  # print("Adding NAs to data...")
  
  # loop for variables
  
  for (i in 1:variableNumber) {
    
    # sample rows to put NAs
    
    rowsToImputation <- sample(obsNumber,ceiling(obsNumber*percentage))
    
    # we put NA for selected places
    
    matrixFuzzyWithMask[rowsToImputation,i] <- NA
    
  }
  
  return(matrixFuzzyWithMask)
  
}
