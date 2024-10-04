#' Calculation of the errors for the imputed values.
#'
#' @description
#' `ErrorMatrix` calculates the various types of the errors between two datasets -- the true and the imputed one.
#'
#' @details
#' The procedure calculates different types of the errors (MAE - the mean absolute error, WMA - the weighted mean absolute error,
#' MSE - the mean squared error, WMSE - the weighted mean squared error, NRMSE - the normalized root mean squared error)
#' between two datasets - the first one with true values (set by \code{trueData}), and the second one (specified by \code{imputedData}) with
#' the imputed variables.
#' To properly distinguish the real values with their imputed counterparts, the additional matrix \code{imputedMask} should be provided.
#' In this matrix, the logical value \code{TRUE} points out the cells with the imputed values.
#' Otherwise, \code{FALSE} should be used.
#' 
#' All of the input datasets can be given as matrices or data frames.
#' 
#'
#' @return
#' The output is given as a matrix (the rows are related to various types of the errors, the columns - to the variables).
#'
#'
#'
#'
#'
#' @param trueData Name of the input matrix (or data frame) with the true values of the variables.
#'
#'
#' @param imputedData Name of the input matrix (or data frame) with the imputed values.
#' 
#' @param imputedMask Matrix (or data frame) with logical values where \code{TRUE} indicates the cells with the imputed values.
#' 
#' @param ... Additional parameters passed to other functions.
#'
#'
#' @examples
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # load the necessary library
#' 
#' library(FuzzySimRes)
#' 
#' # generate sample of trapezoidal fuzzy numbers with FuzzySimRes library
#'
#' list1<-SimulateSample(20,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
#' incrCorePD="rexp", parIncrCorePD=list(rate=2),
#' suppLeftPD="runif",parSuppLeftPD=list(min=0,max=0.6),
#' suppRightPD="runif", parSuppRightPD=list(min=0,max=0.6),
#' type="trapezoidal")
#' 
#' # convert fuzzy data into a matrix
#' 
#' matrix1 <- FuzzyNumbersToMatrix(list1$value)
#' 
#' # check starting values
#' 
#' head(matrix1)
#' 
#' # add some NAs to the matrix
#' 
#' matrix1NA <- IntroducingNA(matrix1,percentage = 0.1)
#' 
#' head(matrix1NA)
#' 
#' # impute missing values
#' 
#' matrix1DImp <- ImputationDimp(matrix1NA)
#' 
#' # find cells with NAs
#' 
#' matrix1Mask <- is.na(matrix1NA)
#' 
#' # calculate errors for the imputed values
#' 
#' ErrorMatrix(matrix1,matrix1DImp,matrix1Mask)
#'
#' 
#'
#' @export



ErrorMatrix <- function(trueData,imputedData,imputedMask,...)
{
  
  # checking parameters
  
  if(!(is.data.frame(trueData) | is.matrix(trueData)))
  {
    stop("Parameter trueData should be a data frame or a matrix!")
  }
  
  
  if(!(is.data.frame(imputedData) | is.matrix(imputedData)))
  {
    stop("Parameter imputedData should be a data frame or a matrix!")
  }
  
  
  if(!(is.data.frame(imputedMask) | is.matrix(imputedMask)))
  {
    stop("Parameter imputedMask should be a data frame or a matrix!")
  }
  
  
  # conversions
  
  if(is.data.frame(trueData))
  {
    trueData <- data.matrix(trueData)
    
  } 
  
  if(is.data.frame(imputedData))
  {
    imputedData <- data.matrix(imputedData)
    
  } 
  
  if(is.data.frame(imputedMask))
  {
    imputedMask <- data.matrix(imputedMask)
    
  } 
  
  
  if (!is.numeric(trueData))
  {
    stop("Parameter trueData should have numerical values!")
  }
  
  if (!is.numeric(imputedData))
  {
    stop("Parameter imputedData should have numerical values!")
  }
  
  if (!is.logical(imputedMask))
  {
    stop("Parameter imputedMask should have logical values!")
  }
  
  if(!(ncol(trueData) == ncol(imputedData)) & !(ncol(imputedData) == ncol(imputedMask)))
  {
    stop("The parameters trueData, imputedData, imputedMask should have the same number of columns!")
  }
  
  
  if(!(nrow(trueData) == nrow(imputedData)) & !(nrow(imputedData) == nrow(imputedMask)))
  {
    stop("The parameters trueData, imputedData, imputedMask should have the same number of columns!")
  }
  
  variableNumber <- ncol(trueData)
  
  obsNumber <- nrow(trueData)
  
  
  # matrix for errors
  
  output <- matrix(data = 0,nrow = length(errorTypes), ncol = variableNumber+1)
  
  rownames(output) <- errorTypes
  
  if(is.null(colnames(trueData))==TRUE) {
    
    colnames(output) <- c(noquote(paste("X", 1:variableNumber, sep="")),"mean")
    
  } else {
    
    colnames(output) <- c(colnames(trueData),"mean")
    
  }
  
  
  
  # main loop
  
  for (i in 1:variableNumber) {
    
    # which rows are with some NAs?
    
    rowsWithNA <- which(imputedMask[,i])
    
    # let's calculate errors
    
    output["MAE",i] <- mean(abs(trueData[rowsWithNA,i]-imputedData[rowsWithNA,i]))
    
    output["WMAE",i] <- mean(abs((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])/
                                         ChangeDenominator(trueData[rowsWithNA,i])))
    
    output["MSE",i] <- mean((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])^2)
    
    output["WMSE",i] <- mean(((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])/
                                      ChangeDenominator(trueData[rowsWithNA,i]))^2)
    
    output["NRMSE",i] <- sqrt(mean((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])^2)/(max(trueData[,i])-min(trueData[,i])))
    
  }
  
  # calculate means for errors
  
  output[,variableNumber+1] <- apply(output[,-(variableNumber+1)],1,mean)
  
  
  return(output)
  
  
}











# auxiliary function if the denominator is equal to zero

ChangeDenominator <- function(inputVector)
{
  whereToChange <- (inputVector==0)
  
  output <- inputVector
  
  output[whereToChange] <- 1
  
  return(output)
  
}
