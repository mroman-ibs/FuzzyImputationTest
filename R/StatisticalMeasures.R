#' Calculation of statistical measures for errors of the imputed data.
#'
#' @description
#' `StatisticalMeasures` calculates various statistical measures between the real and imputed data.
#'
#' @details
#' The procedure calculates different statistical measures between the real and
#' imputed data for each column, namely:
#' * TrueMean - the mean only for the real but missing data,
#' * ImpMean - the mean only for the imputed values,
#' * TrueSD - the standard deviation only for the real but missing data,
#' * ImpSD - the standard deviation only for the imputed values,
#' * GenMean - the mean for the all real data (given by \code{trueData}),
#' * GenImpMean - the mean for real data with the respectively imputed values (given by \code{imputedData}),
#' * GenSD - the standard deviation for the all real data (given by \code{trueData}),
#' * GenImpSD - the standard deviation for real data with the respectively imputed values (given by \code{imputedData}),
#' * AbsDiffTrueImpMean - the absolute difference between TrueMean and ImpMean,
#' * AbsDiffTrueImpSD - the absolute difference between TrueSD and ImSD,
#' * AbsDiffGenImpMean - the absolute difference between GenMean and GenImpMean,
#' * AbsDiffGenImpSD - the absolute difference between GenSD and GenImpSD.
#'
#' To properly distinguish the real values with their imputed counterparts, the additional matrix \code{imputedMask} should be provided.
#' In this matrix, the logical value \code{TRUE} points out the cells with the imputed values.
#' Otherwise, \code{FALSE} should be used. These input datasets should be given as matrices
#' or data frames.
#' 
#' 
#' 
#' 
#' @md
#' 
#' 
#'
#' @return
#' The output is given as a matrix.
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
#' StatisticalMeasures(matrix1,matrix1DImp,matrix1Mask)
#'
#'
#' @export




StatisticalMeasures <- function(trueData,imputedData,imputedMask,...)
{
  
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
  
  variableNumber <- ncol(trueData)
  
  obsNumber <- nrow(trueData)
  
  # matrix for output measures
  
  
  output <- matrix(data = 0,nrow = length(measuresTypes), ncol = variableNumber)
  
  rownames(output) <- measuresTypes
  
  # adding names to variables
  
  if(is.null(colnames(trueData))==TRUE) {
    
    colnames(output) <- noquote(paste("X", 1:variableNumber, sep=""))
    
  } else {
    
    colnames(output) <- colnames(trueData)
    
  }
    
  
  
  
  # for loop for all variables
  
  for (i in 1:variableNumber) {
    
    # which rows are with some NAs?
    
    rowsWithNA <- which(imputedMask[,i])
    
    
    # measures only for NA/imputed data
    
    output["TrueMean",i] <- mean(trueData[rowsWithNA,i])
    
    output["ImpMean",i] <- mean(imputedData[rowsWithNA,i])
    
    output["TrueSD",i] <- stats::sd(trueData[rowsWithNA,i])
    
    output["ImpSD",i] <- stats::sd(imputedData[rowsWithNA,i])
    
  }
  
  # measures for all data
  
  output["GenMean",] <- apply(trueData,MARGIN = 2,FUN = mean)
  
  output["GenImpMean",] <- apply(imputedData,MARGIN = 2,FUN = mean)
  
  output["GenSD",] <- apply(trueData,MARGIN = 2,FUN = stats::sd)
  
  output["GenImpSD",] <- apply(imputedData,MARGIN = 2,FUN = stats::sd)
  
  # some differences
  
  output["AbsDiffTrueImpMean",] <- abs(output["TrueMean",]-output["ImpMean",])
  
  output["AbsDiffGenImpMean",] <- abs(output["GenMean",]-output["GenImpMean",])
  
  output["AbsDiffTrueImpSD",] <- abs(output["TrueSD",]-output["ImpSD",])
  
  output["AbsDiffGenImpSD",] <- abs(output["GenSD",]-output["GenImpSD",])
  
  
  return(output)
  
  
  
}
