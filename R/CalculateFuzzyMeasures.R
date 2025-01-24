#' Calculation of the fuzzy measures for the imputed values.
#'
#' @description
#' `CalculateFuzzyMeasures` calculates the various types of fuzzy measures between two datasets -- the true and the imputed one.
#'
#' @details
#' The procedure calculates different types of the distance measures (Euclidean - the Euclidean measure,
#' AHD - the AHD measure, HSD - the HSD measure), and means of the absolute differences for various fuzzy characteristics
#' (DiffVal - the difference of the value, DiffAmb - the ambiguity, DiffEV - the expected value, DiffWidth - the width)
#' between two datasets - the first one with true values (set by \code{trueData}), and the second one (specified by \code{imputedData}) with
#' the imputed variables.
#' Only the truly imputed values are taken into account for these calculations.
#' To properly distinguish the real values with their imputed counterparts, the additional matrix \code{imputedMask} should be provided.
#' In this matrix, the logical value \code{TRUE} points out the cells with the imputed values.
#' Otherwise, \code{FALSE} should be used.
#' 
#' All of the input datasets can be given as matrices or data frames.
#' 
#'
#' @return
#' The output is given as a matrix (the rows are related to various types of the errors, the columns - to the variables and the overall mean).
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
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
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
#' # calculate fuzzy measures for the imputed values
#' 
#' CalculateFuzzyMeasures(matrix1,matrix1DImp,matrix1Mask,trapezoidal=TRUE)
#'
#' 
#'
#' @export



CalculateFuzzyMeasures <- function(trueData,imputedData,imputedMask,trapezoidal=TRUE,...)
{
  # checking parameters
  
  if(!(is.data.frame(trueData) | is.matrix(trueData) | is.list(trueData)))
  {
    stop("Parameter trueData should be a data frame, a matrix or a list!")
  }
  
  if(!(is.data.frame(imputedData) | is.matrix(imputedData)))
  {
    stop("Parameter imputedData should be a data frame or a matrix!")
  }
  
  
  if(!(is.data.frame(imputedMask) | is.matrix(imputedMask)))
  {
    stop("Parameter imputedMask should be a data frame or a matrix!")
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
  
  
  if(is.data.frame(imputedMask))
  {
    imputedMask <- as.matrix(imputedMask)
    
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
    stop("The parameters trueData, imputedData, imputedMask should have the same number of rows!")
  }
  
  # number of all variables
  
  parameterTrapezoidal <- ifelse(trapezoidal,4,3)
  
  varNumber <- ncol(trueData) / parameterTrapezoidal
  
  # cat("varNumber: ", varNumber, "\n")
  
  # output matrix
  
  output <- matrix(NA,ncol = varNumber+1, nrow = length(distanceNames))
  
  rownames(output) <- distanceNames
  
  colnames(output) <- c(noquote(paste("V", 1:varNumber, sep="")),"mean")
  
  
  
  # for loop for all variables
  
  for (i in 1:varNumber) {
    
    # cat("i: ", i, "\n")
    
    # find the right range for each variable
    
    rangeToCheck <- c((parameterTrapezoidal*(i-1)+1):(parameterTrapezoidal*i))
    
    # cat("rangeToCheck: ", rangeToCheck, "\n")
    
    # select only this variable for calculation of the measures
    
    outputSingleVar <- CalculateFuzzyMeasuresSingleVar(trueData[,rangeToCheck],
                                                       imputedData[,rangeToCheck],
                                                       imputedMask[,rangeToCheck],
                                                       trapezoidal=trapezoidal)
    
    # input to the output matrix
    
    output[,i] <- outputSingleVar
    
  }
  
  if(varNumber==1)
  {
    
    output[,"mean"] <- output[,1]
    
  } else {
    
    output[,"mean"] <- apply(output[,c(1:varNumber)],MARGIN=1,FUN=mean)
    
  }
  
  
  
  return(output)
  
  
}

  
