#' Battery of test for the imputed fuzzy values.
#'
#' @description
#' `ImputationTests` calculates various measures and applies goodness-of-fit statistical tests to
#' check the quality of the imputed fuzzy values.
#'
#' @details
#' The procedure uses other functions embedded in this package to check the quality of the imputed fuzzy values if they
#' are compared with the original ones.
#' This procedure calculates number of non-FNs for each variable, error matrix (using \code{ErrorMatrix}), various statistical measures 
#' (with \code{StatisticalMeasures}), applies epistemic goodness-of-fit tests (using \code{ApplyStatisticalTests}), and evaluates the
#' fuzzy measures (with \code{CalculateFuzzyMeasures}).
#' Therefore, this function can be directly applied as one-click benchmark tool.
#' 
#' To properly distinguish the real values with their imputed counterparts, the additional matrix \code{imputedMask} should be provided.
#' In this matrix, the logical value \code{TRUE} points out the cells with the imputed values.
#' Otherwise, \code{FALSE} should be used.
#' 
#' All of the input datasets can be given as matrices or data frames.
#' 
#'
#' @return
#' The output is given as a list of the matrices: \code{trueValues} - the true, input values (the same as \code{trueData}),
#' \code{mask} - the masked (NAs) values (the same as \code{imputedMask}),
#' \code{nonFNNumbers} - the vector with the numbers of non-FNs samples for each variable (with the overall mean),
#' \code{errorMatrix} -- the output from the function \code{ErrorMatrix},
#' \code{statisticalMeasures} -- the output from the function \code{StatisticalMeasures}, 
#'  \code{statisticalTests} -- the output from the function \code{ApplyStatisticalTests}, 
#' \code{fuzzyMeasures} -- the output from the function \code{CalculateFuzzyMeasures}.
#'
#'
#'
#'
#' @param trueData Name of the input matrix (or data frame, or list) with the true values of the variables.
#'
#'
#' @param imputedData Name of the input matrix (or data frame) with the imputed values.
#' 
#' @param imputedMask Matrix (or data frame) with logical values where \code{TRUE} indicates the cells with the imputed values.
#' 
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#' 
#' @param cutsNumber Number of cuts for the epistemic bootstrap tests.
#' 
#' @param K Value of \code{K} for the \code{res} epistemic test.
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
#' # check the quality of the imputed values
#' 
#' ImputationTests(matrix1,matrix1DImp,matrix1Mask,trapezoidal=TRUE)
#'
#' 
#'
#' @export




# function to apply tests after imputation

ImputationTests <- function(trueData,imputedData,imputedMask,trapezoidal=TRUE,
                            cutsNumber=100,K=50,...)
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
  
  if(is.data.frame(imputedData))
  {
    imputedData <- as.matrix(imputedData)
    
  } 
  
  if(is.data.frame(imputedMask))
  {
    imputedMask <- as.matrix(imputedMask)
    
  } 
  
  if(is.list(trueData) && !is.data.frame(trueData))
  {
    # conversion to matrix
    
    trueData <- FuzzyNumbersToMatrix(trueData,trapezoidal = trapezoidal,...)
    
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
    stop("The parameters trueData, imputedData, imputedMask should have the same number of columns!")
  }
  
  variableNumber <- ncol(trueData)
  
  obsNumber <- nrow(trueData)
  
  # which rows are imputed?
  
  rowsNumbersImputed <- which(apply(imputedMask,1,any))
  
  
  # how many non-FN values in outputs?
  
  nonFNNumbers <- NumberOfNonFNs(imputedData[rowsNumbersImputed,],trapezoidal = trapezoidal)
  
  # calculate the error matrix
  
  errorMatrix <- ErrorMatrix(trueData,imputedData,imputedMask,trapezoidal=trapezoidal,...)
  
  # to do
  
  # calculate statistical measures
  
  statisticalMeasures <- StatisticalMeasures(trueData,imputedData,imputedMask,...)

  # apply tests
  
  statisticalTests <- ApplyStatisticalTests(trueData,imputedData,imputedMask,trapezoidal=trapezoidal,
                                            cutsNumber=cutsNumber,K=K,...)
  
  # calculate distances
  
  # print("Calculating fuzzy distances for the output..")
  
  fuzzyMeasures <- CalculateFuzzyMeasures(trueData,imputedData,imputedMask,
                                          trapezoidal=trapezoidal,...)
  
  
  

  return(list(trueValues=trueData,
              mask=imputedMask,
              nonFNNumbers=nonFNNumbers,
              errorMatrix=errorMatrix,
              statisticalMeasures=statisticalMeasures,
              statisticalTests=statisticalTests,
              fuzzyMeasures=fuzzyMeasures))
  
  
}



