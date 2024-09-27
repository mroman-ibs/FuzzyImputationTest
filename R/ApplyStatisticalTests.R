#' Statistical epistemic tests the imputed values.
#'
#' @description
#' `ApplyStatisticalTests` applies the epistemic goodness-of-fit test for
#' fuzzy data to check the quality of the imputed values.
#'
#' @details
#' The procedure applies three types of the epistemic goodness-of-fit Kolmogorov-Smirnov tests (\code{avs} - averaging statistic, 
#' \code{ms} - multi-statistic, \code{res} - resampling algorithm) from the \code{FuzzySimRes}
#' package to check the quality of the imputed values.
#' To do this, three subsamples are used:
#' * \code{true} - the dataset \code{trueData} without imputed values vs the values from the same dataset that are then imputed,
#' * \code{imputed} - the dataset \code{trueData} without imputed values vs only the imputed values from \code{imputedData},
#' * \code{parts} - only the imputed values from the dataset \code{trueData} vs their counterparts from \code{imputedData}.
#' 
#' To assess the respective imputation quality, p-values for \code{true} and \code{imputed} should be close to each other,
#' and in the case of \code{parts}, they should exceed the selected significance level.
#' 
#'  
#' 
#' All of the input datasets can be given as matrices or data frames.
#' The statistical tests are performed only for the input values that are proper fuzzy numbers (triangular or trapezoidal ones).
#' 
#' @md
#' 
#'
#' @return
#' The output is given as a matrix (the rows are related to various types of the test and subsamples, the columns - to the variables plus the overall mean).
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
#' @param cutsNumber Number of cuts for the epistemic bootstrap tests.
#' 
#' @param K Value of \code{K} for the \code{res} epistemic test.
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
#' # impute missing values (with possible repetitions!)
#' 
#' matrix1DImp <- FuzzyImputation(matrix1NA,method="dimp",checkFuzzy=TRUE)
#' 
#' # find cells with NAs
#' 
#' matrix1Mask <- is.na(matrix1NA)
#' 
#' # apply statistical epistemic bootstrap tests
#' 
#' ApplyStatisticalTests(matrix1,matrix1DImp,matrix1Mask,cutsNumber = 100, K=10)
#'
#' 
#'
#' @export



# main function to perform statistical tests

ApplyStatisticalTests <- function(trueData,imputedData,imputedMask,trapezoidal=TRUE,cutsNumber=100,K=50,...)
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
  
  
  
  # number of all variables
  
  parameterTrapezoidal <- ifelse(trapezoidal,4,3)
  
  varNumber <- ncol(trueData) / parameterTrapezoidal
  
  # cat("varNumber: ", varNumber, "\n")
  
  # output matrix
  
  output <- matrix(NA,ncol = varNumber+1, nrow = length(setsNames)*length(testsNames))
  
  rownames(output) <- noquote(paste(rep(setsNames,each=3),testsNames,sep="+"))
  
  colnames(output) <- c(noquote(paste("V", 1:varNumber, sep="")),"mean")
  
  
  
  # for loop for all variables
  
  for (i in 1:varNumber) {
    
    # cat("i: ", i, "\n")
    
    # find the right range for each variable
    
    rangeToCheck <- c((parameterTrapezoidal*(i-1)+1):(parameterTrapezoidal*i))
    
    # cat("rangeToCheck: ", rangeToCheck, "\n")
    
    # select only this variable for calculation of the measures
    
    outputSingleVar <- ApplyStatisticalTestsSingleVar(trueData[,rangeToCheck],
                                                       imputedData[,rangeToCheck],
                                                       imputedMask[,rangeToCheck],
                                                      trapezoidal = trapezoidal,
                                                      cutsNumber = cutsNumber,
                                                      K=K)
    
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