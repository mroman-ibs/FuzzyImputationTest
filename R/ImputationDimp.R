#' DIMP (d-imputation) method for fuzzy numbers.
#'
#' @description
#' `ImputationDimp` imputes (i.e., replaces missing values) fuzzy numbers using the DIMP (d-imputation) method.
#'
#' @details
#' The procedure randomly imputes missing values (NAs) with suitable data in the case of the dataset (or matrix, or list)
#' consisting of fuzzy numbers (triangular fuzzy numbers if \code{trapezoidal=FALSE} is set, or trapezoidal if the default
#' \code{trapezoidal=TRUE} is used).
#' The output is given as a matrix without NAs, where each row is related to fuzzy numbers (given by 3 values for the triangular fuzzy numbers,
#' or 4 values in the case of trapezoidal ones) for the consecutive variables.
#' Many fuzzy variables (not the only one) can be used. 
#' The input has to consist of fuzzy numbers of the same types (i.e., mixing triangular and trapezoidal fuzzy numbers is not allowed).
#'
#'
#' @return
#' The output is given as a matrix.
#'
#'
#'
#'
#'
#' @param dataToImpute Name of the input matrix (data frame or list) of fuzzy numbers with some NAs.
#'
#'
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#' 
#' @param ... Additional parameters passed to other functions
#'
#' @references
#'
#' M. Romaniuk, P. Grzegorzewski, “Fuzzy data imputation with DIMP and FGAIN",
#' Journal of Computational Science, vol. 93, pp. 102738, 2026
#'
#' @examples
#'
#' 
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
#' ImputationDimp(matrix1NA)
#'
#'
#' @export




# DIMP method for many variables

ImputationDimp <- function(dataToImpute,trapezoidal=TRUE,...)
{
  
  # checking parameters
  
  if(!(is.data.frame(dataToImpute) | is.matrix(dataToImpute) | is.list(dataToImpute)))
  {
    stop("Parameter dataToImpute should be a data frame, a matrix or a list!")
  }
  
  if ((length(trapezoidal)!=1 || (is.na(trapezoidal)) || (!is.logical(trapezoidal))))
  {
    stop("Parameter trapezoidal should be a single logical value!")
  }
  
  # conversions
  
  if(is.data.frame(dataToImpute))
  {
    dataToImpute <- as.matrix(dataToImpute)
    
  } 
  
  if(is.list(dataToImpute) && !is.data.frame(dataToImpute))
  {
    # conversion to matrix
    
    dataToImpute <- FuzzyNumbersToMatrix(dataToImpute,trapezoidal = trapezoidal,...)
    
  } 
  
  
  if (!is.numeric(dataToImpute))
  {
    stop("Parameter dataToImpute should have numerical values!")
  }
  
  
  if (!((ncol(dataToImpute) %% 4) == 0) & trapezoidal)
  {
    stop("For trapezoidal fuzzy numbers, the parameter dataToImpute should have a multiple of 4 columns!")
  }
  
  
  if (!((ncol(dataToImpute) %% 3) == 0) & !trapezoidal)
  {
    stop("For triangular fuzzy numbers, the parameter dataToImpute should have a multiple of 3 columns!")
  }
  
  # number of all variables
  
  parameterTrapezoidal <- ifelse(trapezoidal,4,3)
  
  varNumber <- ncol(dataToImpute) / parameterTrapezoidal
  
  # cat("varNumber: ", varNumber, "\n")
  
  output <- dataToImpute
  
  # for loop for all variables
  
  for (i in 1:varNumber) {
    
    # cat("i: ", i, "\n")
    
    # find the right range for each variable
    
    rangeToCheck <- c((parameterTrapezoidal*(i-1)+1):(parameterTrapezoidal*i))
    
    # cat("rangeToCheck: ", rangeToCheck, "\n")
    
    # select only this variable
    
    dataSingleVar <- dataToImpute[,rangeToCheck]
    
    # cat("dataSingleVar przed imputacją: \n")
    
    # print(dataSingleVar)
    
    dataSingleVar <- ImputationDimpSingleVar(dataToImpute = dataSingleVar)
    
    # cat("dataSingleVar po imputacji: \n")
    
    # print(dataSingleVar)
    
    # input to the output matrix
    
    output[,rangeToCheck] <- dataSingleVar
    
  }
  
  return(output)
  
  
}
