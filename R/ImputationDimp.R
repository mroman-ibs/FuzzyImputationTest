#' DIMP (d-imputation) method for fuzzy numbers.
#'
#' @description
#' `ImputationDimp` imputes (i.e., replaces missing values) fuzzy numbers using DIMP (d-imputation) method.
#'
#' @details
#' The procedure randomly imputes missing values (NAs) with suitable data in the case of the dataset (or matrix, or list)
#' consisting of fuzzy numbers (triangular fuzzy numbers if \code{trapezoidal=FALSE} is set, or trapezoidal if the default
#' \code{trapezoidal=TRUE} is used).
#' The output is given as a matrix without NAs, where each row is related to fuzzy numbers (with 3 values for the triangular fuzzy numbers,
#' or 4 values in the case of trapezoidal ones) for the consecutive variables.
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
#' @param dataToImpute Name of the input matrix (data frame or a list) of fuzzy numbers with some NAs.
#'
#'
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#'
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
#' # convert fuzzy data into matrix
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
  
  # conversions
  
  if(is.data.frame(dataToImpute))
  {
    dataToImpute <- data.matrix(dataToImpute)
    
  } 
  
  if(is.list(dataToImpute) && !is.data.frame(dataToImpute))
  {
    # conversion to matrix
    
    dataToImpute <- FuzzyNumbersToMatrix(dataToImpute,trapezoidal = trapezoidal,...)
    
  } else {
    
    
    dataToImpute <- dataToImpute
    
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
    
    # wybieramy tylko tą zmienną
    
    dataSingleVar <- dataToImpute[,rangeToCheck]
    
    # cat("dataSingleVar przed imputacją: \n")
    
    # print(dataSingleVar)
    
    dataSingleVar <- ImputationDimpSingleVar(dataToImpute = dataSingleVar)
    
    # cat("dataSingleVar po imputacji: \n")
    
    # print(dataSingleVar)
    
    # wstawiamy do macierzy wyjściowej
    
    output[,rangeToCheck] <- dataSingleVar
    
  }
  
  return(output)
  
  
}
