#' Main method to impute fuzzy values.
#'
#' @description
#' `FuzzyImputation` imputes (i.e., replaces missing values) fuzzy numbers using various methods.
#'
#' @details
#' The procedure randomly imputes missing values (NAs) with suitable data in the case of a data frame (or a matrix, or a list)
#' consisting of fuzzy numbers (triangular fuzzy numbers if \code{trapezoidal=FALSE} is set, or trapezoidal ones if the default
#' \code{trapezoidal=TRUE} is used).
#' The output is given as a matrix without NAs, where each row is related to fuzzy numbers (given by 3 values for the triangular fuzzy numbers,
#' or 4 values in the case of trapezoidal ones) for the consecutive variables.
#' Many fuzzy variables (not only the single one) can be used. 
#' The input has to consist of fuzzy numbers of the same type (i.e., mixing triangular and trapezoidal fuzzy numbers is not allowed).
#' 
#' Various possible imputation methods can be used when the parameter \code{method} is specified --
#'  both the general ones (\code{missForest} or \code{miceRanger} from the respective packages, or \code{knn} from
#'  \code{VIM} package) and a more specific ones, tailored for the fuzzy data (\code{dimp} in the case of the DIMP method).
#' Please note that due to the imputation, some output values can be improper fuzzy variables 
#' (e.g., a core of a fuzzy number can have greater value than its right end of the support).
#' To avoid this, \code{checkFuzzy=TRUE} should be set.
#' In this case, the imputation procedure is repeated until all of the results are proper triangular or trapezoidal fuzzy numbers.
#' The improper values are removed and replaced with the respective fuzzy numbers from the input dataset.
#' However, many repetitions (even unacceptably many) are then possible.
#'
#'
#' @return
#' The output is given as a matrix.
#'
#'
#'
#'
#'
#' @param dataSet Name of the input matrix (data frame or list) of fuzzy numbers with some NAs.
#' 
#' @param method Name of the imputation method (possible values: \code{dimp,missForest,miceRanger,knn}).
#'
#'
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#' 
#' @param checkFuzzy If \code{TRUE} is set, after each imputation, the output values are checked if they are proper fuzzy numbers.
#' If there are some improper fuzzy numbers, they are removed, and the imputation procedure is repeated.
#' 
#' @param ... Additional parameters that are passed to the imputation procedure.
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
#' # impute missing values with the DIMP method
#' 
#' set.seed(12345)
#' 
#' FuzzyImputation(matrix1NA)
#' 
#' # impute missing values with the miceRanger method
#' 
#' set.seed(12345)
#' 
#' FuzzyImputation(matrix1NA,method = "miceRanger")
#'
#'
#' @export



# main method to impute values

FuzzyImputation <- function(dataSet,method="dimp",trapezoidal=TRUE,checkFuzzy=FALSE,...)
{
  # conversions
  
  if(is.data.frame(dataSet))
  {
    fuzzyMatrix <- data.matrix(dataSet)
    
  } 
  
  if(is.list(dataSet) && !is.data.frame(dataSet))
  {
    # conversion to matrix
    
    fuzzyMatrix <- FuzzyNumbersToMatrix(dataSet,trapezoidal = trapezoidal,...)
    
  } else {
  
    
    fuzzyMatrix <- dataSet
    
  }
  
  # condition to stop
  
  stopImputation = TRUE
  
  i <- 1
  
  repeat {
    
    cat("Iteration: ", i, "\n")
    
    i <- i+1
    
    # use dimp
    
    if(method=="dimp") {
      
      outputMatrix <- ImputationDimp(fuzzyMatrix,trapezoidal = trapezoidal,...)
      
    }
    
    # use fknn
    
    if(method=="fknn") {
      
      outputMatrix <- ImputationkFkNNMethod(fuzzyMatrix,trapezoidal = trapezoidal,...)
      
    }
    
    # use missForest
    
    if(method=="missForest") {
      
      outputMatrix <- missForest::missForest(fuzzyMatrix,...)$ximp
      
    }
    
    
    # use missForest
    
    if(method=="miceRanger") {
      
      objMR <- miceRanger::miceRanger(data.frame(fuzzyMatrix),m=1,verbose=FALSE,...)
      
      outputMatrix <- data.matrix(miceRanger::completeData(objMR)[[1]])
        
        
    }
    
    # use knn from VIM
    
    if(method=="knn") {
      
      outputMatrix <- VIM::kNN(fuzzyMatrix,imp_var = FALSE,...)
      
      
    }
    
    
    # now we remove nonFNs if necessary
    
    if(checkFuzzy) {
      
      outputMatrix <- RemoveNotFuzzy(trueData=fuzzyMatrix,imputedData=outputMatrix,trapezoidal = trapezoidal)
      
      # are there some NAs still?
      
      if(any(is.na(outputMatrix))) 
      {
        
        stopImputation=FALSE
        
      } else {
        
        stopImputation=TRUE
        
      }
      
    }
    
    if(stopImputation)
    {
      break
    }
    
  }
  
  return(outputMatrix)
  
}
