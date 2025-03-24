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
#'  \code{VIM} package, or \code{pmm} from \code{mice} package) and a more specific ones, tailored for the fuzzy data (\code{dimp} in the case of the DIMP method).
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
#' @param dataToImpute Name of the input matrix (data frame or list) of fuzzy numbers with some NAs.
#' 
#' @param method Name of the imputation method (possible values: \code{dimp,missForest,miceRanger,knn,pmm}).
#'
#'
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#' 
#' @param checkFuzzy If \code{TRUE} is set, after each imputation, the output values are checked if they are proper fuzzy numbers.
#' If there are some improper fuzzy numbers, they are removed, and the imputation procedure is repeated.
#' 
#' @param verbose If \code{TRUE} is set, the current simulation number is printed.
#' 
#' @param pmmWarnings Suppress warnings from \code{pmm} method.
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

FuzzyImputation <- function(dataToImpute,method="dimp",trapezoidal=TRUE,checkFuzzy=FALSE,verbose=TRUE,pmmWarnings=TRUE,...)
{
  # checking parameters
  
  if(!(is.data.frame(dataToImpute) | is.matrix(dataToImpute) | is.list(dataToImpute)))
  {
    stop("Parameter dataToImpute should be a data frame or a matrix or a list!")
  }
  
  if(!(method %in% methodNames))
  {
    stop("Parameter method should be a proper name of the imputation method - check the values of methodNames")
  }
  
  
  if ((length(trapezoidal)!=1 || (is.na(trapezoidal)) || (!is.logical(trapezoidal))))
  {
    stop("Parameter trapezoidal should be a single logical value!")
  }
  
  if ((length(checkFuzzy)!=1 || (is.na(checkFuzzy)) || (!is.logical(checkFuzzy))))
  {
    stop("Parameter checkFuzzy should be a single logical value!")
  }
  
  if ((length(verbose)!=1 || (is.na(verbose)) || (!is.logical(verbose))))
  {
    stop("Parameter verbose should be a single logical value!")
  }
  
  # conversions
  
  if(is.data.frame(dataToImpute))
  {
    dataToImpute <- data.matrix(dataToImpute)
    
  } 
  
  if(is.list(dataToImpute) && !is.data.frame(dataToImpute))
  {
    # conversion to matrix
    
    dataToImpute <- FuzzyNumbersToMatrix(dataToImpute,trapezoidal = trapezoidal,...)
    
  } 
  
  # checking parameters
  
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
  
  # condition to stop
  
  stopImputation = TRUE
  
  i <- 1
  
  repeat {
    
    if(verbose)
    {
      cat("Iteration: ", i, "\n")
    }
    
    
    
    i <- i+1
    
    # use dimp
    
    if(method=="dimp") {
      
      outputMatrix <- ImputationDimp(dataToImpute,trapezoidal = trapezoidal,...)
      
    }
    
    
    
    # use missForest
    
    if(method=="missForest") {
      
      outputMatrix <- missForest::missForest(dataToImpute,...)$ximp
      
    }
    
    
    # use missForest
    
    if(method=="miceRanger") {
      
      objMR <- miceRanger::miceRanger(data.frame(dataToImpute),m=1,verbose=FALSE,...)
      
      outputMatrix <- data.matrix(miceRanger::completeData(objMR)[[1]])
        
        
    }
    
    # use knn from VIM
    
    if(method=="knn") {
      
      outputMatrix <- VIM::kNN(dataToImpute,imp_var = FALSE,...)
      
      
    }
    
    # use pmm from mice
    
    if(method=="pmm") {
      
      if(pmmWarnings) {
        
        temp <- mice::mice(dataToImpute,m=1,defaultMethod = "pmm",printFlag = FALSE,...)
        
      } else {
        
        suppressWarnings(temp <- mice::mice(dataToImpute,m=1,defaultMethod = "pmm",printFlag = FALSE,...))
        
      }
      
      outputMatrix <- data.matrix(mice::complete(temp))
      
      
    }
    
    
    # now we remove nonFNs if necessary
    
    if(checkFuzzy) {
      
      dataToImpute <- RemoveNotFuzzy(trueData=dataToImpute,imputedData=outputMatrix,trapezoidal = trapezoidal)
      
      # are there some NAs still?
      
      if(any(is.na(dataToImpute))) 
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
