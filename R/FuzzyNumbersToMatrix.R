#' Conversion of a list of fuzzy numbers into a matrix.
#'
#' @description
#' `FuzzyNumbersToMatrix` converts a list of triangular or trapezoidal fuzzy numbers into a matrix.
#'
#' @details
#' The procedure converts the given list of triangular or trapezoidal fuzzy numbers into a matrix.
#' It is necessary to select the appropriate type of these fuzzy numbers (using \code{trapezoidal=FALSE} for
#'  triangular fuzzy
#' numbers or the default \code{trapezoidal=TRUE} for trapezoidal ones)
#' The output matrix has 3 (for triangular fuzzy numbers)
#' or 4 (for trapezoidal ones) columns with the values of the left supports, cores (or the left and right ends of the cores for trapezoidal fuzzy 
#' cores) and right supports. 
#' Each row is related to single fuzzy number.
#'
#'
#' @return
#' The output is given as a matrix with 3 (for triangular fuzzy numbers)
#' or 4 (for trapezoidal ones) columns and the number of rows is equal to number of fuzzy values.
#'
#'
#'
#' @seealso \code{\link{MatrixToFuzzyNumbers}} for conversion of a matrix to fuzzy numbers
#'
#'
#' @param fuzzyList Name of the list with fuzzy numbers.
#'
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones).
#'
#' @param varNames Optional names for columns of the output matrix.
#'
#' @param ... Additional parameters passed to other functions.
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
#' # check the first fuzzy number
#' 
#' list1$value[[1]]
#' 
#' # convert fuzzy numbers to a matrix and check the first value
#'
#' head(FuzzyNumbersToMatrix(list1$value))
#'
#'
#' @export



# conversion of list of TRFN/TPFNs to matrix

FuzzyNumbersToMatrix <- function(fuzzyList,trapezoidal=TRUE,varNames=NA,...)
{
  numberOfFN <- length(fuzzyList)
  
  if(trapezoidal)
  {
    
    output <- matrix(NA,nrow = numberOfFN,ncol = 4)
    
  } else
    
  {
    output <- matrix(NA,nrow = numberOfFN,ncol = 3)
  }
  
  if(!is.na(varNames))
  {
    
    rownames(fuzzyList) <- varNames
    
  }
  
  # FNs are converted to real values
  
  for (i in 1:numberOfFN) {
    
    output[i,1] <- FuzzyNumbers::supp(fuzzyList[[i]])[1]
    
    output[i,2] <- FuzzyNumbers::core(fuzzyList[[i]])[1]
    
    if(trapezoidal)
    {
      output[i,3] <- FuzzyNumbers::core(fuzzyList[[i]])[2]
      
      output[i,4] <- FuzzyNumbers::supp(fuzzyList[[i]])[2]
      
    } else {
      
      output[i,3] <- FuzzyNumbers::supp(fuzzyList[[i]])[2]
      
    }
    
  }
  
  return(output)
  
}