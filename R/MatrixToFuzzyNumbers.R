#' Conversion of a matrix to a list of fuzzy numbers.
#'
#' @description
#' `MatrixToFuzzyNumbers` converts a matrix into a list of triangular or trapezoidal fuzzy numbers. 
#'
#' @details
#' The procedure converts the given matrix to a list of triangular or trapezoidal fuzzy numbers.
#' If the input matrix has 3 columns, then they are treated as descriptions of consecutive triangular fuzzy
#' numbers.
#' In the case of 4 columns, we get trapezoidal fuzzy numbers.
#' The values in these columns are equal to
#' the left supports, cores (or left and right ends of the cores for trapezoidal fuzzy 
#' numbers) and right supports. 
#' Each row is related to single fuzzy number.
#'
#'
#' @return
#' The output is given as a list of fuzzy numbers.
#'
#'
#'
#' @seealso \code{\link{FuzzyNumbersToMatrix}} for conversion of a list of fuzzy numbers into a matrix.
#'
#'
#' @param fuzzyMatrix Name of the matrix with fuzzy numbers.
#'
#'
#' @param varNames Optional names for values of the output list.
#' 
#' @param ... Additional parameters passed to other functions.
#'
#'
#' @examples
#'
#' 
#' library(FuzzyNumbers)
#' 
#' # prepare matrix with 2 triangular fuzzy numbers
#'
#' matrix1 <- matrix(c(1,3,5,2,5,7),ncol=3,byrow = TRUE)
#' 
#' # convert this matrix to list of fuzzy numbers
#'
#' MatrixToFuzzyNumbers(matrix1)
#'
#'
#' @export



# conversion of matrix to list of TRFN/TPFN

MatrixToFuzzyNumbers <- function(fuzzyMatrix, varNames=NA,...)
{
  
  # checking parameters
  
  if(!(is.matrix(fuzzyMatrix)))
  {
    stop("Parameter fuzzyList should be a matrix!")
  }
  
  if(nrow(fuzzyMatrix)==0)
  {
    stop("Not enough rows in the parameter fuzzyMatrix!")
    
  }
  
  if(!(ncol(fuzzyMatrix)==3 || ncol(fuzzyMatrix)==4))
  {
    stop("Parameter fuzzyList should be a matrix with 3 or 4 columns!")
    
  }
  
  if (!is.numeric(fuzzyMatrix))
  {
    stop("Parameter fuzzyMatrix should have numerical values!")
  }
  
  if(length(varNames)>1) {
    
    if (!is.vector(varNames) || !is.character(varNames))
    {
      stop("Parameter varNames should be a vector with character values or NA!")
    }
    
  }
  
  if(length(varNames)==1) {
    
    if (!(is.na(varNames) || is.character(varNames)))
    {
      stop("Parameter varNames should be a vector with character values or NA!")
    }
    
  }
  
  
  
  
  
  output <- list(rep(NA,nrow(fuzzyMatrix)))
  
  if(ncol(fuzzyMatrix)==3)
  {
    for (i in 1:nrow(fuzzyMatrix)) {
      
      output[[i]] <- FuzzyNumbers::TriangularFuzzyNumber(a1=fuzzyMatrix[i,1],amid = fuzzyMatrix[i,2],a4 = fuzzyMatrix[i,3])
      
      
    }
    
    
  }
  
  if(ncol(fuzzyMatrix)==4)
  {
    for (i in 1:nrow(fuzzyMatrix)) {
      
      output[[i]] <- FuzzyNumbers::TrapezoidalFuzzyNumber(a1=fuzzyMatrix[i,1],a2 = fuzzyMatrix[i,2],
                                            a3 = fuzzyMatrix[i,3],a4 = fuzzyMatrix[i,4])
      
      
    }
    
  }
  
  if(!anyNA(varNames))
  {
    names(output) <- varNames
  }
  
  return(output)
  
}



