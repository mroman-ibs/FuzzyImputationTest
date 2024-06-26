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
#' @param varNames Optional names for values of the output list
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

MatrixToFuzzyNumbers <- function(fuzzyMatrix, varNames=NA)
{
  if(nrow(fuzzyMatrix)==0)
  {
    print("Not enough rows!")
    
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



