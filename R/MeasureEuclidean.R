#' Function to calculate the Euclidean distance between two fuzzy numbers.
#'
#' @description
#' `MeasureEuclidean` calculates the Euclidean measure between two trapezoidal or triangular fuzzy numbers.
#'
#' @details
#' The procedure calculates the Euclidean measure of the distance between two trapezoidal or triangular fuzzy numbers.
#' The input values can be given as triangular/trapezoidal fuzzy numbers using the objects defined in \code{FuzzyNumbers} package
#' or vectors with three (in the case of triangular fuzzy numbers) or four (for trapezoidal fuzzy numbers) values related to
#' left end of the support, the core (or its interval, respectively), and the right end of the support.
#' The parameter \code{trapezoidal} is used to indicate if the input values are trapezoidal fuzzy numbers or triangular ones.
#'
#'
#' @return
#' The output is given as a numerical value.
#'
#'
#' @seealso \code{\link{MeasureAHD}, \link{MeasureHSD}} for other procedures to calculate distance measures.
#'
#'
#' @param value1 The first input triangular or trapezoidal fuzzy number.
#' 
#' @param value2 The second input triangular or trapezoidal fuzzy number.
#'
#'
#' @param trapezoidal Logical value depending on the type of input fuzzy values (triangular or trapezoidal ones)
#'
#' @param ... Additional parameters passed to other functions.
#'
#'
#' @examples
#'
#' 
#' 
#' # let's define two trapezoidal fuzzy numbers
#'
#' tpfn1 <- c(1,2,3,4)
#' 
#' tpfn2 <- c(2,6,8,10)
#' 
#' # calculate the distance
#' 
#' MeasureEuclidean(tpfn1,tpfn2)
#' 
#' # now we use objects from the FuzzyNumbers package
#'
#' # load the necessary library
#' 
#' library(FuzzyNumbers)
#' 
#' tpfn1 <- TrapezoidalFuzzyNumber(1,2,3,4)
#' 
#' tpfn2 <- TrapezoidalFuzzyNumber(2,6,8,10)
#' 
#' MeasureEuclidean(tpfn1,tpfn2)
#' 
#'
#' @export




# Euclidean measure for two fuzzy numbers


MeasureEuclidean <- function(value1,value2,trapezoidal=TRUE,...)
{
  # checking parameters
  
  if(!(methods::is(value1,"TrapezoidalFuzzyNumber") || is.vector(value1)))
  {
    stop("Parameter value1 should be an object of the class TrapezoidalFuzzyNumber or a vector!")
  }
  
  if(!(methods::is(value2,"TrapezoidalFuzzyNumber") || is.vector(value2)))
  {
    stop("Parameter value2 should be an object of the class TrapezoidalFuzzyNumber or a vector!!")
  }
  
  if ((length(trapezoidal)!=1 || (is.na(trapezoidal)) || (!is.logical(trapezoidal))))
  {
    stop("Parameter trapezoidal should be a single logical value!")
  }
  
  if(is.vector(value1))
  {
    if(!is.numeric(value1))
    {
      stop("Parameter value1 should be a numeric vector with 3 or 4 values!")
    }
    
    if(!(length(value1)==3 || length(value1)==4))
    {
      stop("Parameter value1 should be a numeric vector with 3 or 4 values!")
    }
  }
  
  
  if(is.vector(value2))
  {
    if(!is.numeric(value2))
    {
      stop("Parameter value2 should be a numeric vector with 3 or 4 values!")
    }
    
    if(!(length(value2)==3 || length(value2)==4))
    {
      stop("Parameter value2 should be a numeric vector with 3 or 4 values!")
    }
  }
  
  
  
  
  # some conversions
  
  if(methods::is(value1,"TrapezoidalFuzzyNumber"))
  {
    value1 <- c(FuzzyNumbers::supp(value1)[1],
                FuzzyNumbers::core(value1)[1],
                FuzzyNumbers::core(value1)[2],
                FuzzyNumbers::supp(value1)[2])
  }
  
  if(methods::is(value2,"TrapezoidalFuzzyNumber"))
  {
    value2 <- c(FuzzyNumbers::supp(value2)[1],
                FuzzyNumbers::core(value2)[1],
                FuzzyNumbers::core(value2)[2],
                FuzzyNumbers::supp(value2)[2])
  }
  
  
  if(trapezoidal)
  {
    
    fuzzyNumber1 <- value1
    fuzzyNumber2 <- value2
    
  } else {
    
    fuzzyNumber1 <- c(value1[1],value1[2],value1[2],value1[3])
    fuzzyNumber2 <- c(value2[1],value2[2],value2[2],value2[3])
    
  }
  
  output <- 1/3 * ( (fuzzyNumber1[1]-fuzzyNumber2[1])^2 + (fuzzyNumber1[2]-fuzzyNumber2[2])^2 +
                      (fuzzyNumber1[1]-fuzzyNumber2[1])*(fuzzyNumber1[2]-fuzzyNumber2[2]) +
                      (fuzzyNumber1[3]-fuzzyNumber2[3])^2 + (fuzzyNumber1[4]-fuzzyNumber2[4])^2 +
                      (fuzzyNumber1[3]-fuzzyNumber2[3])*(fuzzyNumber1[4]-fuzzyNumber2[4]))
  
  
  
  
  
  return(output/2)
  
}
