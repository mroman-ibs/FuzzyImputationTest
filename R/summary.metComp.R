#' Print summary of the comparison of the imputation methods.
#' 
#' @param object Object of S3 class \code{metComp}.
#' 
#' @param ... Additional parameters passed to other functions.

#' @export

summary.metComp <- function(object,...)
{
  # preparing output matrix
  
  namesRows <- names(summary.impTest(object=object[[methodNames[1]]]))
  
  output <- matrix(NA, nrow = length(namesRows), ncol = length(methodNames))
  
  dimnames(output) <- list(namesRows,methodNames)
  
  # adding directly values to the output matrix
  
  for (j in 1:length(methodNames)) {
    
    output[,j] <- summary.impTest(object=object[[methodNames[j]]])
    
    
  }
  
  
  return(output)
}
