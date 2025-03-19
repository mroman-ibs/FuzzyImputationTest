# utility function to prepare data from ImputationTest for MethodsComparison

PrepareDataForComparison <- function(outputImpTest)
{
  # add some part of the input list
  
  output <- outputImpTest[c(3:7)]
  
  # take nonFNNumbers, prepare name for new value (standard error)
  
  oldNames <- names(output$nonFNNumbers)
  
  # add the squared mean to the nonFNNumbers
  
  output$nonFNNumbers <- c(output$nonFNNumbers,(output$nonFNNumbers["mean"])^2)
  
  names(output$nonFNNumbers) <- c(oldNames,"se")
  
  # the same as above, but for other benchmarks
  
  for (i in 2:length(output)) {
    
    oldNames <- colnames(output[[i]])
    
    output[[i]] <- cbind(output[[i]],(output[[i]][,"mean"])^2)
    
    colnames(output[[i]]) <- c(oldNames,"se")
    
  }
  
  
  return(output)
  
}