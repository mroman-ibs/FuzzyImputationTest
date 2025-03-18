# function to calculate standard error based on squares

CalculateSE <- function(outputImpTest,iterations)
{
  output <- outputImpTest
  
  # calculate SE for nonFNNumbers (difference of the squares)
  
  value <- output$nonFNNumbers["se"] - output$nonFNNumbers["mean"]^2
  
  output$nonFNNumbers["se"] <- sqrt(value/iterations) 
  
  # calculate SE for other benchmarks
  
  for (i in 2:length(output)) {
    
    value <- output[[i]][,"se"] - output[[i]][,"mean"]^2
    
    output[[i]][,"se"] <- sqrt(value/iterations) 
    
  }
  
  return(output)
}