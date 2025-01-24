# function to measure the absolute difference between two FNs using one of the characteristics

MeasureCharacteristic <- function(value1,value2,type="EV",trapezoidal=TRUE)
{
  # conversions related to trapezoidal/triangular types
  
  if(trapezoidal)
  {
    
    fuzzyNumber1 <- value1
    fuzzyNumber2 <- value2
    
  } else {
    
    fuzzyNumber1 <- c(value1[1],value1[2],value1[2],value1[3])
    fuzzyNumber2 <- c(value2[1],value2[2],value2[2],value2[3])
    
  }
  
  output <- abs(do.call(what = type,args = list(fuzzyNumber1))-do.call(what = type,args = list(fuzzyNumber2)))
  
  return(output)
  
}