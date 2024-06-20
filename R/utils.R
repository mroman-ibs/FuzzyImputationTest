# check if the initial value is correct fuzzy number

IsFuzzy <- function(fuzzyNumber,trapezoidal)
{
  
  if(trapezoidal)
  {
    
    if((fuzzyNumber[1] <= fuzzyNumber[2]) & (fuzzyNumber[2] <= fuzzyNumber[3]) & (fuzzyNumber[3] <= fuzzyNumber[4]))
    {
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
      
    }
    
  } else {
    
    if((fuzzyNumber[1] <= fuzzyNumber[2]) & (fuzzyNumber[2] <= fuzzyNumber[3]))
    {
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
      
    }
    
    
  }
  
  
  
  
  
}


# numbers of rows for non-FNs in the matrix

NonFNRowNumbers <- function(fuzzyMatrix)
{
  if(ncol(fuzzyMatrix)==3)
  {
    
    trapezoidal=FALSE
    
  } else {
    
    trapezoidal=TRUE
    
  }
  
  which(apply(fuzzyMatrix, MARGIN=1, FUN=IsFuzzy, trapezoidal=trapezoidal) == FALSE)
  
}



