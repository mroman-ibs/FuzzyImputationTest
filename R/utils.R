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


# function to check if the parameter is given by the integer

IfInteger <- function(x)
{
  if(is.numeric(x))
  {
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
    
    if(test == TRUE)
    { return(TRUE) }
    else { return(FALSE) }
  }
  
  else { return(FALSE) }
}



# A vector containing names of the calculated distance measures between fuzzy numbers.

distanceNames <- c("Euclidean", "AHD", "HSD", "DiffVal", "DiffAmb", "DiffEV", "DiffWidth")

# A vector containing names of the types of the calculated errors.

errorTypes <- c("MAE","WMAE","MSE","WMSE","NRMSE")

# A vector containing names of the types of the calculated statistical measures.

measuresTypes <- c("TrueMean","ImpMean", "AbsDiffTrueImpMean", "GenMean","GenImpMean", "AbsDiffGenImpMean","TrueSD","ImpSD",
                   "AbsDiffTrueImpSD","GenSD","GenImpSD","AbsDiffGenImpSD")

# A vector containing names of the subsets used in the epistemic tests.

setsNames <- c("true","imputed", "parts")

# A vector containing names of the types of the applied epistemic tests.

testsNames <- c("avs-anti","ms-anti","res-anti")
