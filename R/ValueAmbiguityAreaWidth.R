# auxiliary functions to calculate H-value

HValue <- function(fuzzyNumber,height=1)
{
  output <- height^2 / 6 * (fuzzyNumber[1]+2*fuzzyNumber[2]+2*fuzzyNumber[3]+fuzzyNumber[4])
  
  return(output)
  
  
}


# and H-ambiguity

HAmbiguity <- function(fuzzyNumber,height=1)
{
  output <- height^2 / 6 * (-fuzzyNumber[1]-2*fuzzyNumber[2]+2*fuzzyNumber[3]+fuzzyNumber[4])
  
  return(output)
  
  
}


# and H-area

HArea <- function(fuzzyNumber,height=1)
{
  output <- height / 2 * (-fuzzyNumber[1]-fuzzyNumber[2]+fuzzyNumber[3]+fuzzyNumber[4])
  
  return(output)
  
  
}


# Hausdorff distance

HDistance <- function(fuzzyNumber1,fuzzyNumber2)
{
  output <- max(abs(fuzzyNumber1[3]-fuzzyNumber2[3]),abs(fuzzyNumber1[4]-fuzzyNumber2[4]))
  
  return(output)
}

# Expected Value

EV <- function(fuzzyNumber)
{
  output <- (fuzzyNumber[1]+fuzzyNumber[2]+fuzzyNumber[3]+fuzzyNumber[4]) / 4
  
  return(output)
  
  
}

# width

Width <- function(fuzzyNumber)
{
  output <- (-fuzzyNumber[1]-fuzzyNumber[2]+fuzzyNumber[3]+fuzzyNumber[4]) / 4
  
  return(output)
  
  
}

