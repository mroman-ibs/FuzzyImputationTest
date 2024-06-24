# helper function for imputation using DIMP method with single variable data

ImputationDimpSingleVar <- function(dataToImpute)
{
  if(ncol(dataToImpute)==3)
  {
    
    trapezoidal=FALSE
  
  } else {
    
    trapezoidal=TRUE
    
  }
  
  maskMatrix <- is.na(dataToImpute)
  
  # print("trapezoidal")
  # print(trapezoidal)
  
  # if trapezoidal FNs, then we have another vector
  
  if(trapezoidal==TRUE)
  {
    
    
    # calculate the cores, and spreads of the core
    
    missingCore <- which(maskMatrix[,2]==TRUE)
    
    coreValues <- dataToImpute[-missingCore,2]
    
    # where we have NAs for the core?
    
    missingCoreSpread <- which(apply(maskMatrix[,c(2:3)],1,any))
    
    coreSpreads <- dataToImpute[-missingCoreSpread,3] - dataToImpute[-missingCoreSpread,2]
    
    # where we have NAs for the left support increase?
    
    missingLeftSupp <- which(apply(maskMatrix[,c(1:2)],1,any))
    
    # calculate the left increases of the support
    
    leftSuppIncrease <- dataToImpute[-missingLeftSupp,2] - dataToImpute[-missingLeftSupp,1]
    
    # where we have NAs for the right support increase?
    
    missingRightSupp <- which(apply(maskMatrix[,c(3:4)],1,any))
    
    # calculate the right increases of the support
    
    rightSuppIncrease <- dataToImpute[-missingRightSupp,4] - dataToImpute[-missingRightSupp,3]
    
    
    # 
    # print("coreSpreads")
    # print(coreSpreads)
    # 
    
  } else {
    
    # we have triangular FNs
    
    # where we have NAs for the core?
    
    missingCore <- which(maskMatrix[,2]==TRUE)
    
    # calculate the cores
    
    coreValues <- dataToImpute[-missingCore,2]
    
    # where we have NAs for the left support increase?
    
    missingLeftSupp <- which(apply(maskMatrix[,c(1:2)],1,any))
    
    # calculate the left increases of the support
    
    leftSuppIncrease <- dataToImpute[-missingLeftSupp,2] - dataToImpute[-missingLeftSupp,1]
    
    # where we have NAs for the right support increase?
    
    missingRightSupp <- which(apply(maskMatrix[,c(2:3)],1,any))
    
    # calculate the right increases of the support
    
    rightSuppIncrease <- dataToImpute[-missingRightSupp,3] - dataToImpute[-missingRightSupp,2]
    
    
  }
  
  # print("coreValues")
  # print(coreValues)
  # 
  # 
  # print("leftSuppIncrease")
  # print(leftSuppIncrease)
  # 
  # 
  # print("rightSuppIncrease")
  # print(rightSuppIncrease)
  
  # now we impute missing data
  
  if(trapezoidal == FALSE)
  {
    # let's start from triangular
    
    # now all missing values
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==TRUE) & (is.na(dataToImpute[,2])==TRUE) & (is.na(dataToImpute[,3])==TRUE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core
      
      imputedValues <- sample(coreValues,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
   
      
    }
    
    # print("all missing values")
    # print(dataToImpute)
    # 
    
    # now: existing, NA
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==FALSE) & (is.na(dataToImpute[,2])==TRUE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core using spreads
      
      imputedValues <- sample(leftSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- dataToImpute[rowsToImpute,1] + imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
      
    }
    
    # print("existing, NA")
    # print(dataToImpute)
    
    # now: NA, NA, existing
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==TRUE) & (is.na(dataToImpute[,2])==TRUE) & (is.na(dataToImpute[,3])==FALSE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core using spreads
      
      imputedValues <- sample(rightSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- dataToImpute[rowsToImpute,3] - imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("NA, NA, existing")
    # print(dataToImpute)
    
    # now: missing left end of the support
    
    rowsToImpute <- which(is.na(dataToImpute[,1])==TRUE)
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing left end of the support using spreads
      
      imputedValues <- sample(leftSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,1] <- dataToImpute[rowsToImpute,2] - imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("missing left end of the support")
    # print(dataToImpute)
    
    
    # now: missing right end of the support
    
    rowsToImpute <- which(is.na(dataToImpute[,3])==TRUE)
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing right end of the support using spreads
      
      imputedValues <- sample(rightSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,3] <- dataToImpute[rowsToImpute,2] + imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("missing right end of the support")
    # print(dataToImpute)
    
    
  }
  
  if(trapezoidal==TRUE)
  {
    # trapezoidal FNs
    
    # now all missing values
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==TRUE) & (is.na(dataToImpute[,2])==TRUE) & (is.na(dataToImpute[,3])==TRUE) & (is.na(dataToImpute[,4])==TRUE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core and its spread
      
      imputedValues <- sample(coreValues,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      imputedValues <- sample(coreSpreads,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,3] <- dataToImpute[rowsToImpute,2] + imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("all missing values")
    # print(dataToImpute)
    
    
    # now: existing, NA, NA, ?
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==FALSE) & (is.na(dataToImpute[,2])==TRUE) & (is.na(dataToImpute[,3])==TRUE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core using spreads for the left support and core
      
      imputedValues <- sample(leftSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- dataToImpute[rowsToImpute,1] + imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      imputedValues <- sample(coreSpreads,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,3] <- dataToImpute[rowsToImpute,2] + imputedValues
      
      # print("imputedValues")
      # print(imputedValues)
      
    }
    
    # print("existing, NA, NA, ?")
    # print(dataToImpute)
    
    # now: NA, NA, NA, existing
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==TRUE) & (is.na(dataToImpute[,2])==TRUE) & (is.na(dataToImpute[,3])==TRUE) & (is.na(dataToImpute[,4])==FALSE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core using spread for the right support and core
      
      imputedValues <- sample(rightSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,3] <- dataToImpute[rowsToImpute,4] - imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      imputedValues <- sample(coreSpreads,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- dataToImpute[rowsToImpute,3] - imputedValues
      
      
      # print("imputedValues")
      # print(imputedValues)
      # 
      
    }
    
    # print("NA, NA, NA, existing")
    # print(dataToImpute)
    
    
    # now: ?, NA, existing, ?
    
    rowsToImpute <- which((is.na(dataToImpute[,2])==TRUE) & (is.na(dataToImpute[,3])==FALSE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core using spreads for the core
      
      imputedValues <- sample(coreSpreads,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,2] <- dataToImpute[rowsToImpute,3] - imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("?, NA, existing, ?")
    # print(dataToImpute)
    
    
    # now: ?, existing, NA, ?
    
    rowsToImpute <- which((is.na(dataToImpute[,2])==FALSE) & (is.na(dataToImpute[,3])==TRUE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing core using spreads for the core
      
      imputedValues <- sample(coreSpreads,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,3] <- dataToImpute[rowsToImpute,2] + imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("?, existing, NA, ?")
    # print(dataToImpute)
    
    # now: NA, exisiting, ?, ?
    
    rowsToImpute <- which((is.na(dataToImpute[,1])==TRUE) & (is.na(dataToImpute[,2])==FALSE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing support using left spread for the support
      
      imputedValues <- sample(leftSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,1] <- dataToImpute[rowsToImpute,2] - imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("NA, exisiting, ?, ?")
    # print(dataToImpute)
    # 
    
    # now: ?, ?, existing, NA
    
    rowsToImpute <- which((is.na(dataToImpute[,4])==TRUE) & (is.na(dataToImpute[,3])==FALSE))
    
    if(length(rowsToImpute) > 0)
    {
      # sample for the missing support using right spread for the support
      
      imputedValues <- sample(rightSuppIncrease,length(rowsToImpute),replace = TRUE)
      
      dataToImpute[rowsToImpute,4] <- dataToImpute[rowsToImpute,3] + imputedValues
      
      # print("rowsToImpute")
      # print(rowsToImpute)
      # 
      # print("imputedValues")
      # print(imputedValues)
      
      
    }
    
    # print("?, ?, existing, NA")
    # print(dataToImpute)
    
    
  }
  
  
  
  
  return(dataToImpute)
  
}
