# function to apply tests after imputation

ImputationTests <- function(trueData,imputedData,imputedMask,trapezoidal=TRUE,
                            cutsNumber=100,K=50,...)
{
  # conversions
  
  if(is.data.frame(trueData))
  {
    trueData <- data.matrix(trueData)
    
  } 
  
  if(is.data.frame(imputedData))
  {
    imputedData <- data.matrix(imputedData)
    
  } 
  
  if(is.data.frame(imputedMask))
  {
    imputedMask <- data.matrix(imputedMask)
    
  } 
  
 
  
  
  
  variableNumber <- ncol(trueData)
  
  obsNumber <- nrow(trueData)
  
  # which rows are imputed?
  
  rowsNumbersImputed <- which(apply(imputedMask,1,any))
  
  
  # how many non-FN values in outputs?
  
  nonFNNumbers <- NumberOfNonFNs(imputedData[rowsNumbersImputed,],trapezoidal = trapezoidal)
  
  # calculate the error matrix
  
  errorMatrix <- ErrorMatrix(trueData,imputedData,imputedMask,...)
  
  # to do
  
  # calculate statistical measures
  
  statisticalMeasures <- StatisticalMeasures(trueData,imputedData,imputedMask,...)

  # apply tests
  
  statisticalTests <- ApplyStatisticalTests(trueData,imputedData,imputedMask,
                                            cutsNumber=cutsNumber,K=K,...)
  
  # calculate distances
  
  # print("Calculating fuzzy distances for the output..")
  
  fuzzyMeasures <- CalculateFuzzyMeasures(trueData,imputedData,imputedMask,
                                          trapezoidal=trapezoidal,...)
  
  
  

  return(list(trueValues=trueData,
              mask=imputedMask,
              nonFNNumbers=nonFNNumbers,
              errorMatrix=errorMatrix,
              statisticalMeasures=statisticalMeasures,
              statisticalTests=statisticalTests,
              fuzzyMeasures=fuzzyMeasures))
  
  
}



