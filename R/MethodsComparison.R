




# function to compare all the imputation methods

MethodsComparison <- function(trueData,iterations=100,percentage=0.05,trapezoidal=TRUE,verbose=TRUE,...)
{
  # conversions
  
  if(is.data.frame(trueData))
  {
    trueData <- data.matrix(trueData)
    
  } 
  
  
  if(is.list(trueData) && !is.data.frame(trueData))
  {
    # conversion to matrix
    
    trueData <- FuzzyNumbersToMatrix(trueData,...)
    
  } 
  
  if(verbose)
  {
    
    pb = txtProgressBar(min = 0, max = iterations, initial = 1, style = 3, char = "=")
    
  }
  
  
  # main loop
  
  for (i in 1:iterations)
  {
    
    
    # let's introduce some NAs
    
    dataWithNA <- IntroducingNA(dataMatrix = trueData, percentage = percentage)
    
    # check NAs pattern
    
    imputationMask <- is.na(dataWithNA)
    
    # now imputation methods
    
    imputedDataDimp <- FuzzyImputation(dataSet = dataWithNA,method = "dimp", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    imputedDataMF <- FuzzyImputation(dataSet = dataWithNA,method = "missForest", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    imputedDataMiceR <- FuzzyImputation(dataSet = dataWithNA,method = "miceRanger", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    imputedDataKnn <- FuzzyImputation(dataSet = dataWithNA,method = "knn", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    # check quality
    
    qualityDimp <- ImputationTests(trueData = trueData,imputedData = imputedDataDimp,
                                   imputedMask=imputationMask,trapezoidal = trapezoidal,...)
    
    qualityMF <- ImputationTests(trueData = trueData,imputedData = imputedDataMF,
                                   imputedMask=imputationMask,trapezoidal = trapezoidal,...)
    
    qualityMiceR <- ImputationTests(trueData = trueData,imputedData = imputedDataMiceR,
                                   imputedMask=imputationMask,trapezoidal = trapezoidal,...)
    
    qualityKnn <- ImputationTests(trueData = trueData,imputedData = imputedDataKnn,
                                   imputedMask=imputationMask,trapezoidal = trapezoidal,...)
    
    # print(qualityDimp)
    
    # create new output or add to the previous one
    
    if(i==1) 
    {
      outputQualityDimp <- qualityDimp[c(3:7)]
      
      outputQualityMF <- qualityMF[c(3:7)]
      
      outputQualityMiceR <- qualityMiceR[c(3:7)]
      
      outputQualityKnn <- qualityKnn[c(3:7)]
      
      # print(outputQualityDimp)
      
    } else {
      
      outputQualityDimp <- mapply("+", qualityDimp[c(3:7)],outputQualityDimp)
      
      outputQualityMF <- mapply("+", qualityMF[c(3:7)],outputQualityMF)
      
      outputQualityMiceR <- mapply("+", qualityMiceR[c(3:7)],outputQualityMiceR)
      
      outputQualityKnn <- mapply("+", qualityKnn[c(3:7)],outputQualityKnn)
      
      # print(outputQualityDimp)

    }
    
    
    if(verbose)
    {
      
      setTxtProgressBar(pb,i)
      
    }
    
  }
  
  
  
  if(verbose)
  {
    
    close(pb)
    
    
  }
  
  
  # calculation of the averages
  
  
  outputQualityDimp <- mapply("/", outputQualityDimp,iterations)
  
  outputQualityMF <- mapply("/", outputQualityDimp,iterations)
  
  outputQualityMiceR <- mapply("/", outputQualityDimp,iterations)
  
  outputQualityKnn <- mapply("/", outputQualityDimp,iterations)
  
  
  
  return(list(dimp=outputQualityDimp))
  
  
}


