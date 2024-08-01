# auxiliary function to perform statistical tests for the single variable

ApplyStatisticalTestsSingleVar <- function(trueData,imputedData,imputedMask,trapezoidal,cutsNumber,K)
{
  # let's divide sample to subsamples
  
  rowsNumbersImputed <- which(apply(imputedMask,1,any))
  
  dataWithoutChanges <- trueData[-rowsNumbersImputed,]
  
  dataFromImputation <- imputedData[rowsNumbersImputed,]
  
  dataBeforeImputation <- trueData[rowsNumbersImputed,]
  
  # use only true FNs
  
  trueFNRowsNumbers <- which(apply(dataFromImputation, MARGIN=1, FUN=IsFuzzy, trapezoidal=trapezoidal) == TRUE)
  
  
  
  # conversion to fuzzy numbers
  
  dataWithoutChangesFuzzy <- MatrixToFuzzyNumbers(dataWithoutChanges)
  
  dataFromImputationFuzzy <- MatrixToFuzzyNumbers(dataFromImputation[trueFNRowsNumbers,])
  
  dataBeforeImputationFuzzy <- MatrixToFuzzyNumbers(dataBeforeImputation)
  
  # vector for the results
  
  output <- rep(NA,times=length(setsNames)*length(testsNames))
  
  names(output) <- noquote(paste(rep(setsNames,each=3),testsNames,sep="+"))
  
  # now we apply statistical tests
  
  output["true+avs-anti"] <- FuzzySimRes::AverageStatisticEpistemicTest(dataWithoutChangesFuzzy,dataBeforeImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber)
  
  output["true+ms-anti"] <- FuzzySimRes::MultiStatisticEpistemicTest(dataWithoutChangesFuzzy,dataBeforeImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,combineMethod = "mean")
  
  output["true+res-anti"] <- FuzzySimRes::ResamplingStatisticEpistemicTest(dataWithoutChangesFuzzy,dataBeforeImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,K=K,combineMethod = "mean")
  
   output["imputed+avs-anti"] <- FuzzySimRes::AverageStatisticEpistemicTest(dataWithoutChangesFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber)
  
  output["imputed+ms-anti"] <- FuzzySimRes::MultiStatisticEpistemicTest(dataWithoutChangesFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,combineMethod = "mean")
  
  output["imputed+res-anti"] <- FuzzySimRes::ResamplingStatisticEpistemicTest(dataWithoutChangesFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,K=K,combineMethod = "mean")
  
  output["parts+avs-anti"] <- FuzzySimRes::AverageStatisticEpistemicTest(dataBeforeImputationFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber)
  
  output["parts+ms-anti"] <- FuzzySimRes::MultiStatisticEpistemicTest(dataBeforeImputationFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,combineMethod = "mean")
  
  output["parts+res-anti"] <- FuzzySimRes::ResamplingStatisticEpistemicTest(dataBeforeImputationFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,K=K,combineMethod = "mean")
  
  
  return(output)
  
}
