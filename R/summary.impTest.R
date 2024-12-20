#' Print summary of the benchmark for the imputation method.
#' 
#' @param object Object of S3 class \code{impTest}.
#' 
#' @param ... Additional parameters passed to other functions.

#' @export

summary.impTest <- function(object,...)
{
  # preparing output matrix
  
  namesRows <- c("nonFNNumbers","MAE","MSE","NRMSE",
                 "AbsDiffTrueImpMean", "AbsDiffGenImpMean","AbsDiffTrueImpSD","AbsDiffGenImpSD",
                 "diff+avs-anti","diff+ms-anti","diff+res-anti","parts+avs-anti","parts+ms-anti","parts+res-anti",
                 distanceNames)
  
  output <- rep(NA, length(namesRows))
  
  
  
  names(output) <- namesRows
  
  # adding directly values to the output vector
  
  output["nonFNNumbers"] <- object[["nonFNNumbers"]][["mean"]]
  
  output["MAE"] <- object[["errorMatrix"]][["MAE","mean"]]
  
  output["MSE"] <- object[["errorMatrix"]][["MSE","mean"]]
  
  output["NRMSE"] <- object[["errorMatrix"]][["NRMSE","mean"]]
  
  output["AbsDiffTrueImpMean"] <- object[["statisticalMeasures"]][["AbsDiffTrueImpMean","mean"]]
  
  output["AbsDiffGenImpMean"] <- object[["statisticalMeasures"]][["AbsDiffGenImpMean","mean"]]
  
  output["AbsDiffTrueImpSD"] <- object[["statisticalMeasures"]][["AbsDiffTrueImpSD","mean"]]
  
  output["AbsDiffGenImpSD"] <- object[["statisticalMeasures"]][["AbsDiffGenImpSD","mean"]]
  
  output["parts+avs-anti"] <- object[["statisticalTests"]][["parts+avs-anti","mean"]]
  
  output["parts+ms-anti"] <- object[["statisticalTests"]][["parts+ms-anti","mean"]]
  
  output["parts+res-anti"] <- object[["statisticalTests"]][["parts+res-anti","mean"]]
  
  for (i in 1:length(distanceNames)) {
    
    output[distanceNames[i]] <- object[["fuzzyMeasures"]][[distanceNames[i],"mean"]]
    
  }
  
  # and some additional calculations for differences f p-values
  
  output["diff+avs-anti"] <- object[["statisticalTests"]][["true+avs-anti","mean"]] -
    object[["statisticalTests"]][["imputed+avs-anti","mean"]]
  
  output["diff+ms-anti"] <- object[["statisticalTests"]][["true+ms-anti","mean"]] -
    object[["statisticalTests"]][["imputed+ms-anti","mean"]]
  
  output["diff+res-anti"] <- object[["statisticalTests"]][["true+res-anti","mean"]] -
    object[["statisticalTests"]][["imputed+res-anti","mean"]]
  
  
  
  return(output)
}
