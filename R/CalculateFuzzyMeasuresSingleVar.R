# calculate all fuzzy distances for the pairs of fuzzy numbers for single variable

CalculateFuzzyMeasuresSingleVar <- function(fuzzyMatrix1,fuzzyMatrix2,imputedMask,trapezoidal,...)
{
  # create vector for output
  
  output <- rep(0,length(distanceNames))
  
  names(output) <- distanceNames
  
  
  
  # choose only imputed values
  
  rowsNumbersImputed <- which(apply(imputedMask,1,any))
  
  fuzzyMatrix1Imp <- fuzzyMatrix1[rowsNumbersImputed,]
  
  fuzzyMatrix2Imp <- fuzzyMatrix2[rowsNumbersImputed,]
  
  # use only true FNs from the second matrix
  
  trueFNRowsNumbers <- which(apply(fuzzyMatrix2Imp, MARGIN=1, FUN=IsFuzzy, trapezoidal=trapezoidal) == TRUE)
  
  # cat("trueFNRowsNumbers: ", trueFNRowsNumbers, "\n")
  
  # cat("rowsNumbersImputed: ", rowsNumbersImputed, "\n")
  
  fuzzyMatrix1Imp <- fuzzyMatrix1Imp[trueFNRowsNumbers,]
  
  # print(fuzzyMatrix1Imp)
  
  fuzzyMatrix2Imp <- fuzzyMatrix2Imp[trueFNRowsNumbers,]
  
  # print(fuzzyMatrix2Imp)
  
  for (i in 1:nrow(fuzzyMatrix1Imp)) {
    
    # cat("i: ", i , "\n")
    
    output["Euclidean"] <-  output["Euclidean"] + MeasureEuclidean(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],trapezoidal = trapezoidal)
    
    output["AHD"] <-  output["AHD"] + MeasureAHD(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],trapezoidal = trapezoidal)
    
    output["HSD"] <-  output["HSD"] + MeasureHSD(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],trapezoidal = trapezoidal)
    
    # calculate Bertoluzza measure according to if FN is trapezoidal or not
    
    if(trapezoidal)
    {
      # cat("fuzzyMatrix1Imp[i,]: ", fuzzyMatrix1Imp[i,] , "\n")
      
      a <- FuzzyNumbers::TrapezoidalFuzzyNumber(fuzzyMatrix1Imp[i,1],fuzzyMatrix1Imp[i,2],fuzzyMatrix1Imp[i,3],fuzzyMatrix1Imp[i,4])
      
      # cat("fuzzyMatrix2Imp[i,]: ", fuzzyMatrix2Imp[i,] , "\n")

      b <- FuzzyNumbers::TrapezoidalFuzzyNumber(fuzzyMatrix2Imp[i,1],fuzzyMatrix2Imp[i,2],fuzzyMatrix2Imp[i,3],fuzzyMatrix2Imp[i,4])

    } else {

      a <- FuzzyNumbers::TriangularFuzzyNumber(fuzzyMatrix1Imp[i,1],fuzzyMatrix1Imp[i,2],fuzzyMatrix1Imp[i,3])

      b <- FuzzyNumbers::TriangularFuzzyNumber(fuzzyMatrix2Imp[i,1],fuzzyMatrix2Imp[i,2],fuzzyMatrix2Imp[i,3])

    }

    output["Bertoluzza"] <-  output["Bertoluzza"] + FuzzySTs::distance(a,b,type = "Bertoluzza",...)
    
    output["DiffVal"] <-  output["DiffVal"] + MeasureCharacteristic(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],type="HValue",trapezoidal = trapezoidal)
    
    output["DiffAmb"] <-  output["DiffAmb"] + MeasureCharacteristic(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],type="HAmbiguity",trapezoidal = trapezoidal)
    
    output["DiffEV"] <-  output["DiffEV"] + MeasureCharacteristic(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],type="EV",trapezoidal = trapezoidal)
    
    output["DiffWidth"] <-  output["DiffWidth"] + MeasureCharacteristic(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],type="Width",trapezoidal = trapezoidal)
    
    # print(output)
    
  }
  
  output <- output / nrow(fuzzyMatrix1Imp)
  
  return(output)
  
  
}
