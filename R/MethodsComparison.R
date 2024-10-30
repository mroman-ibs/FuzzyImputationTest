#' Comparison of imputation methods for fuzzy values.
#'
#' @description
#' `MethodsComparison` compares the quality of built-in imputation methods using various measures and goodness-of-fit statistical tests
#'  for the given fuzzy dataset.
#'
#' @details
#' The procedure uses the function \code{ImputationTests} to compare the quality of the imputation methods for the specified fuzzy dataset.
#' To minimize random effects, each analysis is repeated \code{iterations} times with the new randomly generated NA values
#' in the input dataset, and then new imputed values for all built-in methods.
#' To generate the new NAs values, the function \code{IntroducingNA} is used.
#' Next, the results, the same as for\code{ImputationTests} (apart from \code{trueValues} and \code{mask}), are averaged.
#' 
#' The input dataset can be given as matrix or data frame.
#' 
#'
#' @return
#' The output is given as a list of the matrices: 
#' \code{nonFNNumbers} - the vector with the numbers of non-FNs samples for each variable (with the overall mean),
#' \code{errorMatrix} -- the output from the function \code{ErrorMatrix},
#' \code{statisticalMeasures} -- the output from the function \code{StatisticalMeasures}, 
#'  \code{statisticalTests} -- the output from the function \code{ApplyStatisticalTests}, 
#' \code{fuzzyMeasures} -- the output from the function \code{CalculateFuzzyMeasures}.
#'
#'
#'
#'
#' @param trueData Name of the input matrix (or data frame) with the true values of the variables.
#'
#'
#' @param iterations Number of the repetitions of each analyses (introducing NAs and then imputation of the missing values).
#' 
#' @param percentage Desired percentage of missing values (NAs) in each row.
#' 
#' @param trapezoidal Logical value depending on the type of fuzzy values (triangular or trapezoidal ones) in the dataset.
#' 
#' @param verbose Logical value if the progress bar should be shown.
#' 
#' @param ... Additional parameters passed to other functions.
#'
#'
#' @examples
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # load the necessary library
#' 
#' library(FuzzySimRes)
#' 
#' # generate sample of trapezoidal fuzzy numbers with FuzzySimRes library
#'
#' list1<-SimulateSample(20,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
#' incrCorePD="rexp", parIncrCorePD=list(rate=2),
#' suppLeftPD="runif",parSuppLeftPD=list(min=0,max=0.6),
#' suppRightPD="runif", parSuppRightPD=list(min=0,max=0.6),
#' type="trapezoidal")
#' 
#' # convert fuzzy data into a matrix
#' 
#' matrix1 <- FuzzyNumbersToMatrix(list1$value)
#' 
#' # check starting values
#' 
#' head(matrix1)
#' 
#' 
#' # check the quality of the imputed values
#' 
#' \dontrun{
#' 
#' MethodsComparison(matrix1,iterations=10,trapezoidal=TRUE)
#'
#' }
#'
#' @export




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
    
    trueData <- FuzzyNumbersToMatrix(fuzzyList=trueData,trapezoidal=trapezoidal,...)
    
  } 
  
  if(verbose)
  {
    
    pb = utils::txtProgressBar(min = 0, max = iterations, initial = 1, style = 3, char = "=")
    
  }
  
  
  # main loop
  
  for (i in 1:iterations)
  {
    
    
    # let's introduce some NAs
    
    dataWithNA <- IntroducingNA(dataMatrix = trueData, percentage = percentage)
    
    # check NAs pattern
    
    imputationMask <- is.na(dataWithNA)
    
    # now imputation methods
    
    imputedDataDimp <- FuzzyImputation(dataToImpute = dataWithNA,method = "dimp", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    imputedDataMF <- FuzzyImputation(dataToImpute = dataWithNA,method = "missForest", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    imputedDataMiceR <- FuzzyImputation(dataToImpute = dataWithNA,method = "miceRanger", trapezoidal = trapezoidal,verbose = FALSE,...)
    
    imputedDataKnn <- FuzzyImputation(dataToImpute = dataWithNA,method = "knn", trapezoidal = trapezoidal,verbose = FALSE,...)
    
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
      
      utils::setTxtProgressBar(pb,i)
      
    }
    
  }
  
  
  
  if(verbose)
  {
    
    close(pb)
    
    
  }
  
  
  # calculation of the averages
  
  
  outputQualityDimp <- mapply("/", outputQualityDimp,iterations)
  
  outputQualityMF <- mapply("/", outputQualityMF,iterations)
  
  outputQualityMiceR <- mapply("/", outputQualityMiceR,iterations)
  
  outputQualityKnn <- mapply("/", outputQualityKnn,iterations)
  
  
  
  return(list(dimp=outputQualityDimp,
              missForest=outputQualityMF,
              miceRanger=outputQualityMiceR,
              knn=outputQualityKnn))
  
  
}


