test_that("Function returns correct values", {
  
  # starting values
  
  testListTPFNs <- list(FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,4,5),FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,7,10),
                        FuzzyNumbers::TrapezoidalFuzzyNumber(-2,1,5,9))
  
  testListTRFNs <- list(FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,3,5),FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,6,10),
                        FuzzyNumbers::TrapezoidalFuzzyNumber(-2,1,1,9))
  
  
  
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      FuzzyNumbersToMatrix(fuzzyList=testListTPFNs,trapezoidal=TRUE)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      FuzzyNumbersToMatrix(fuzzyList=testListTRFNs,trapezoidal=FALSE)
      
    }
  )
  
  expect_snapshot(
    
    {
      FuzzyNumbersToMatrix(fuzzyList=testListTPFNs,trapezoidal=TRUE,varNames = c("a","b","c","d"))
      
    }
  )
  
  
  expect_snapshot(
    
    {
      FuzzyNumbersToMatrix(fuzzyList=testListTRFNs,trapezoidal=FALSE,varNames = c("a","b","c"))
      
    }
  )
  
})



test_that("Function reports errors", {
  
  
  # starting values
  
  testListTPFNs <- list(FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,4,5),FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,7,10),
                        FuzzyNumbers::TrapezoidalFuzzyNumber(-2,1,5,9))
  
  testListTRFNs <- list(FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,3,5),FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,6,10),
                        FuzzyNumbers::TrapezoidalFuzzyNumber(-2,1,1,9))
  
  # tests
  
  expect_error(FuzzyNumbersToMatrix(fuzzyList=testListTRFNs,trapezoidal="c"),
               
               "Parameter trapezoidal should be a single logical value!")
  
  
  expect_error(FuzzyNumbersToMatrix(fuzzyList=c(0,1,2,5),trapezoidal=TRUE),
               
               "Parameter fuzzyList should be a list of fuzzy numbers!")
  
  expect_error(FuzzyNumbersToMatrix(fuzzyList=testListTRFNs,trapezoidal=FALSE,varNames = c(0,1,4)),
               
               "Parameter varNames should be a vector with character values or NA!")
  
  
  
  
  
})
