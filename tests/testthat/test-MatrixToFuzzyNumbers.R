test_that("Function returns correct values", {
  
  # starting values
  
  testMatrixTRFNs <- matrix(c(1,3,5,2,5,7),ncol=3,byrow = TRUE)
  
  testMatrixTPFNs <- matrix(c(1,3,5,6,2,5,7,10),ncol=4,byrow = TRUE)
  
  
  
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      MatrixToFuzzyNumbers(fuzzyMatrix=testMatrixTRFNs)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      MatrixToFuzzyNumbers(fuzzyMatrix=testMatrixTPFNs)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      MatrixToFuzzyNumbers(fuzzyMatrix=testMatrixTPFNs,varNames = c("a","b"))
      
    }
  )
  
  
  
})


test_that("Function reports errors", {
  
  # starting values
  
  testMatrixBad1 <- matrix(c(1,3,5,9,10,2,5,7,11,12),ncol=5,byrow = TRUE)
  
  testMatrixBad2 <- matrix(c(1,3,5,2,5,"c"),ncol=3,byrow = TRUE)
  
  
  # tests
  
  expect_error(MatrixToFuzzyNumbers(fuzzyMatrix=testMatrixBad1),
               
               "Parameter fuzzyList should be a matrix with 3 or 4 columns!")
  
  
  expect_error(MatrixToFuzzyNumbers(fuzzyMatrix=testMatrixBad2),
               
               "Parameter fuzzyMatrix should have numerical values!")
  
  expect_error(MatrixToFuzzyNumbers(fuzzyMatrix=5),
               
               "Parameter fuzzyList should be a matrix!")
  
  
  
  
  
})

