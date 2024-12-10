test_that("Function returns correct values", {
  
  # starting values
  
  testFN1 <- FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,4,5)
  
  testFN2 <- FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,7,10)
  
  testFN3 <- FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,3,4)
  
  testFN4 <- FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,6,7)
  
  testFN1c <- c(1,3,4,5)
  
  testFN2c <- c(2,6,7,10)
  
  testFN3c <- c(1,3,4)
  
  testFN4c <- c(2,6,7)
  
  
  
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      MeasureAHD(value1 = testFN1,value2 = testFN2)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      MeasureAHD(value1 = testFN3,value2 = testFN4, trapezoidal = FALSE)
      
    }
  )
  
  expect_snapshot(
    
    {
      MeasureAHD(value1 = testFN1c,value2 = testFN2c)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      MeasureAHD(value1 = testFN3c,value2 = testFN4c, trapezoidal = FALSE)
      
    }
  )
  
})


test_that("Function reports errors", {
  
  
  # starting values
  
  testFN1 <- FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,4,5)
  
  testFN2 <- FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,7,10)
  
  testFN3 <- FuzzyNumbers::TrapezoidalFuzzyNumber(1,3,3,4)
  
  testFN4 <- FuzzyNumbers::TrapezoidalFuzzyNumber(2,6,6,7)
  
  testFN1c <- c(1,3,4,5)
  
  testFN2c <- c(2,6,7,10)
  
  testFN3c <- c(1,3,4)
  
  testFN4c <- c(2,6,7)
  
  
  # tests
  
  expect_error(MeasureAHD(value1 = c("a",2,3),value2 = testFN1, trapezoidal = TRUE),
               
               "Parameter value1 should be a numeric vector with 3 or 4 values!")
  
  expect_error(MeasureAHD(value1 = c(1,2,3,4,5),value2 = testFN1, trapezoidal = TRUE),
               
               "Parameter value1 should be a numeric vector with 3 or 4 values!")
  
  expect_error(MeasureAHD(value1 = testFN1,value2 = c("a",2,3), trapezoidal = TRUE),
               
               "Parameter value2 should be a numeric vector with 3 or 4 values!")
  
  expect_error(MeasureAHD(value1 = testFN1,value2 = c(1,2,3,4,5), trapezoidal = TRUE),
               
               "Parameter value2 should be a numeric vector with 3 or 4 values!")
  
  expect_error(MeasureAHD(value1 = testFN1,testFN2, trapezoidal = "b"),
               
               "Parameter trapezoidal should be a single logical value!")
  
  
  
  
  
})
