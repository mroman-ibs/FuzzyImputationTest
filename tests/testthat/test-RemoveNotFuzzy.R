test_that("Function returns correct values", {
  
  # starting values
  
  testMatrix1 <- matrix(c(1,2,3,4,
                          -1,3,5,9,
                          0,4,7,1,
                          -1,-3,5,0,
                          7,11,22,26),nrow = 5,ncol = 4,byrow = TRUE)
  
  testMatrix1Bad <- matrix(c(1,2,3,4,
                          -1,3,5,0,
                          5,4,7,1,
                          -1,-3,5,6,
                          7,11,22,10),nrow = 5,ncol = 4,byrow = TRUE)
  
  
  testMatrix2 <- matrix(c(1,2,3,
                          -1,3,5,
                          0,4,7,
                          -1,-3,5,
                          7,11,22),nrow = 5,ncol = 3,byrow = TRUE)
  
  testMatrix2Bad <- matrix(c(1,7,3,
                             -1,3,5,
                             0,4,7,
                             -1,-3,5,
                             12,11,22),nrow = 5,ncol = 3,byrow = TRUE)
  
  testDataFrame1 <- data.frame(x1=c(-2,-1,0,7),
                               x2=c(2,3,4,11),
                               x3=c(3,5,7,22),
                               x4=c(4,9,10,15))
  
  testDataFrame1Bad <- data.frame(x1=c(-2,-1,0,7),
                               x2=c(5,3,4,11),
                               x3=c(3,10,7,22),
                               x4=c(4,9,10,23))
  
  testDataFrame2 <- data.frame(x1=c(-2,-1,0,7),
                               x2=c(2,3,4,11),
                               x3=c(3,5,7,22))
  
  testDataFrame2Bad <- data.frame(x1=c(-2,5,0,7),
                                  x2=c(10,3,4,11),
                                  x3=c(3,5,7,10))
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      RemoveNotFuzzy(trueData = testMatrix1, imputedData = testMatrix1Bad, trapezoidal = TRUE)
      
    }
  )
  
  expect_snapshot(
    
    {
      RemoveNotFuzzy(trueData = testMatrix2, imputedData = testMatrix2Bad, trapezoidal = FALSE)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      RemoveNotFuzzy(trueData = testDataFrame1, imputedData = testDataFrame1Bad, trapezoidal = TRUE)
      
    }
  )
  
  expect_snapshot(
    
    {
      RemoveNotFuzzy(trueData = testDataFrame2, imputedData = testDataFrame2Bad, trapezoidal = FALSE)
      
    }
  )
  
  
  
  
  
})



test_that("Function reports errors", {
  
  # starting values
  
  testMatrix1 <- matrix(c(1,2,3,4,
                          -1,3,5,9,
                          0,4,7,1,
                          -1,-3,5,0,
                          7,11,22,26),nrow = 5,ncol = 4,byrow = TRUE)
  
  testMatrix1Bad <- matrix(c(1,2,3,4,
                             -1,3,5,0,
                             5,4,7,1,
                             -1,-3,5,6,
                             7,11,22,10),nrow = 5,ncol = 4,byrow = TRUE)
  
  
  testMatrix2 <- matrix(c(1,2,3,
                          -1,3,5,
                          0,4,7,
                          -1,-3,5,
                          7,11,22),nrow = 5,ncol = 3,byrow = TRUE)
  
  testMatrix2Bad <- matrix(c(1,7,3,
                             -1,3,5,
                             0,4,7,
                             -1,-3,5,
                             12,11,22),nrow = 5,ncol = 3,byrow = TRUE)
  
  testDataFrame1 <- data.frame(x1=c(-2,-1,0,7),
                               x2=c(2,3,4,11),
                               x3=c(3,5,7,22),
                               x4=c(4,9,10,15))
  
  testDataFrame1Bad <- data.frame(x1=c(-2,-1,0,7),
                                  x2=c(5,3,4,11),
                                  x3=c(3,10,7,22),
                                  x4=c(4,9,10,23))
  
  testDataFrame2 <- data.frame(x1=c(-2,-1,0,7),
                               x2=c(2,3,4,11),
                               x3=c(3,5,7,22))
  
  testDataFrame2Bad <- data.frame(x1=c(-2,5,0,7),
                                  x2=c(10,3,4,11),
                                  x3=c(3,5,7,10))
  
  testMatrixChar <- matrix(c(1,2,3,4,
                          -1,3,5,9,
                          0,4,7,"c",
                          -1,-3,5,0,
                          7,11,22,26),nrow = 5,ncol = 4,byrow = TRUE)
  
  testMatrix1Col <- matrix(c(1,2,3,4,5,
                             -1,3,5,0,6,
                             5,4,7,1,7,
                             -1,-3,5,6,10,
                             7,11,22,10,11),nrow = 5,ncol = 5,byrow = TRUE)
  
  
  # tests
  
  expect_error(RemoveNotFuzzy(trueData=c(1,2,3),imputedData = testMatrix2Bad),
               
               "Parameter trueData should be a data frame, a matrix or a list!")
  
  expect_error(RemoveNotFuzzy(trueData=testMatrix1,imputedData = NA),
               
               "Parameter imputedData should be a data frame or a matrix!")
  
  expect_error(RemoveNotFuzzy(trueData=testMatrix1,imputedData = testMatrix2Bad,trapezoidal="b"),
               
               "Parameter trapezoidal should be a single logical value!")
  
  
  expect_error(RemoveNotFuzzy(trueData=testMatrix1,imputedData = testMatrix2Bad,trapezoidal = NA),
               
               "Parameter trapezoidal should be a single logical value!")
  
  expect_error(RemoveNotFuzzy(trueData=testMatrix1Col,imputedData = testMatrix1Bad,trapezoidal=TRUE),
               
               "For trapezoidal fuzzy numbers, the parameter trueData should have a multiple of 4 columns!")
  
  expect_error(RemoveNotFuzzy(trueData=testMatrix1Col,imputedData = testMatrix1Bad,trapezoidal=FALSE),
               
               "For triangular fuzzy numbers, the parameter trueData should have a multiple of 3 columns!")
  
  
  
  
})

