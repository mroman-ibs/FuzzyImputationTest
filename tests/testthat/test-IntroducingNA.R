test_that("Function returns correct values", {
  
  # starting values
  
  # snapshots tests
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testMatrix1 <- matrix(c(1,2,3,4,
                              -1,3,5,9,
                              0,4,7,1,
                              -1,-3,5,0,
                              7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
      
      IntroducingNA(dataMatrix=testMatrix1,percentage=0.1)}
  )
  
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testMatrix1 <- matrix(c(1,2,3,4,
                              -1,3,5,9,
                              0,4,7,1,
                              -1,-3,5,0,
                              7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
      
      IntroducingNA(dataMatrix=testMatrix1,percentage=0.5)}
  )
  
  
  
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testDataFrame1 <- data.frame(a=c(1,-1,0,-1,7),b=c(2,3,4,-3,11),c=c(3,5,7,5,22),d=c(4,9,1,0,15))
      
      IntroducingNA(dataMatrix=testMatrix1,percentage=0.2)}
  )
  
  
  
  
})



test_that("Function reports errors", {
  
  # starting values
  
  set.seed(123456)
  
  testMatrix1 <- matrix(c(1,2,3,4,
                          -1,3,5,9,
                          0,4,7,1,
                          -1,-3,5,0,
                          7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
  
  testMatrixBad <- matrix(c(1,2,3,4,
                          -1,3,5,"c",
                          0,4,7,1,
                          -1,-3,5,0,
                          7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
  
  # tests
  
  expect_error(IntroducingNA(dataMatrix=c(1,2,3),percentage = 0.1),
               
               "Parameter dataMatrix should be a data frame or a matrix or a list!")
  
  
  expect_error(IntroducingNA(dataMatrix=testMatrixBad,percentage = 0.1),
               
               "Parameter dataMatrix should be a numerical matrix or dataframe!")
  
  expect_error(IntroducingNA(dataMatrix=testMatrix1,percentage = -0.2),
               
               "Parameter percentage should be a single real value from the interval <0,1>!")
  
  expect_error(IntroducingNA(dataMatrix=testMatrix1,percentage="c"),
               
               "Parameter percentage should be a single real value from the interval <0,1>!")
  
  expect_error(IntroducingNA(dataMatrix=testMatrix1,percentage=c(1,2,3)),
               
               "Parameter percentage should be a single real value from the interval <0,1>!")
  
  
  
  
})