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
      
      FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.2,supportFactor=0.2,trapezoidal=TRUE)}
  )
  
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testMatrix1 <- matrix(c(1,2,3,4,
                              -1,3,5,9,
                              0,4,7,1,
                              -1,-3,5,0,
                              7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
      
      FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.2,supportFactor=0.2,trapezoidal=FALSE)}
  )
  
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testMatrix1 <- matrix(c(1,2,3,4,
                              -1,3,5,9,
                              0,4,7,1,
                              -1,-3,5,0,
                              7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
      
      FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.3,supportFactor=0.5,trapezoidal=TRUE)}
  )
  
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testDataFrame1 <- data.frame(a=c(1,-1,0,-1,7),b=c(2,3,4,-3,11),b=c(3,5,7,5,22),d=c(4,9,1,0,15))
      
      FuzzifyMatrix(crispMatrix=testDataFrame1,coreFactor=0.2,supportFactor=0.5,trapezoidal=TRUE)}
  )
  
  
  expect_snapshot(
    
    {set.seed(123456)
      
      testDataFrame1 <- data.frame(a=c(1,-1,0,-1,7),b=c(2,3,4,-3,11),b=c(3,5,7,5,22),d=c(4,9,1,0,15))
      
      FuzzifyMatrix(crispMatrix=testDataFrame1,coreFactor=0.4,supportFactor=0.3,trapezoidal=FALSE)}
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
  
  # tests
  
  expect_error(FuzzifyMatrix(crispMatrix="c",coreFactor=0.1,supportFactor=0.2,trapezoidal=TRUE),
               
               "Parameter crispMatrix should be a data frame or a matrix!")
  
  expect_error(FuzzifyMatrix(crispMatrix=list(1,2,3),coreFactor=0.1,supportFactor=0.2,trapezoidal=TRUE),
               
               "Parameter crispMatrix should be a data frame or a matrix!")
  
  expect_error(FuzzifyMatrix(crispMatrix=c(1,2,3),coreFactor=0.1,supportFactor=0.2,trapezoidal=TRUE),
               
               "Parameter crispMatrix should be a data frame or a matrix!")
  
  expect_error(FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=-1,supportFactor=0.2,trapezoidal=TRUE),
               
               "Parameter coreFactor should be a single positive real value!")
  
  expect_error(FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=c(2,3),supportFactor=0.2,trapezoidal=TRUE),
               
               "Parameter coreFactor should be a single positive real value!")
  
  expect_error(FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.5,supportFactor=-0.2,trapezoidal=TRUE),
               
               "Parameter supportFactor should be a single positive real value!")
  
  expect_error(FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.5,supportFactor=list(1,2,3),trapezoidal=TRUE),
               
               "Parameter supportFactor should be a single positive real value!")
  
  
  expect_error(FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.5,supportFactor=.2,trapezoidal="s"),
               
               "Parameter trapezoidal should be a single logical value!")
  
  
  expect_error(FuzzifyMatrix(crispMatrix=testMatrix1,coreFactor=0.5,supportFactor=.2,trapezoidal=list(TRUE,FALSE)),
               
               "Parameter trapezoidal should be a single logical value!")
  
  
  
  
})
