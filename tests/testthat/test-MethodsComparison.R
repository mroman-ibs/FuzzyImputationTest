test_that("Function returns correct values", {
  
  # starting values
  
  testMatrix1 <- matrix(data=c(0.1199240,  0.188146026,  0.31481645,  0.454346750,
                               -0.4088952, -0.009244923,  0.44777067,  1.001830760,
                               0.4277991,  0.603188626 , 1.92344212 , 2.062777662,
                               -2.2328662 ,-2.042898771 , 0.35867035 , 0.907465245,
                               -2.5630808 ,-2.064273777, -1.23140885, -0.927024725,
                               0.2323925,  0.341050237,  0.89701427,  1.084108858,
                               -0.9907339 ,-0.617642400, -0.06840412,  0.117407837,
                               -0.4229014,  0.007461673 , 0.41849777,  0.937398067,
                               -1.3006321 ,-1.275517766, -0.46704551, -0.436057542,
                               -3.6169809 ,-3.278239026, -1.14842928 ,-0.917869269,
                               -0.5573558 ,-0.515324252 ,-0.42162779 ,-0.303922994,
                               -1.6530331 ,-1.113884835, -0.22295531 ,-0.142508395,
                               -1.7779261 ,-1.698957584,  0.58819769 , 0.609471724,
                               -1.6492244 ,-1.310178700, -0.08413849 , 0.008898696,
                               -0.4535884 ,-0.375611086,  1.18356358 , 1.753746539,
                               -0.8892359 ,-0.816145242, -0.47619296 ,-0.029626635,
                               1.5656538 , 2.115238384,  3.00592957,  3.082389607,
                               0.9068369 , 1.160665144 , 1.54206699 , 1.736698607,
                               -0.8969491, -0.608124123 ,-0.30055712,  0.287914788,
                               -1.6964713 ,-1.350383708, -0.07207102 , 0.289067511),
                        ncol = 4, byrow = TRUE)
  
  colnames(testMatrix1) <- paste0("X",seq(1,4,by=1))
  
  testDF1 <- as.data.frame(testMatrix1)
  
  testList1 <- MatrixToFuzzyNumbers(testMatrix1)
  
  testMatrix2 <- matrix(data=c(0.1199240,  0.188146026,  0.31481645,  
                               -0.4088952, -0.009244923,  0.44777067, 
                               0.4277991,  0.603188626 , 1.92344212 , 
                               -2.2328662 ,-2.042898771 , 0.35867035 , 
                               -2.5630808 ,-2.064273777, -1.23140885, 
                               0.2323925,  0.341050237,  0.89701427,  
                               -0.9907339 ,-0.617642400, -0.06840412,  
                               -0.4229014,  0.007461673 , 0.41849777,  
                               -1.3006321 ,-1.275517766, -0.46704551, 
                               -3.6169809 ,-3.278239026, -1.14842928 ,
                               -0.5573558 ,-0.515324252 ,-0.42162779 ,
                               -1.6530331 ,-1.113884835, -0.22295531 ,
                               -1.7779261 ,-1.698957584,  0.58819769 , 
                               -1.6492244 ,-1.310178700, -0.08413849 , 
                               -0.4535884 ,-0.375611086,  1.18356358 , 
                               -0.8892359 ,-0.816145242, -0.47619296 ,
                               1.5656538 , 2.115238384,  3.00592957,  
                               0.9068369 , 1.160665144 , 1.54206699 , 
                               -0.8969491, -0.608124123 ,-0.30055712, 
                               -1.6964713 ,-1.350383708, -0.07207102 ),
                        ncol = 3, byrow = TRUE)
  
  colnames(testMatrix2) <- paste0("X",seq(1,3,by=1))
  
  testDF2 <- as.data.frame(testMatrix2)
  
  testList2 <- MatrixToFuzzyNumbers(testMatrix2)
  
  
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      MethodsComparison(trueData = testMatrix1,iterations=2,percentage = 0.1,verbose = FALSE)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      MethodsComparison(trueData = testDF1,iterations=2,percentage = 0.1,verbose = FALSE)
      
    }
  )
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      MethodsComparison(trueData = testList1,iterations=2,percentage = 0.1,verbose = FALSE)
      
    }
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
  
  testMatrixBad <- matrix(c(1,2,3,"c",
                            -1,3,5,9,
                            0,4,7,1,
                            -1,-3,5,0,
                            7,11,22,15),nrow = 5,ncol = 4,byrow = TRUE)
  
  testMatrix2 <- matrix(c(1,2,3,
                          -1,3,5,
                          0,4,7,
                          -1,-3,5,
                          7,11,22),nrow = 5,ncol = 3,byrow = TRUE)
  
  testMatrixBadC <- matrix(c(1,2,3,4,
                             -1,3,5,9,
                             0,4,7,1,
                             -1,-3,5,0,
                             7,11,22,15,
                             10,12,14,16),nrow = 6,ncol = 4,byrow = TRUE)
  
  
  testMatrix1Mask <- matrix(c(TRUE,TRUE,TRUE,FALSE,
                              TRUE,TRUE,TRUE,TRUE,
                              TRUE,FALSE,TRUE,FALSE,
                              TRUE,TRUE,TRUE,FALSE,
                              TRUE,TRUE,TRUE,FALSE),nrow = 5,ncol = 4,byrow = TRUE)
  
  # tests
  
  expect_error(MethodsComparison(trueData=0.53),
               
               "Parameter trueData should be a data frame or a matrix or a list!")
  
  
  expect_error(MethodsComparison(trueData=testMatrixBad),
               
               "Parameter trueData should have numerical values!")
  
  expect_error(MethodsComparison(trueData=testMatrix1,trapezoidal=c(12,3)),
               
               "Parameter trapezoidal should be a single logical value!")
  
  expect_error(MethodsComparison(trueData=testMatrix1,trapezoidal=TRUE,verbose = 0.5),
               
               "Parameter verbose should be a single logical value!")
  
  expect_error(MethodsComparison(trueData=testMatrix1,iterations=0.5,trapezoidal=TRUE,verbose = TRUE),
               
               "Parameter iterations should be a single integer value greater than 0!")
  
  expect_error(MethodsComparison(trueData=testMatrix1,iterations=-10,trapezoidal=TRUE,verbose = TRUE),
               
               "Parameter iterations should be a single integer value greater than 0!")
  
  expect_error(MethodsComparison(trueData=testMatrix1,iterations=10,percentage=1.3,trapezoidal=TRUE,verbose = TRUE),
               
               "Parameter percentage should be a single numeric value from the interval <0,1>!")
  
  
  
})


