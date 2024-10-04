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
  
  testMatrix1Res <- matrix(data=c(0.11992398 , 0.188146026 , 0.31481645 , 0.454346750,
                                  -0.40889518  ,0.321100250 , 0.44777067 , 1.001830760,
                                  0.42779912 , 0.603188626 , 1.92344212 , 2.062777662,
                                  -2.23286624 ,-2.042898771 , 0.35867035 , 0.907465245,
                                  -2.56308081 ,-2.064273777 ,-1.23140885, -0.927024725,
                                  0.23239251 , 0.486220708 , 0.61289113 , 1.084108858,
                                  -0.99073392 ,-0.617642400 ,-0.06840412 , 0.008055915,
                                  -0.06076037,  0.007461673 , 0.41849777 , 0.937398067,
                                  -1.30063213 ,-1.275517766 ,-0.46704551 ,-0.386598596,
                                  -3.61698093, -3.278239026 ,-1.14842928, -0.917869269,
                                  -0.55735575, -0.548298205 ,-0.42162779 ,-0.303922994,
                                  -1.65303313, -1.113884835, -0.22295531, -0.142508395,
                                  -2.03769949, -1.698957584 , 0.58819769,  0.609471724,
                                  -1.64892060, -1.310178700 ,-0.03186601 , 0.008898696,
                                  -0.45358837, -0.375611086,  0.51508010 , 1.753746539,
                                  -0.88923590 ,-0.602863385, -0.47619296 ,-0.029626635,
                                  1.57609009,  2.115238384 , 3.00592957 , 3.082389607,
                                  0.90683695 , 1.160665144 , 1.54206699 , 1.736698607,
                                  -0.89694909, -0.608124123 , 0.20034813 , 0.318052920,
                                  -1.69647135, -1.350383708, -0.07207102 , 0.045633775),
                           ncol = 4, byrow = TRUE)
  
  testMatrix2Res <- matrix(data=c(0.11992398 , 0.188146026 , 0.31481645 , 
                                  -0.40889518  ,0.321100250 , 0.44777067 , 
                                  0.42779912 , 0.603188626 , 1.92344212 , 
                                  -2.23286624 ,-2.042898771 , 0.35867035 , 
                                  -2.56308081 ,-2.064273777 ,-1.23140885, 
                                  0.23239251 , 0.486220708 , 0.61289113 , 
                                  -0.99073392 ,-0.617642400 ,-0.06840412 , 
                                  -0.06076037,  0.007461673 , 0.41849777 , 
                                  -1.30063213 ,-1.275517766 ,-0.46704551 ,
                                  -3.61698093, -3.278239026 ,-1.14842928, 
                                  -0.55735575, -0.548298205 ,-0.42162779 ,
                                  -1.65303313, -1.113884835, -0.22295531, 
                                  -2.03769949, -1.698957584 , 0.58819769,  
                                  -1.64892060, -1.310178700 ,-0.03186601 , 
                                  -0.45358837, -0.375611086,  0.51508010 , 
                                  -0.88923590 ,-0.602863385, -0.47619296 ,
                                  1.57609009,  2.115238384 , 3.00592957 , 
                                  0.90683695 , 1.160665144 , 1.54206699 , 
                                  -0.89694909, -0.608124123 , 0.20034813 , 
                                  -1.69647135, -1.350383708, -0.07207102 ),
                           ncol = 3, byrow = TRUE)
  
  testMatrix1Mask <- matrix(data=c(FALSE ,FALSE, FALSE ,FALSE,
                                   FALSE , TRUE ,FALSE ,FALSE,
                                   FALSE ,FALSE, FALSE ,FALSE,
                                   FALSE, FALSE, FALSE, FALSE,
                                   FALSE, FALSE, FALSE, FALSE,
                                   FALSE , TRUE , TRUE, FALSE,
                                   FALSE, FALSE, FALSE , TRUE,
                                   TRUE, FALSE, FALSE ,FALSE,
                                   FALSE ,FALSE ,FALSE , TRUE,
                                   FALSE, FALSE, FALSE ,FALSE,
                                   FALSE , TRUE ,FALSE ,FALSE,
                                   FALSE ,FALSE, FALSE, FALSE,
                                   TRUE, FALSE, FALSE, FALSE,
                                   TRUE, FALSE , TRUE, FALSE,
                                   FALSE ,FALSE , TRUE, FALSE,
                                   FALSE , TRUE, FALSE, FALSE,
                                   TRUE, FALSE, FALSE, FALSE,
                                   FALSE, FALSE, FALSE ,FALSE,
                                   FALSE, FALSE , TRUE , TRUE,
                                   FALSE, FALSE ,FALSE , TRUE),
                            ncol = 4, byrow = TRUE)
  
  testMatrix2Mask <- matrix(data=c(FALSE ,FALSE, FALSE ,
                                   FALSE , TRUE ,FALSE ,
                                   FALSE ,FALSE, FALSE ,
                                   FALSE, FALSE, FALSE, 
                                   FALSE, FALSE, FALSE, 
                                   FALSE , TRUE , TRUE, 
                                   FALSE, FALSE, FALSE , 
                                   TRUE, FALSE, FALSE ,
                                   FALSE ,FALSE ,FALSE , 
                                   FALSE, FALSE, FALSE ,
                                   FALSE , TRUE ,FALSE ,
                                   FALSE ,FALSE, FALSE, 
                                   TRUE, FALSE, FALSE, 
                                   TRUE, FALSE , TRUE, 
                                   FALSE ,FALSE , TRUE, 
                                   FALSE , TRUE, FALSE, 
                                   TRUE, FALSE, FALSE, 
                                   FALSE, FALSE, FALSE ,
                                   FALSE, FALSE , TRUE , 
                                   FALSE, FALSE ,FALSE ),
                            ncol = 3, byrow = TRUE)
  
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      ErrorMatrix(trueData=testMatrix1,imputedData=testMatrix1Res,imputedMask=testMatrix1Mask)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      ErrorMatrix(trueData=testMatrix2,imputedData=testMatrix2Res,imputedMask=testMatrix2Mask)
      
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
  
  testMatrixBadC <- matrix(c(1,2,3,4,
                            -1,3,5,9,
                            0,4,7,1,
                            -1,-3,5,0,
                            7,11,22,15,
                            10,12,14,16),nrow = 6,ncol = 4,byrow = TRUE)
  
  testMatrix2 <- matrix(c(1,2,3,
                          -1,3,5,
                          0,4,7,
                          -1,-3,5,
                          7,11,22),nrow = 5,ncol = 3,byrow = TRUE)
  
  
  testMatrix1Mask <- matrix(c(TRUE,TRUE,TRUE,FALSE,
                              TRUE,TRUE,TRUE,TRUE,
                              TRUE,FALSE,TRUE,FALSE,
                              TRUE,TRUE,TRUE,FALSE,
                              TRUE,TRUE,TRUE,FALSE),nrow = 5,ncol = 4,byrow = TRUE)
  
  # tests
  
  expect_error(ErrorMatrix(trueData=0.53,imputedData=testMatrix1,imputedMask=testMatrix1Mask),
               
               "Parameter trueData should be a data frame or a matrix!")
  
  
  expect_error(ErrorMatrix(trueData=testMatrixBad,imputedData=testMatrix1,imputedMask=testMatrix1Mask),
               
               "Parameter trueData should have numerical values!")
  
  
  expect_error(ErrorMatrix(trueData=testMatrix1,imputedData=testMatrix1,imputedMask=testMatrixBad),
               
               "Parameter imputedMask should have logical values!")
  
  expect_error(ErrorMatrix(trueData=testMatrix1,imputedData=testMatrix2,imputedMask=testMatrix1Mask),
               
               "The parameters trueData, imputedData, imputedMask should have the same number of columns!")
  
  expect_error(ErrorMatrix(trueData=testMatrix1,imputedData=testMatrixBadC,imputedMask=testMatrix1Mask),
               
               "The parameters trueData, imputedData, imputedMask should have the same number of columns!")
  
  
  
  
  
})
