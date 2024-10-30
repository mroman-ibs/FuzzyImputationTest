test_that("Function returns correct values", {
  
  # starting values
  
  testMatrix10NA <- matrix(c(-0.998659999,           NA, -0.126340068 ,          NA, -0.702649549 ,-1.699066011, -0.301233156 ,          NA , 1.161248043,           NA,  0.116372448,
                             -1.274325006,  1.211867970, -1.927880721,  0.618439237, -0.940677980,  0.209705266, -0.759345554, -0.692978617,  0.405306840, -0.853835182,  0.994879585,
                             0.401619878, -0.183290977, -0.115573742, -1.182500705,  0.265799467,           NA,  1.456405083, -0.001873174,  0.518994150, -0.848995095,           NA,
                             -1.497981828,  1.055793836,           NA,           NA, -0.571328407, -0.330465218,  0.880285432,-0.545485389 , 1.574799674 , 0.627165993, -0.042520747,
                             0.003696135, -0.820664650,  0.512902081,  0.683882278,           NA,           NA ,          NA, -0.683944360,  1.919502904, -1.241472339,  1.472316590,
                             -0.623535158,  1.119213099, -0.474394441,           NA , 0.976037808),
                           ncol = 3)
  
  testMatrix1NA <- matrix(c(0.119923979, -0.408895178,  0.427799122, -2.232866244, -2.563080805,  0.232392512, -0.990733919,           NA, -1.300632131, -3.616980930, -0.557355750,
                            -1.653033128 ,          NA ,          NA ,-0.453588370 ,-0.889235900       ,    NA,  0.906836947, -0.896949093 ,-1.696471350 , 0.188146026  ,         NA,
                            0.603188626 ,-2.042898771, -2.064273777,           NA, -0.617642400,  0.007461673, -1.275517766, -3.278239026 ,          NA, -1.113884835 ,-1.698957584,
                            -1.310178700, -0.375611086   ,        NA  ,2.115238384 , 1.160665144, -0.608124123, -1.350383708,  0.314816446 , 0.447770670,  1.923442115 , 0.358670346,
                            -1.231408847  ,         NA, -0.068404124,  0.418497768, -0.467045513, -1.148429281, -0.421627785, -0.222955312 , 0.588197688  ,         NA     ,      NA,
                            -0.476192964 , 3.005929569,  1.542066990  ,         NA ,-0.072071017,  0.454346750 , 1.001830760,  2.062777662 , 0.907465245, -0.927024725,  1.084108858,
                            NA,  0.937398067   ,        NA, -0.917869269, -0.303922994, -0.142508395,  0.609471724,  0.008898696,  1.753746539, -0.029626635,  3.082389607,
                            1.736698607  ,         NA   ,        NA),
                          ncol=4)
  
  testDataFrame10NA <- as.data.frame(testMatrix10NA)
  
  testDataFrame1NA <- as.data.frame(testMatrix1NA)
  
  
  # snapshots tests
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testMatrix1NA)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testMatrix10NA,trapezoidal = FALSE)
      
    }
  )
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testDataFrame1NA)
      
    }
  )
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testDataFrame10NA,trapezoidal = FALSE)
      
    }
  )
  
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testMatrix1NA,method="missForest")
      
    }
  )
  
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testMatrix10NA,method="missForest",trapezoidal = FALSE)
      
    }
  )
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testDataFrame1NA,method="missForest")
      
    }
  )
  
  expect_snapshot(
    
    {
      set.seed(123456)
      
      FuzzyImputation(dataToImpute=testDataFrame10NA,method="missForest",trapezoidal = FALSE)
      
    }
  )
  
  
  
})

