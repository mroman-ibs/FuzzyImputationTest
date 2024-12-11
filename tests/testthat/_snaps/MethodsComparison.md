# Function returns correct values

    Code
      set.seed(123456)
      MethodsComparison(trueData = testMatrix1, iterations = 2, percentage = 0.1,
        verbose = FALSE)
    Output
      $dimp
      $dimp$nonFNNumbers
        V1 mean 
       0.5  0.5 
      
      $dimp$errorMatrix
                    X1        X2         X3        X4      mean
      MAE   0.17424084 0.7350063  1.1033438 0.3937679 0.6015897
      WMAE  0.15625127 0.7539969  3.4628738 0.2527175 1.1564599
      MSE   0.03712528 0.7583725  1.5120639 0.4424950 0.6875141
      WMSE  0.05184381 0.7097546 16.4196927 0.1489231 4.3325535
      NRMSE 0.08459201 0.3629490  0.5574172 0.2646539 0.3174030
      
      $dimp$statisticalMeasures
                                  X1           X2          X3         X4        mean
      TrueMean           -1.89181983 -0.144243172  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.84097570 -0.009525658 -0.58783694 0.24345351 -0.54872120
      AbsDiffTrueImpMean  0.12811116  0.735006275  1.10334381 0.38891096  0.58884305
      GenMean            -0.89286341 -0.633033805  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.88777900 -0.619562054  0.19902257 0.53993456 -0.19209598
      AbsDiffGenImpMean   0.01281112  0.073500628  0.11033438 0.03889110  0.05888431
      TrueSD              1.08174083  1.434092909  0.52378759 1.11236840  1.03799743
      ImpSD               0.90056429  1.124215541  0.50012062 0.74973180  0.81865806
      AbsDiffTrueImpSD    0.18117654  0.551321033  0.28168450 0.36263659  0.34420467
      GenSD               1.23204855  1.227368275  1.02518086 1.02063125  1.12630723
      GenImpSD            1.21293436  1.199221314  1.06303112 1.00351659  1.11967585
      AbsDiffGenImpSD     0.01911419  0.028146961  0.04564605 0.02084513  0.02843808
      
      $dimp$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6826278 0.6826278
      true+res-anti    0.6421881 0.6421881
      imputed+avs-anti 0.3901666 0.3901666
      imputed+ms-anti  0.4835689 0.4835689
      imputed+res-anti 0.4369346 0.4369346
      parts+avs-anti   0.7922772 0.7922772
      parts+ms-anti    0.7892953 0.7892953
      parts+res-anti   0.7902029 0.7902029
      
      $dimp$fuzzyMeasures
                        V1       mean
      Euclidean 0.02611859 0.02611859
      AHD       0.15080981 0.15080981
      HSD       0.10809360 0.10809360
      
      
      $missForest
      $missForest$nonFNNumbers
        V1 mean 
         1    1 
      
      $missForest$errorMatrix
                   X1        X2        X3        X4      mean
      MAE   0.5634337 0.6671421 0.6167042 0.4859673 0.5833118
      WMAE  0.3414464 0.6032638 1.9762607 0.4064291 0.8318500
      MSE   0.6256070 0.6135548 0.4213471 0.3298363 0.4975863
      WMSE  0.1916301 0.4085520 5.5488562 0.2321680 1.5953015
      NRMSE 0.3057540 0.3297853 0.3132873 0.2868184 0.3089113
      
      $missForest$statisticalMeasures
                                  X1          X2          X3         X4        mean
      TrueMean           -1.89181983 -0.14424317  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.53963946 -0.24731609 -0.10119736 0.71111260 -0.29426008
      AbsDiffTrueImpMean  0.48153809  0.66714211  0.61670424 0.48166663  0.56176277
      GenMean            -0.89286341 -0.63303381  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.85764538 -0.64334110  0.24768653 0.58670047 -0.16664987
      AbsDiffGenImpMean   0.04815381  0.06671421  0.06167042 0.04816666  0.05617628
      TrueSD              1.08174083  1.43409291  0.52378759 1.11236840  1.03799743
      ImpSD               0.39829267  0.91736048  0.38776096 0.73796524  0.61034484
      AbsDiffTrueImpSD    0.68344816  0.51673242  0.20844264 0.37440316  0.44575660
      GenSD               1.23204855  1.22736828  1.02518086 1.02063125  1.12630723
      GenImpSD            1.15680378  1.15661550  1.02457946 0.99089053  1.08222232
      AbsDiffGenImpSD     0.07524477  0.07075277  0.01293635 0.02974072  0.04716866
      
      $missForest$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6622539 0.6622539
      true+res-anti    0.6636708 0.6636708
      imputed+avs-anti 0.4783835 0.4783835
      imputed+ms-anti  0.5589371 0.5589371
      imputed+res-anti 0.6111803 0.6111803
      parts+avs-anti   0.4980159 0.4980159
      parts+ms-anti    0.7583838 0.7583838
      parts+res-anti   0.7404185 0.7404185
      
      $missForest$fuzzyMeasures
                         V1        mean
      Euclidean 0.001031675 0.001031675
      AHD       0.014513224 0.014513224
      HSD       0.008293271 0.008293271
      
      
      $miceRanger
      $miceRanger$nonFNNumbers
        V1 mean 
       1.5  1.5 
      
      $miceRanger$errorMatrix
                   X1        X2        X3        X4      mean
      MAE   0.5644528 0.9808929 0.7061296 0.5212157 0.6931727
      WMAE  0.2887787 0.9442214 1.9916242 0.4995107 0.9310338
      MSE   0.8740102 1.3589836 0.5738746 0.3877424 0.7986527
      WMSE  0.1535998 1.2447005 4.9395065 0.3494964 1.6718258
      NRMSE 0.3376719 0.4924162 0.3638808 0.3109525 0.3762304
      
      $miceRanger$statisticalMeasures
                                  X1          X2          X3         X4        mean
      TrueMean           -1.89181983 -0.14424317  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.49330705 -0.22614784 -0.19062269 0.70426686 -0.30145268
      AbsDiffTrueImpMean  0.54273373  0.98089293  0.70612956 0.33107987  0.64020902
      GenMean            -0.89286341 -0.63303381  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.85301214 -0.64122427  0.23874399 0.58601589 -0.16736913
      AbsDiffGenImpMean   0.05427337  0.09808929  0.07061296 0.03310799  0.06402090
      TrueSD              1.08174083  1.43409291  0.52378759 1.11236840  1.03799743
      ImpSD               0.31419943  0.69401962  0.27966406 0.37525811  0.41578530
      AbsDiffTrueImpSD    0.76754139  0.74007329  0.24412354 0.73711029  0.62221213
      GenSD               1.23204855  1.22736828  1.02518086 1.02063125  1.12630723
      GenImpSD            1.15156705  1.14172701  1.02714754 0.97929296  1.07493364
      AbsDiffGenImpSD     0.08048150  0.08564127  0.01736889 0.04133829  0.05620749
      
      $miceRanger$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6981216 0.6981216
      true+res-anti    0.6561468 0.6561468
      imputed+avs-anti 0.4493403 0.4493403
      imputed+ms-anti  0.4925790 0.4925790
      imputed+res-anti 0.4797352 0.4797352
      parts+avs-anti   0.6011905 0.6011905
      parts+ms-anti    0.6785823 0.6785823
      parts+res-anti   0.6484343 0.6484343
      
      $miceRanger$fuzzyMeasures
                         V1        mean
      Euclidean 0.000656723 0.000656723
      AHD       0.011579332 0.011579332
      HSD       0.006616761 0.006616761
      
      
      $knn
      $knn$nonFNNumbers
        V1 mean 
       3.5  3.5 
      
      $knn$errorMatrix
                   X1        X2        X3        X4      mean
      MAE   0.9628259 0.9928994 0.4856660 0.6604259 0.7754543
      WMAE  0.6101618 1.1586596 1.5686256 0.7012439 1.0096727
      MSE   1.5578272 1.4495443 0.2641178 0.5973828 0.9672180
      WMSE  0.5743589 2.1203726 3.6504932 0.7224116 1.7669091
      NRMSE 0.4957328 0.4899888 0.2480280 0.3835187 0.4043171
      
      $knn$statisticalMeasures
                                  X1          X2          X3         X4        mean
      TrueMean           -1.89181983 -0.14424317 0.515506875 0.63236446 -0.22204791
      ImpMean            -1.30063210 -0.45335331 0.029840897 0.57540693 -0.28718439
      AbsDiffTrueImpMean  0.76490628  0.99289944 0.485665977 0.61928687  0.71568964
      GenMean            -0.89286341 -0.63303381 0.309356951 0.57882565 -0.15942865
      GenImpMean         -0.83374464 -0.66394482 0.260790353 0.57312990 -0.16594230
      AbsDiffGenImpMean   0.07649063  0.09928994 0.048566598 0.06192869  0.07156896
      TrueSD              1.08174083  1.43409291 0.523787595 1.11236840  1.03799743
      ImpSD               0.00000000  1.07293337 0.314222276 0.60305436  0.49755250
      AbsDiffTrueImpSD    1.08174083  0.77836727 0.209565318 0.50931403  0.64474686
      GenSD               1.23204855  1.22736828 1.025180858 1.02063125  1.12630723
      GenImpSD            1.13377670  1.15133097 1.020977144 0.97747236  1.07088930
      AbsDiffGenImpSD     0.09827186  0.09076027 0.008489656 0.04315889  0.06017017
      
      $knn$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6882526 0.6882526
      true+res-anti    0.6683359 0.6683359
      imputed+avs-anti 0.5237395 0.5237395
      imputed+ms-anti  0.6137479 0.6137479
      imputed+res-anti 0.5428655 0.5428655
      parts+avs-anti   0.6357143 0.6357143
      parts+ms-anti    0.6695952 0.6695952
      parts+res-anti   0.7176667 0.7176667
      
      $knn$fuzzyMeasures
                V1 mean
      Euclidean  0    0
      AHD        0    0
      HSD        0    0
      
      

---

    Code
      set.seed(123456)
      MethodsComparison(trueData = testDF1, iterations = 2, percentage = 0.1,
        verbose = FALSE)
    Output
      $dimp
      $dimp$nonFNNumbers
        V1 mean 
       0.5  0.5 
      
      $dimp$errorMatrix
                    V1        V2         V3        V4      mean
      MAE   0.17424084 0.7350063  1.1033438 0.3937679 0.6015897
      WMAE  0.15625127 0.7539969  3.4628738 0.2527175 1.1564599
      MSE   0.03712528 0.7583725  1.5120639 0.4424950 0.6875141
      WMSE  0.05184381 0.7097546 16.4196927 0.1489231 4.3325535
      NRMSE 0.08459201 0.3629490  0.5574172 0.2646539 0.3174030
      
      $dimp$statisticalMeasures
                                  V1           V2          V3         V4        mean
      TrueMean           -1.89181983 -0.144243172  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.84097570 -0.009525658 -0.58783694 0.24345351 -0.54872120
      AbsDiffTrueImpMean  0.12811116  0.735006275  1.10334381 0.38891096  0.58884305
      GenMean            -0.89286341 -0.633033805  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.88777900 -0.619562054  0.19902257 0.53993456 -0.19209598
      AbsDiffGenImpMean   0.01281112  0.073500628  0.11033438 0.03889110  0.05888431
      TrueSD              1.08174083  1.434092909  0.52378759 1.11236840  1.03799743
      ImpSD               0.90056429  1.124215541  0.50012062 0.74973180  0.81865806
      AbsDiffTrueImpSD    0.18117654  0.551321033  0.28168450 0.36263659  0.34420467
      GenSD               1.23204855  1.227368275  1.02518086 1.02063125  1.12630723
      GenImpSD            1.21293436  1.199221314  1.06303112 1.00351659  1.11967585
      AbsDiffGenImpSD     0.01911419  0.028146961  0.04564605 0.02084513  0.02843808
      
      $dimp$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6826278 0.6826278
      true+res-anti    0.6421881 0.6421881
      imputed+avs-anti 0.3901666 0.3901666
      imputed+ms-anti  0.4835689 0.4835689
      imputed+res-anti 0.4369346 0.4369346
      parts+avs-anti   0.7922772 0.7922772
      parts+ms-anti    0.7892953 0.7892953
      parts+res-anti   0.7902029 0.7902029
      
      $dimp$fuzzyMeasures
                        V1       mean
      Euclidean 0.02611859 0.02611859
      AHD       0.15080981 0.15080981
      HSD       0.10809360 0.10809360
      
      
      $missForest
      $missForest$nonFNNumbers
        V1 mean 
         1    1 
      
      $missForest$errorMatrix
                   V1        V2        V3        V4      mean
      MAE   0.5634337 0.6671421 0.6167042 0.4859673 0.5833118
      WMAE  0.3414464 0.6032638 1.9762607 0.4064291 0.8318500
      MSE   0.6256070 0.6135548 0.4213471 0.3298363 0.4975863
      WMSE  0.1916301 0.4085520 5.5488562 0.2321680 1.5953015
      NRMSE 0.3057540 0.3297853 0.3132873 0.2868184 0.3089113
      
      $missForest$statisticalMeasures
                                  V1          V2          V3         V4        mean
      TrueMean           -1.89181983 -0.14424317  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.53963946 -0.24731609 -0.10119736 0.71111260 -0.29426008
      AbsDiffTrueImpMean  0.48153809  0.66714211  0.61670424 0.48166663  0.56176277
      GenMean            -0.89286341 -0.63303381  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.85764538 -0.64334110  0.24768653 0.58670047 -0.16664987
      AbsDiffGenImpMean   0.04815381  0.06671421  0.06167042 0.04816666  0.05617628
      TrueSD              1.08174083  1.43409291  0.52378759 1.11236840  1.03799743
      ImpSD               0.39829267  0.91736048  0.38776096 0.73796524  0.61034484
      AbsDiffTrueImpSD    0.68344816  0.51673242  0.20844264 0.37440316  0.44575660
      GenSD               1.23204855  1.22736828  1.02518086 1.02063125  1.12630723
      GenImpSD            1.15680378  1.15661550  1.02457946 0.99089053  1.08222232
      AbsDiffGenImpSD     0.07524477  0.07075277  0.01293635 0.02974072  0.04716866
      
      $missForest$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6622539 0.6622539
      true+res-anti    0.6636708 0.6636708
      imputed+avs-anti 0.4783835 0.4783835
      imputed+ms-anti  0.5589371 0.5589371
      imputed+res-anti 0.6111803 0.6111803
      parts+avs-anti   0.4980159 0.4980159
      parts+ms-anti    0.7583838 0.7583838
      parts+res-anti   0.7404185 0.7404185
      
      $missForest$fuzzyMeasures
                         V1        mean
      Euclidean 0.001031675 0.001031675
      AHD       0.014513224 0.014513224
      HSD       0.008293271 0.008293271
      
      
      $miceRanger
      $miceRanger$nonFNNumbers
        V1 mean 
       1.5  1.5 
      
      $miceRanger$errorMatrix
                   V1        V2        V3        V4      mean
      MAE   0.5644528 0.9808929 0.7061296 0.5212157 0.6931727
      WMAE  0.2887787 0.9442214 1.9916242 0.4995107 0.9310338
      MSE   0.8740102 1.3589836 0.5738746 0.3877424 0.7986527
      WMSE  0.1535998 1.2447005 4.9395065 0.3494964 1.6718258
      NRMSE 0.3376719 0.4924162 0.3638808 0.3109525 0.3762304
      
      $miceRanger$statisticalMeasures
                                  V1          V2          V3         V4        mean
      TrueMean           -1.89181983 -0.14424317  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.49330705 -0.22614784 -0.19062269 0.70426686 -0.30145268
      AbsDiffTrueImpMean  0.54273373  0.98089293  0.70612956 0.33107987  0.64020902
      GenMean            -0.89286341 -0.63303381  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.85301214 -0.64122427  0.23874399 0.58601589 -0.16736913
      AbsDiffGenImpMean   0.05427337  0.09808929  0.07061296 0.03310799  0.06402090
      TrueSD              1.08174083  1.43409291  0.52378759 1.11236840  1.03799743
      ImpSD               0.31419943  0.69401962  0.27966406 0.37525811  0.41578530
      AbsDiffTrueImpSD    0.76754139  0.74007329  0.24412354 0.73711029  0.62221213
      GenSD               1.23204855  1.22736828  1.02518086 1.02063125  1.12630723
      GenImpSD            1.15156705  1.14172701  1.02714754 0.97929296  1.07493364
      AbsDiffGenImpSD     0.08048150  0.08564127  0.01736889 0.04133829  0.05620749
      
      $miceRanger$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6981216 0.6981216
      true+res-anti    0.6561468 0.6561468
      imputed+avs-anti 0.4493403 0.4493403
      imputed+ms-anti  0.4925790 0.4925790
      imputed+res-anti 0.4797352 0.4797352
      parts+avs-anti   0.6011905 0.6011905
      parts+ms-anti    0.6785823 0.6785823
      parts+res-anti   0.6484343 0.6484343
      
      $miceRanger$fuzzyMeasures
                         V1        mean
      Euclidean 0.000656723 0.000656723
      AHD       0.011579332 0.011579332
      HSD       0.006616761 0.006616761
      
      
      $knn
      $knn$nonFNNumbers
        V1 mean 
       3.5  3.5 
      
      $knn$errorMatrix
                   V1        V2        V3        V4      mean
      MAE   0.9628259 0.9928994 0.4856660 0.6604259 0.7754543
      WMAE  0.6101618 1.1586596 1.5686256 0.7012439 1.0096727
      MSE   1.5578272 1.4495443 0.2641178 0.5973828 0.9672180
      WMSE  0.5743589 2.1203726 3.6504932 0.7224116 1.7669091
      NRMSE 0.4957328 0.4899888 0.2480280 0.3835187 0.4043171
      
      $knn$statisticalMeasures
                                  V1          V2          V3         V4        mean
      TrueMean           -1.89181983 -0.14424317 0.515506875 0.63236446 -0.22204791
      ImpMean            -1.30063210 -0.45335331 0.029840897 0.57540693 -0.28718439
      AbsDiffTrueImpMean  0.76490628  0.99289944 0.485665977 0.61928687  0.71568964
      GenMean            -0.89286341 -0.63303381 0.309356951 0.57882565 -0.15942865
      GenImpMean         -0.83374464 -0.66394482 0.260790353 0.57312990 -0.16594230
      AbsDiffGenImpMean   0.07649063  0.09928994 0.048566598 0.06192869  0.07156896
      TrueSD              1.08174083  1.43409291 0.523787595 1.11236840  1.03799743
      ImpSD               0.00000000  1.07293337 0.314222276 0.60305436  0.49755250
      AbsDiffTrueImpSD    1.08174083  0.77836727 0.209565318 0.50931403  0.64474686
      GenSD               1.23204855  1.22736828 1.025180858 1.02063125  1.12630723
      GenImpSD            1.13377670  1.15133097 1.020977144 0.97747236  1.07088930
      AbsDiffGenImpSD     0.09827186  0.09076027 0.008489656 0.04315889  0.06017017
      
      $knn$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6882526 0.6882526
      true+res-anti    0.6683359 0.6683359
      imputed+avs-anti 0.5237395 0.5237395
      imputed+ms-anti  0.6137479 0.6137479
      imputed+res-anti 0.5428655 0.5428655
      parts+avs-anti   0.6357143 0.6357143
      parts+ms-anti    0.6695952 0.6695952
      parts+res-anti   0.7176667 0.7176667
      
      $knn$fuzzyMeasures
                V1 mean
      Euclidean  0    0
      AHD        0    0
      HSD        0    0
      
      

---

    Code
      set.seed(123456)
      MethodsComparison(trueData = testList1, iterations = 2, percentage = 0.1,
        verbose = FALSE)
    Output
      $dimp
      $dimp$nonFNNumbers
        V1 mean 
       0.5  0.5 
      
      $dimp$errorMatrix
                    X1        X2         X3        X4      mean
      MAE   0.17424084 0.7350063  1.1033438 0.3937679 0.6015897
      WMAE  0.15625127 0.7539969  3.4628738 0.2527175 1.1564599
      MSE   0.03712528 0.7583725  1.5120639 0.4424950 0.6875141
      WMSE  0.05184381 0.7097546 16.4196927 0.1489231 4.3325535
      NRMSE 0.08459201 0.3629490  0.5574172 0.2646539 0.3174030
      
      $dimp$statisticalMeasures
                                  X1           X2          X3         X4        mean
      TrueMean           -1.89181983 -0.144243172  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.84097570 -0.009525658 -0.58783694 0.24345351 -0.54872120
      AbsDiffTrueImpMean  0.12811116  0.735006275  1.10334381 0.38891096  0.58884305
      GenMean            -0.89286341 -0.633033805  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.88777900 -0.619562054  0.19902257 0.53993456 -0.19209598
      AbsDiffGenImpMean   0.01281112  0.073500628  0.11033438 0.03889110  0.05888431
      TrueSD              1.08174083  1.434092909  0.52378759 1.11236840  1.03799743
      ImpSD               0.90056429  1.124215541  0.50012062 0.74973180  0.81865806
      AbsDiffTrueImpSD    0.18117654  0.551321033  0.28168450 0.36263659  0.34420467
      GenSD               1.23204855  1.227368275  1.02518086 1.02063125  1.12630723
      GenImpSD            1.21293436  1.199221314  1.06303112 1.00351659  1.11967585
      AbsDiffGenImpSD     0.01911419  0.028146961  0.04564605 0.02084513  0.02843808
      
      $dimp$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6826278 0.6826278
      true+res-anti    0.6421881 0.6421881
      imputed+avs-anti 0.3901666 0.3901666
      imputed+ms-anti  0.4835689 0.4835689
      imputed+res-anti 0.4369346 0.4369346
      parts+avs-anti   0.7922772 0.7922772
      parts+ms-anti    0.7892953 0.7892953
      parts+res-anti   0.7902029 0.7902029
      
      $dimp$fuzzyMeasures
                        V1       mean
      Euclidean 0.02611859 0.02611859
      AHD       0.15080981 0.15080981
      HSD       0.10809360 0.10809360
      
      
      $missForest
      $missForest$nonFNNumbers
        V1 mean 
         1    1 
      
      $missForest$errorMatrix
                   X1        X2        X3        X4      mean
      MAE   0.5634337 0.6671421 0.6167042 0.4859673 0.5833118
      WMAE  0.3414464 0.6032638 1.9762607 0.4064291 0.8318500
      MSE   0.6256070 0.6135548 0.4213471 0.3298363 0.4975863
      WMSE  0.1916301 0.4085520 5.5488562 0.2321680 1.5953015
      NRMSE 0.3057540 0.3297853 0.3132873 0.2868184 0.3089113
      
      $missForest$statisticalMeasures
                                  X1          X2          X3         X4        mean
      TrueMean           -1.89181983 -0.14424317  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.53963946 -0.24731609 -0.10119736 0.71111260 -0.29426008
      AbsDiffTrueImpMean  0.48153809  0.66714211  0.61670424 0.48166663  0.56176277
      GenMean            -0.89286341 -0.63303381  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.85764538 -0.64334110  0.24768653 0.58670047 -0.16664987
      AbsDiffGenImpMean   0.04815381  0.06671421  0.06167042 0.04816666  0.05617628
      TrueSD              1.08174083  1.43409291  0.52378759 1.11236840  1.03799743
      ImpSD               0.39829267  0.91736048  0.38776096 0.73796524  0.61034484
      AbsDiffTrueImpSD    0.68344816  0.51673242  0.20844264 0.37440316  0.44575660
      GenSD               1.23204855  1.22736828  1.02518086 1.02063125  1.12630723
      GenImpSD            1.15680378  1.15661550  1.02457946 0.99089053  1.08222232
      AbsDiffGenImpSD     0.07524477  0.07075277  0.01293635 0.02974072  0.04716866
      
      $missForest$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6622539 0.6622539
      true+res-anti    0.6636708 0.6636708
      imputed+avs-anti 0.4783835 0.4783835
      imputed+ms-anti  0.5589371 0.5589371
      imputed+res-anti 0.6111803 0.6111803
      parts+avs-anti   0.4980159 0.4980159
      parts+ms-anti    0.7583838 0.7583838
      parts+res-anti   0.7404185 0.7404185
      
      $missForest$fuzzyMeasures
                         V1        mean
      Euclidean 0.001031675 0.001031675
      AHD       0.014513224 0.014513224
      HSD       0.008293271 0.008293271
      
      
      $miceRanger
      $miceRanger$nonFNNumbers
        V1 mean 
       1.5  1.5 
      
      $miceRanger$errorMatrix
                   X1        X2        X3        X4      mean
      MAE   0.5644528 0.9808929 0.7061296 0.5212157 0.6931727
      WMAE  0.2887787 0.9442214 1.9916242 0.4995107 0.9310338
      MSE   0.8740102 1.3589836 0.5738746 0.3877424 0.7986527
      WMSE  0.1535998 1.2447005 4.9395065 0.3494964 1.6718258
      NRMSE 0.3376719 0.4924162 0.3638808 0.3109525 0.3762304
      
      $miceRanger$statisticalMeasures
                                  X1          X2          X3         X4        mean
      TrueMean           -1.89181983 -0.14424317  0.51550688 0.63236446 -0.22204791
      ImpMean            -1.49330705 -0.22614784 -0.19062269 0.70426686 -0.30145268
      AbsDiffTrueImpMean  0.54273373  0.98089293  0.70612956 0.33107987  0.64020902
      GenMean            -0.89286341 -0.63303381  0.30935695 0.57882565 -0.15942865
      GenImpMean         -0.85301214 -0.64122427  0.23874399 0.58601589 -0.16736913
      AbsDiffGenImpMean   0.05427337  0.09808929  0.07061296 0.03310799  0.06402090
      TrueSD              1.08174083  1.43409291  0.52378759 1.11236840  1.03799743
      ImpSD               0.31419943  0.69401962  0.27966406 0.37525811  0.41578530
      AbsDiffTrueImpSD    0.76754139  0.74007329  0.24412354 0.73711029  0.62221213
      GenSD               1.23204855  1.22736828  1.02518086 1.02063125  1.12630723
      GenImpSD            1.15156705  1.14172701  1.02714754 0.97929296  1.07493364
      AbsDiffGenImpSD     0.08048150  0.08564127  0.01736889 0.04133829  0.05620749
      
      $miceRanger$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6981216 0.6981216
      true+res-anti    0.6561468 0.6561468
      imputed+avs-anti 0.4493403 0.4493403
      imputed+ms-anti  0.4925790 0.4925790
      imputed+res-anti 0.4797352 0.4797352
      parts+avs-anti   0.6011905 0.6011905
      parts+ms-anti    0.6785823 0.6785823
      parts+res-anti   0.6484343 0.6484343
      
      $miceRanger$fuzzyMeasures
                         V1        mean
      Euclidean 0.000656723 0.000656723
      AHD       0.011579332 0.011579332
      HSD       0.006616761 0.006616761
      
      
      $knn
      $knn$nonFNNumbers
        V1 mean 
       3.5  3.5 
      
      $knn$errorMatrix
                   X1        X2        X3        X4      mean
      MAE   0.9628259 0.9928994 0.4856660 0.6604259 0.7754543
      WMAE  0.6101618 1.1586596 1.5686256 0.7012439 1.0096727
      MSE   1.5578272 1.4495443 0.2641178 0.5973828 0.9672180
      WMSE  0.5743589 2.1203726 3.6504932 0.7224116 1.7669091
      NRMSE 0.4957328 0.4899888 0.2480280 0.3835187 0.4043171
      
      $knn$statisticalMeasures
                                  X1          X2          X3         X4        mean
      TrueMean           -1.89181983 -0.14424317 0.515506875 0.63236446 -0.22204791
      ImpMean            -1.30063210 -0.45335331 0.029840897 0.57540693 -0.28718439
      AbsDiffTrueImpMean  0.76490628  0.99289944 0.485665977 0.61928687  0.71568964
      GenMean            -0.89286341 -0.63303381 0.309356951 0.57882565 -0.15942865
      GenImpMean         -0.83374464 -0.66394482 0.260790353 0.57312990 -0.16594230
      AbsDiffGenImpMean   0.07649063  0.09928994 0.048566598 0.06192869  0.07156896
      TrueSD              1.08174083  1.43409291 0.523787595 1.11236840  1.03799743
      ImpSD               0.00000000  1.07293337 0.314222276 0.60305436  0.49755250
      AbsDiffTrueImpSD    1.08174083  0.77836727 0.209565318 0.50931403  0.64474686
      GenSD               1.23204855  1.22736828 1.025180858 1.02063125  1.12630723
      GenImpSD            1.13377670  1.15133097 1.020977144 0.97747236  1.07088930
      AbsDiffGenImpSD     0.09827186  0.09076027 0.008489656 0.04315889  0.06017017
      
      $knn$statisticalTests
                              V1      mean
      true+avs-anti    0.6184727 0.6184727
      true+ms-anti     0.6882526 0.6882526
      true+res-anti    0.6683359 0.6683359
      imputed+avs-anti 0.5237395 0.5237395
      imputed+ms-anti  0.6137479 0.6137479
      imputed+res-anti 0.5428655 0.5428655
      parts+avs-anti   0.6357143 0.6357143
      parts+ms-anti    0.6695952 0.6695952
      parts+res-anti   0.7176667 0.7176667
      
      $knn$fuzzyMeasures
                V1 mean
      Euclidean  0    0
      AHD        0    0
      HSD        0    0
      
      
