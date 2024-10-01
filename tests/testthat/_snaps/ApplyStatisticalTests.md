# Function returns correct values

    Code
      set.seed(123456)
      ApplyStatisticalTests(trueData = testMatrix1, imputedData = testMatrix1Res,
        imputedMask = testMatrix1Mask, cutsNumber = 100, K = 10, trapezoidal = TRUE)
    Output
                              V1      mean
      true+avs-anti    0.4319143 0.4319143
      true+ms-anti     0.4494832 0.4494832
      true+res-anti    0.4020562 0.4020562
      imputed+avs-anti 0.3443240 0.3443240
      imputed+ms-anti  0.4212245 0.4212245
      imputed+res-anti 0.3710733 0.3710733
      parts+avs-anti   0.8978057 0.8978057
      parts+ms-anti    0.8711881 0.8711881
      parts+res-anti   0.9074074 0.9074074

---

    Code
      set.seed(123456)
      ApplyStatisticalTests(trueData = testMatrix2, imputedData = testMatrix2Res,
        imputedMask = testMatrix2Mask, cutsNumber = 100, K = 10, trapezoidal = FALSE)
    Output
                              V1      mean
      true+avs-anti    0.1678213 0.1678213
      true+ms-anti     0.3094203 0.3094203
      true+res-anti    0.3046429 0.3046429
      imputed+avs-anti 0.1678213 0.1678213
      imputed+ms-anti  0.2548216 0.2548216
      imputed+res-anti 0.2427320 0.2427320
      parts+avs-anti   0.9944576 0.9944576
      parts+ms-anti    0.9913046 0.9913046
      parts+res-anti   0.9944576 0.9944576

