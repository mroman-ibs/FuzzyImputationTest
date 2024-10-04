# Function returns correct values

    Code
      ErrorMatrix(trueData = testMatrix1, imputedData = testMatrix1Res, imputedMask = testMatrix1Mask)
    Output
                    X1           X2        X3         X4        mean
      MAE   0.15816363   0.18044286 0.3764461 0.10809568  0.20578707
      WMAE  0.25232132   9.12089408 0.7923513 0.49790500  2.66586793
      MSE   0.04968434   0.04419471 0.1953087 0.01864308  0.07695769
      WMSE  0.18867129 319.26814069 0.8957060 0.40012277 80.18816020
      NRMSE 0.09791167   0.09052128 0.2146912 0.06818963  0.11782844

---

    Code
      ErrorMatrix(trueData = testMatrix2, imputedData = testMatrix2Res, imputedMask = testMatrix2Mask)
    Output
                    X1           X2        X3        mean
      MAE   0.15816363   0.18044286 0.3764461   0.2383509
      WMAE  0.25232132   9.12089408 0.7923513   3.3885222
      MSE   0.04968434   0.04419471 0.1953087   0.0963959
      WMSE  0.18867129 319.26814069 0.8957060 106.7841727
      NRMSE 0.09791167   0.09052128 0.2146912   0.1343747

