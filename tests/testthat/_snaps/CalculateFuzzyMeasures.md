# Function returns correct values

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testMatrix1, imputedData = testMatrix1Res,
        imputedMask = testMatrix1Mask, trapezoidal = TRUE)
    Output
                         V1       mean
      Euclidean  0.01597974 0.01597974
      AHD        0.20373806 0.20373806
      HSD        0.14043065 0.14043065
      Bertoluzza 0.07935232 0.07935232
      DiffVal    0.06332650 0.06332650
      DiffAmb    0.07076335 0.07076335
      DiffEV     0.05773562 0.05773562
      DiffWidth  0.06330741 0.06330741

---

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testMatrix2, imputedData = testMatrix2Res,
        imputedMask = testMatrix2Mask, trapezoidal = FALSE)
    Output
                         V1       mean
      Euclidean  0.02153806 0.02153806
      AHD        0.21252430 0.21252430
      HSD        0.15907851 0.15907851
      Bertoluzza 0.10464542 0.10464542
      DiffVal    0.07428797 0.07428797
      DiffAmb    0.03563052 0.03563052
      DiffEV     0.07534339 0.07534339
      DiffWidth  0.05344578 0.05344578

---

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testDF1, imputedData = testDF1Res,
        imputedMask = testDF1Mask, trapezoidal = TRUE)
    Output
                         V1       mean
      Euclidean  0.01597974 0.01597974
      AHD        0.20373806 0.20373806
      HSD        0.14043065 0.14043065
      Bertoluzza 0.07935232 0.07935232
      DiffVal    0.06332650 0.06332650
      DiffAmb    0.07076335 0.07076335
      DiffEV     0.05773562 0.05773562
      DiffWidth  0.06330741 0.06330741

---

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testDF2, imputedData = testDF2Res,
        imputedMask = testDF2Mask, trapezoidal = FALSE)
    Output
                         V1       mean
      Euclidean  0.02153806 0.02153806
      AHD        0.21252430 0.21252430
      HSD        0.15907851 0.15907851
      Bertoluzza 0.10464542 0.10464542
      DiffVal    0.07428797 0.07428797
      DiffAmb    0.03563052 0.03563052
      DiffEV     0.07534339 0.07534339
      DiffWidth  0.05344578 0.05344578

