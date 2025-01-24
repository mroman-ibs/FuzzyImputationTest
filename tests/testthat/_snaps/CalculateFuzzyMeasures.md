# Function returns correct values

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testMatrix1, imputedData = testMatrix1Res,
        imputedMask = testMatrix1Mask, trapezoidal = TRUE)
    Output
                         V1        mean
      Euclidean 0.005449334 0.005449334
      AHD       0.073902810 0.073902810
      HSD       0.043646307 0.043646307
      DiffVal   0.022888064 0.022888064
      DiffAmb   0.030332704 0.030332704
      DiffEV    0.024673023 0.024673023
      DiffWidth 0.030256503 0.030256503

---

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testMatrix2, imputedData = testMatrix2Res,
        imputedMask = testMatrix2Mask, trapezoidal = FALSE)
    Output
                         V1        mean
      Euclidean 0.007183843 0.007183843
      AHD       0.068766234 0.068766234
      HSD       0.052609624 0.052609624
      DiffVal   0.033001344 0.033001344
      DiffAmb   0.010771073 0.010771073
      DiffEV    0.025726234 0.025726234
      DiffWidth 0.016156609 0.016156609

---

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testDF1, imputedData = testDF1Res,
        imputedMask = testDF1Mask, trapezoidal = TRUE)
    Output
                         V1        mean
      Euclidean 0.005449334 0.005449334
      AHD       0.073902810 0.073902810
      HSD       0.043646307 0.043646307
      DiffVal   0.022888064 0.022888064
      DiffAmb   0.030332704 0.030332704
      DiffEV    0.024673023 0.024673023
      DiffWidth 0.030256503 0.030256503

---

    Code
      set.seed(123456)
      CalculateFuzzyMeasures(trueData = testDF2, imputedData = testDF2Res,
        imputedMask = testDF2Mask, trapezoidal = FALSE)
    Output
                         V1        mean
      Euclidean 0.007183843 0.007183843
      AHD       0.068766234 0.068766234
      HSD       0.052609624 0.052609624
      DiffVal   0.033001344 0.033001344
      DiffAmb   0.010771073 0.010771073
      DiffEV    0.025726234 0.025726234
      DiffWidth 0.016156609 0.016156609

