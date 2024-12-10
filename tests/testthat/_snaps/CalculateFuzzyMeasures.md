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

