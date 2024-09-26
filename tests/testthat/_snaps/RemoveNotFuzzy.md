# Function returns correct values

    Code
      RemoveNotFuzzy(trueData = testMatrix1, imputedData = testMatrix1Bad,
        trapezoidal = TRUE)
    Output
           [,1] [,2] [,3] [,4]
      [1,]    1    2    3    4
      [2,]   -1    3    5    9
      [3,]    0    4    7    1
      [4,]   -1   -3    5    0
      [5,]    7   11   22   26

---

    Code
      RemoveNotFuzzy(trueData = testMatrix2, imputedData = testMatrix2Bad,
        trapezoidal = FALSE)
    Output
           [,1] [,2] [,3]
      [1,]    1    2    3
      [2,]   -1    3    5
      [3,]    0    4    7
      [4,]   -1   -3    5
      [5,]    7   11   22

---

    Code
      RemoveNotFuzzy(trueData = testDataFrame1, imputedData = testDataFrame1Bad,
        trapezoidal = TRUE)
    Output
           x1 x2 x3 x4
      [1,] -2  2  3  4
      [2,] -1  3  5  9
      [3,]  0  4  7 10
      [4,]  7 11 22 23

---

    Code
      RemoveNotFuzzy(trueData = testDataFrame2, imputedData = testDataFrame2Bad,
        trapezoidal = FALSE)
    Output
           x1 x2 x3
      [1,] -2  2  3
      [2,] -1  3  5
      [3,]  0  4  7
      [4,]  7 11 22

