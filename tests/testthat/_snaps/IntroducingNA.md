# Function returns correct values

    Code
      set.seed(123456)
      testMatrix1 <- matrix(c(1, 2, 3, 4, -1, 3, 5, 9, 0, 4, 7, 1, -1, -3, 5, 0, 7,
        11, 22, 15), nrow = 5, ncol = 4, byrow = TRUE)
      IntroducingNA(dataMatrix = testMatrix1, percentage = 0.1)
    Output
           [,1] [,2] [,3] [,4]
      [1,]    1    2    3   NA
      [2,]   -1   NA   NA    9
      [3,]    0    4    7    1
      [4,]   NA   -3    5    0
      [5,]    7   11   22   15

---

    Code
      set.seed(123456)
      testMatrix1 <- matrix(c(1, 2, 3, 4, -1, 3, 5, 9, 0, 4, 7, 1, -1, -3, 5, 0, 7,
        11, 22, 15), nrow = 5, ncol = 4, byrow = TRUE)
      IntroducingNA(dataMatrix = testMatrix1, percentage = 0.5)
    Output
           [,1] [,2] [,3] [,4]
      [1,]    1   NA    3    4
      [2,]   NA   NA   NA   NA
      [3,]    0   NA    7   NA
      [4,]   NA   -3   NA   NA
      [5,]   NA   11   NA   15

---

    Code
      set.seed(123456)
      testDataFrame1 <- data.frame(a = c(1, -1, 0, -1, 7), b = c(2, 3, 4, -3, 11), b = c(
        3, 5, 7, 5, 22), d = c(4, 9, 1, 0, 15))
      IntroducingNA(dataMatrix = testMatrix1, percentage = 0.2)
    Output
           [,1] [,2] [,3] [,4]
      [1,]    1    2    3   NA
      [2,]   -1   NA   NA    9
      [3,]    0    4    7    1
      [4,]   NA   -3    5    0
      [5,]    7   11   22   15

