# Function returns correct values

    Code
      FuzzyNumbersToMatrix(fuzzyList = testListTPFNs, trapezoidal = TRUE)
    Output
           [,1] [,2] [,3] [,4]
      [1,]    1    3    4    5
      [2,]    2    6    7   10
      [3,]   -2    1    5    9

---

    Code
      FuzzyNumbersToMatrix(fuzzyList = testListTRFNs, trapezoidal = FALSE)
    Output
           [,1] [,2] [,3]
      [1,]    1    3    5
      [2,]    2    6   10
      [3,]   -2    1    9

---

    Code
      FuzzyNumbersToMatrix(fuzzyList = testListTPFNs, trapezoidal = TRUE, varNames = c(
        "a", "b", "c", "d"))
    Output
            a b c  d
      [1,]  1 3 4  5
      [2,]  2 6 7 10
      [3,] -2 1 5  9

---

    Code
      FuzzyNumbersToMatrix(fuzzyList = testListTRFNs, trapezoidal = FALSE, varNames = c(
        "a", "b", "c"))
    Output
            a b  c
      [1,]  1 3  5
      [2,]  2 6 10
      [3,] -2 1  9

