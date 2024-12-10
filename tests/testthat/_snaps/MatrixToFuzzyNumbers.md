# Function returns correct values

    Code
      MatrixToFuzzyNumbers(fuzzyMatrix = testMatrixTRFNs)
    Output
      [[1]]
      Trapezoidal fuzzy number with:
         support=[1,5],
            core=[3,3].
      
      [[2]]
      Trapezoidal fuzzy number with:
         support=[2,7],
            core=[5,5].
      

---

    Code
      MatrixToFuzzyNumbers(fuzzyMatrix = testMatrixTPFNs)
    Output
      [[1]]
      Trapezoidal fuzzy number with:
         support=[1,6],
            core=[3,5].
      
      [[2]]
      Trapezoidal fuzzy number with:
         support=[2,10],
            core=[5,7].
      

---

    Code
      MatrixToFuzzyNumbers(fuzzyMatrix = testMatrixTPFNs, varNames = c("a", "b"))
    Output
      $a
      Trapezoidal fuzzy number with:
         support=[1,6],
            core=[3,5].
      
      $b
      Trapezoidal fuzzy number with:
         support=[2,10],
            core=[5,7].
      

