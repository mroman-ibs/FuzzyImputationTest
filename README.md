
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FuzzyImputationTest

<!-- badges: start -->
<!-- badges: end -->

The goal of FuzzyImputationTest is to impute (i.e., replace missing
values given by NAs) dataset that consists of triangular or trapezoidal
fuzzy numbers, and check quality of such an imputation. To impute fuzzy
values, various imputation methods - both general (like miceRanger,
missingForest, knn), and specific ones (DIMP) - can be used. To check
the quality of the imputation process, the dataset without missing
values can be specified, and then it is tested with the whole set of
procedures. These procedures are related to calculation of various
sample statistics like the mean, standard deviation, and some special
distance measures for fuzzy numbers, together with obtaining different
error values, and conduction of statistical tests based on the epistemic
bootstrap (see (P. Grzegorzewski and Romaniuk 2021, 2024; Przemyslaw
Grzegorzewski and Romaniuk 2022)) from package (see (Romaniuk,
Grzegorzewski, and Parchami 2024)). There are also special procedures to
fuzzify the input and introduce some NAs when necessary.

The following procedures are available in the library:

- Imputation procedures:
  - *FuzzyImputation* - The main method to impute fuzzy values.
  - *ImputationDimp* - The DIMP (d-imputation) method for fuzzy numbers.
- Conversion procedures:
  - *FuzzyNumbersToMatrix* - Conversion of a list of fuzzy numbers into
    a matrix.
  - *MatrixToFuzzyNumbers* - Conversion of a matrix to a list of fuzzy
    numbers.
- Calculation of the distance measures:
  - *MeasureAHD* - Calculation of the AHD (Area Hight Distance) measure
    between two trapezoidal or triangular fuzzy numbers.
  - *MeasureHSD* - Calculation of the HSD (Hight Source Distance)
    measure between two trapezoidal or triangular fuzzy numbers.
- Additional procedures:
  - *IntroducingNA* - Introducing NAs to the specified matrix.
  - *RemoveNotFuzzy* - Removing values that are not fuzzy numbers.
  - *FuzzifyMatrix* - Fuzzify the real-valued variables and construct
    trapezoidal or triangular fuzzy numbers.

## Installation

You can install the development version of FuzzyImputationTest from
[GitHub](https://github.com/) with:

## Example

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-grzegorzewski2021" class="csl-entry">

Grzegorzewski, P., and M. Romaniuk. 2021. “Epistemic Bootstrap for Fuzzy
Data.” In *Joint Proceedings of IFSA-EUSFLAT-AGOP 2021 Conferences*,
538–45. Atlantis Press.

</div>

<div id="ref-PGMR2024AMS" class="csl-entry">

———. 2024. “Bootstrapped Tests for Epistemic Fuzzy Data.” *International
Journal of Applied Mathematics and Computer Science* 34 (2): 277–89.
<https://doi.org/10.61822/amcs-2024-0020>.

</div>

<div id="ref-Grzegorzewski_Romaniuk_2022" class="csl-entry">

Grzegorzewski, Przemyslaw, and Maciej Romaniuk. 2022. “Bootstrap Methods
for Epistemic Fuzzy Data.” *International Journal of Applied Mathematics
and Computer Science* 32 (2): 285–97.

</div>

<div id="ref-FuzzySimRes" class="csl-entry">

Romaniuk, M., P. Grzegorzewski, and A. Parchami. 2024. “[FuzzySimRes:
Epistemic Bootstrap – the Efficient Tool for Statistical Inference Based
on Imprecise Data]().” *R Journal*.

</div>

</div>
