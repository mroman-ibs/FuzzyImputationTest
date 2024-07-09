# Vectors containing constant names used by other functions in the package.

# `errorTypes` is a vector containing names of the types of the calculated errors.
# `measuresTypes` is a vector containing names of the types of the calculated statistical measures.
# `testsNames` is a vector containing names of the types of the applied epistemic tests.
# `setsNames` is a vector containing names of the subsets used in the epistemic tests.
# `distanceNames` is a vector containing names of the calculated distance measures between fuzzy numbers.
#
#
#
#




errorTypes <- c("MAE","WMAE","MSE","WMSE","NMRSE")

measuresTypes <- c("TrueMean","ImpMean", "AbsDiffTrueImpMean", "GenMean","GenImpMean", "AbsDiffGenImpMean","TrueSD","ImpSD",
                   "AbsDiffTrueImpSD","GenSD","GenImpSD","AbsDiffGenImpSD")

testsNames <- c("avs-anti","ms-anti","res-anti")

setsNames <- c("true","imputed", "parts")

distanceNames <- c("Euclidean", "AHD", "HSD")
