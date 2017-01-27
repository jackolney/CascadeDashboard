# Baseline Model Call
# This now needs to take input from the calibration data set and return a formatted data.frame ready for plotting.
CallBaselineModel <- function(runNumber, initVals) {
    # Setup #
    p <- GetBaselinePar(
        masterCD4 = MasterData$cd4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        runNumber = runNumber)

    y <- GetInitial(
        p = p,
        iterationResult = initVals,
        masterCD4 = MasterData$cd4_2015)

    p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[runNumber,])

    result <- RunSim_Abs(y = y, p = p)
    result
}
