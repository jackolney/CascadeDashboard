CheckInterpolation <- function(propRuns, intLength) {

    theOut <- RunNSOptimisation(propRuns = 0.1, intLength = intLength)

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

    optRuns <- WhichAchieved73(simData = theOut, simLength = simLength)

    frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)

    output <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    results <- c(
        Cost = mean(output[,"iCost"]),
        Test = mean(output[,"iTest"]),
        Link = mean(output[,"iLink"]),
        PreR = mean(output[,"iPreR"]),
        Init = mean(output[,"iInit"]),
        Adhr = mean(output[,"iAdhr"]),
        Retn = mean(output[,"iRetn"])
    )
    results
}
