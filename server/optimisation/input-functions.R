GetParaMatrixRun <- function(cParamOut, runNumber, length) {
    ParRange <- expand.grid(

        Rho   = seq(
            from = if (intSwitch$testing) {
                    cParamOut[runNumber, "rho"]
                } else {
                    cParamOut[runNumber, "rho"]
                },
            to = if (intSwitch$testing) {
                    OptInput$intValue_rho
                } else {
                    cParamOut[runNumber, "rho"]
                },
            length.out = length
        ),

        Q     = seq(
            from = if (intSwitch$linkage) {
                    cParamOut[runNumber, "q"]
                } else {
                    cParamOut[runNumber, "q"]
                },
            to = if (intSwitch$linkage) {
                    OptInput$intValue_q
                } else {
                    cParamOut[runNumber, "q"]
                },
            length.out = length
        ),

        Kappa = seq(
            from = if (intSwitch$preRetention) {
                    OptInput$intValue_kappa
                } else {
                    cParamOut[runNumber, "kappa"]
                },
            to = if (intSwitch$preRetention) {
                    cParamOut[runNumber, "kappa"]
                } else {
                    cParamOut[runNumber, "kappa"]
                },
            length.out = length
        ),

        Gamma = seq(
            from = if (intSwitch$initiation) {
                    cParamOut[runNumber, "gamma"]
                } else {
                    cParamOut[runNumber, "gamma"]
                },
            to = if (intSwitch$initiation) {
                    OptInput$intValue_gamma
                } else {
                    cParamOut[runNumber, "gamma"]
                },
            length.out = length
        ),

        Sigma = seq(
            from = if (intSwitch$adherence) {
                    0
                } else {
                    0
                },
            to = if (intSwitch$adherence) {
                OptInput$intValue_sigma
                } else {
                    0
                },
            length.out = length
        ),

        Omega = seq(
            from = if (intSwitch$retention) {
                    OptInput$intValue_omega
                } else {
                    cParamOut[runNumber, "omega"]
                },
            to = if (intSwitch$retention) {
                    cParamOut[runNumber, "omega"]
                } else {
                    cParamOut[runNumber, "omega"]
                },
            length.out = length
        )
    )
    out <- unique(ParRange)
    out
}

GetParaMatrixRunLimits <- function(cParamOut, runNumber, length) {
    ParRange <- data.frame(

        Rho   = seq(
            from = if (intSwitch$testing) {
                    cParamOut[runNumber, "rho"]
                } else {
                    cParamOut[runNumber, "rho"]
                },
            to = if (intSwitch$testing) {
                    OptInput$intValue_rho
                } else {
                    cParamOut[runNumber, "rho"]
                },
            length.out = length
        ),

        Q     = seq(
            from = if (intSwitch$linkage) {
                    cParamOut[runNumber, "q"]
                } else {
                    cParamOut[runNumber, "q"]
                },
            to = if (intSwitch$linkage) {
                    OptInput$intValue_q
                } else {
                    cParamOut[runNumber, "q"]
                },
            length.out = length
        ),

        Kappa = seq(
            from = if (intSwitch$preRetention) {
                    OptInput$intValue_kappa
                } else {
                    cParamOut[runNumber, "kappa"]
                },
            to = if (intSwitch$preRetention) {
                    cParamOut[runNumber, "kappa"]
                } else {
                    cParamOut[runNumber, "kappa"]
                },
            length.out = length
        ),

        Gamma = seq(
            from = if (intSwitch$initiation) {
                    cParamOut[runNumber, "gamma"]
                } else {
                    cParamOut[runNumber, "gamma"]
                },
            to = if (intSwitch$initiation) {
                    OptInput$intValue_gamma
                } else {
                    cParamOut[runNumber, "gamma"]
                },
            length.out = length
        ),

        Sigma = seq(
            from = if (intSwitch$adherence) {
                    0
                } else {
                    0
                },
            to = if (intSwitch$adherence) {
                OptInput$intValue_sigma
                } else {
                    0
                },
            length.out = length
        ),

        Omega = seq(
            from = if (intSwitch$retention) {
                    OptInput$intValue_omega
                } else {
                    cParamOut[runNumber, "omega"]
                },
            to = if (intSwitch$retention) {
                    cParamOut[runNumber, "omega"]
                } else {
                    cParamOut[runNumber, "omega"]
                },
            length.out = length
        )
    )
    out <- unique(ParRange)
    out
}

GetBestCalibOut <- function(calibOut, minErrorRun) {
    # Firstly, filter out all the data and errors (this changes between countries), only the 'model' output stays constant
    int <- calibOut[calibOut$source == "model",]
    # Now we know that 6 years and 7 indicators == 42, so divide data into sections of 42 in length
    int2 <- int[1:42 + 42 * (minErrorRun - 1),]
    # Select only 2015 data
    out <- int2[int2$year == 2015,]
    # Safety check for length > 7
    if (dim(out)[1] > 7) {
        warning("out length is > 7")
        print(out)
    }
    out
}

GetBestTenPercentCalibOut <- function(CalibOut, runError, selectedRuns, propRuns) {
    # subset the 'model' results (42 for each simulation, 6*7)
    modelledRuns <- CalibOut[CalibOut$source == "model",]

    # sort runs by error (lowest to highest)
    orderedRuns <- order(runError[selectedRuns])

    # identify the best _% (10% by default)
    bestRuns <- orderedRuns[1:(length(orderedRuns) * propRuns)]

    # extract values for each indicator and bind together
    bestRunValues <- modelledRuns[1:42 + 42 * (bestRuns[1] - 1),]
    for(i in 2:length(bestRuns)) {
        bestRunValues <- rbind(bestRunValues, modelledRuns[1:42 + 42 * (bestRuns[i] - 1),])
    }

    out <- bestRunValues[bestRunValues$year == 2015,]

    if (dim(out)[1] != (length(bestRuns) * 7)) {
        warning("Danger, out length is equal to all simulated indicators")
        print(out)
    }
    out
}

# This function uses the parRange base values for interventions
GetParaMatrixExperimental <- function(cParamOut, runNumber, length, parRange) {
    ParRange <- expand.grid(

        Rho   = seq(
            from = if (intSwitch$testing) {
                    parRange["rho", "min"]
                } else {
                    cParamOut[runNumber, "rho"]
                },
            to = if (intSwitch$testing) {
                    OptInput$intValue_rho
                } else {
                    cParamOut[runNumber, "rho"]
                },
            length.out = length
        ),

        Q     = seq(
            from = if (intSwitch$linkage) {
                    parRange["q", "min"]
                } else {
                    cParamOut[runNumber, "q"]
                },
            to = if (intSwitch$linkage) {
                    OptInput$intValue_q
                } else {
                    cParamOut[runNumber, "q"]
                },
            length.out = length
        ),

        Kappa = seq(
            from = if (intSwitch$preRetention) {
                    OptInput$intValue_kappa
                } else {
                    cParamOut[runNumber, "kappa"]
                },
            to = if (intSwitch$preRetention) {
                    parRange["kappa", "max"]
                } else {
                    cParamOut[runNumber, "kappa"]
                },
            length.out = length
        ),

        Gamma = seq(
            from = if (intSwitch$initiation) {
                    parRange["gamma", "min"]
                } else {
                    cParamOut[runNumber, "gamma"]
                },
            to = if (intSwitch$initiation) {
                    OptInput$intValue_gamma
                } else {
                    cParamOut[runNumber, "gamma"]
                },
            length.out = length
        ),

        Sigma = seq(
            from = if (intSwitch$adherence) {
                    0
                } else {
                    0
                },
            to = if (intSwitch$adherence) {
                OptInput$intValue_sigma
                } else {
                    0
                },
            length.out = length
        ),

        Omega = seq(
            from = if (intSwitch$retention) {
                    OptInput$intValue_omega
                } else {
                    cParamOut[runNumber, "omega"]
                },
            to = if (intSwitch$retention) {
                    parRange["omega", "max"]
                } else {
                    cParamOut[runNumber, "omega"]
                },
            length.out = length
        )
    )
    out <- unique(ParRange)
    out
}
