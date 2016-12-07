# Main Model Call
# This now needs to take input from the calibration data set and return a formatted data.frame ready for plotting.
CallModel <- reactive({
    # Setup #
    # This does ignore the cascade::parameter and cascade::initial
    MasterOut <- vector("list", dim(CalibParamOut)[1])

    # FOR EACH PARAMETER SET
    for (i in 1:dim(CalibParamOut)[1]) {

        p <- GetParameters(
            masterCD4 = MasterData$cd4_2015,
            data = MasterData,
            iterationParam = CalibParamOut[i,])

        # Now we need the initials.
        y <- GetInitial(
            p = p,
            iterationResult = CalibOut[CalibOut$year == 2015 & CalibOut$source == "model",][1:7 + 7 * (i - 1),],
            masterCD4 = MasterData$cd4_2015
            )

        p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[i,])

        MasterOut[[i]] <- RunSim_Abs(y = y, p = p)
    }
    return(MasterOut)
})
