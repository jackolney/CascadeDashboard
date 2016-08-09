# Best 'fit' Model Call - well a proportion of them.
CallBestFitModel <- function(CalibOut, propRuns, ...) {
    # Setup #
    # The idea of this model is that it will simulate all parameter sets that will be used during optimisation to provide an accurate reading of the changes that will be made to care.

    bestTenPercentCalibInitial <<- GetBestTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = propRuns)

    orderedRuns <- order(runError[selectedRuns])

    # We are going to use RimSim (as per optimise.R)

    # The difference here is that we need to average across all simulations

    jList <- list()

    # because seven indicators
    for (j in 1:(dim(bestTenPercentCalibInitial)[1] / 7)) {

        p <- parameters(
            prop_preART_500    = MasterData$cd4_2015[1,"prop.Off.ART.500"][[1]],
            prop_preART_350500 = MasterData$cd4_2015[1,"prop.Off.ART.350500"][[1]],
            prop_preART_250350 = MasterData$cd4_2015[1,"prop.Off.ART.250350"][[1]],
            prop_preART_200250 = MasterData$cd4_2015[1,"prop.Off.ART.200250"][[1]],
            prop_preART_100200 = MasterData$cd4_2015[1,"prop.Off.ART.100200"][[1]],
            prop_preART_50100  = MasterData$cd4_2015[1,"prop.Off.ART.50100"][[1]],
            prop_preART_50     = MasterData$cd4_2015[1,"prop.Off.ART.50"][[1]],
            t_1 = ConvertYear2015(MasterData[["treatment_guidelines"]][["more500"]]),
            t_2 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less500"]]),
            t_3 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less350"]]),
            t_4 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less250"]]),
            t_5 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less200"]]),
            Rho = CalibParamOut[orderedRuns[j],"rho"],
            Epsilon = CalibParamOut[orderedRuns[j],"epsilon"],
            Kappa = CalibParamOut[orderedRuns[j],"kappa"],
            Gamma = CalibParamOut[orderedRuns[j],"gamma"],
            Theta = CalibParamOut[orderedRuns[j],"theta"],
            Omega = CalibParamOut[orderedRuns[j],"omega"],
            p = CalibParamOut[orderedRuns[j],"p"],
            q = CalibParamOut[orderedRuns[j],"q"],
            ...
        )

        # Now we need the initials.
        y <- GetInitial(
            p = p,
            iterationResult = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),],
            masterCD4 = MasterData$cd4_2015)

        p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[orderedRuns[j],])

        jList[[j]] <- RunSim_Prop(y = y, p = p)
    }
    out <- apply(abind::abind(jList, along = 3), c(1,2), mean)
    as.data.frame(out)
}
