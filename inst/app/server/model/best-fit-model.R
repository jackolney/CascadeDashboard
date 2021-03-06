# Best 'fit' Model Call - well a proportion of them.
CallBestFitModel <- function(CalibOut, propRuns, ...) {
    # Setup #
    # The idea of this model is that it will simulate all parameter sets that will be used during optimisation to provide an accurate reading of the changes that will be made to care.

    bestTenPercentCalibInitial <<- GetRandomTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = propRuns)

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
            Rho = CalibParamOut[shuffledRuns[j],"rho"],
            Epsilon = CalibParamOut[shuffledRuns[j],"epsilon"],
            Kappa = CalibParamOut[shuffledRuns[j],"kappa"],
            Gamma = CalibParamOut[shuffledRuns[j],"gamma"],
            Theta = CalibParamOut[shuffledRuns[j],"theta"],
            Omega = CalibParamOut[shuffledRuns[j],"omega"],
            p = CalibParamOut[shuffledRuns[j],"p"],
            q = CalibParamOut[shuffledRuns[j],"q"],
            # Advanced Calibration Tools (mortality)
            Alpha_1 = 0.004110 * AdvCalib$HIVMort,
            Alpha_2 = 0.011670 * AdvCalib$HIVMort,
            Alpha_3 = 0.009385 * AdvCalib$HIVMort,
            Alpha_4 = 0.016394 * AdvCalib$HIVMort,
            Alpha_5 = 0.027656 * AdvCalib$HIVMort,
            Alpha_6 = 0.047877 * AdvCalib$HIVMort,
            Alpha_7 = 1.081964 * AdvCalib$HIVMort,
            Tau_1 = 0.003905 * AdvCalib$HIVMort,
            Tau_2 = 0.011087 * AdvCalib$HIVMort,
            Tau_3 = 0.008916 * AdvCalib$HIVMort,
            Tau_4 = 0.015574 * AdvCalib$HIVMort,
            Tau_5 = 0.026273 * AdvCalib$HIVMort,
            Tau_6 = 0.045482 * AdvCalib$HIVMort,
            Tau_7 = 1.02785 * AdvCalib$HIVMort,
            Mu = AdvCalib$NatMort,
            ...
        )

        # Now we need the initials.
        y <- GetInitial(
            p = p,
            iterationResult = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),],
            masterCD4 = MasterData$cd4_2015)

        p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[shuffledRuns[j],])

        jList[[j]] <- RunSim_Prop(y = y, p = p)
    }
    out <- apply(abind::abind(jList, along = 3), c(1,2), mean)
    as.data.frame(out)
}
