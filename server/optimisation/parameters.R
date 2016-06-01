GetOptPar <- function(masterCD4, data, iterationParam, calibParamOut, sampleMinErrorRun) {
    p <- parameters(
        prop_preART_500    = masterCD4[1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = masterCD4[1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = masterCD4[1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = masterCD4[1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = masterCD4[1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = masterCD4[1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = masterCD4[1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear2015(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear2015(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear2015(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear2015(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear2015(data[["treatment_guidelines"]][["less200"]]),

        # These guys still need to be set by the model (but use the best fit run)
        Theta   = calibParamOut[sampleMinErrorRun, "theta"],
        p       = calibParamOut[sampleMinErrorRun, "p"],
        Epsilon = calibParamOut[sampleMinErrorRun, "epsilon"],

        # MODIFYING #
        Rho   = iterationParam[["Rho"]],
        Kappa = iterationParam[["Kappa"]],
        Gamma = iterationParam[["Gamma"]],
        Sigma = iterationParam[["Sigma"]],
        Omega = iterationParam[["Omega"]],
        q     = iterationParam[["Q"]]
    )
    p
}

GetBestPar <- function(masterCD4, data, calibParamOut, sampleMinErrorRun) {
    p <- parameters(
        prop_preART_500    = masterCD4[1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = masterCD4[1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = masterCD4[1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = masterCD4[1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = masterCD4[1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = masterCD4[1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = masterCD4[1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear2015(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear2015(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear2015(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear2015(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear2015(data[["treatment_guidelines"]][["less200"]]),

        # Using best fit run)
        Theta   = calibParamOut[sampleMinErrorRun, "theta"],
        p       = calibParamOut[sampleMinErrorRun, "p"],
        Rho     = calibParamOut[sampleMinErrorRun, "rho"],
        Kappa   = calibParamOut[sampleMinErrorRun, "kappa"],
        Gamma   = calibParamOut[sampleMinErrorRun, "gamma"],
        Omega   = calibParamOut[sampleMinErrorRun, "omega"],
        Epsilon = calibParamOut[sampleMinErrorRun, "epsilon"],
        q       = calibParamOut[sampleMinErrorRun, "q"]
    )
    p
}
