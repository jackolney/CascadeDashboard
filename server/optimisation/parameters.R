GetOptPar <- function(masterCD4, data, iterationParam, calibParamOut, minErrorRun) {
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
        Theta = round(calibParamOut[minErrorRun, "theta"], digits = 4),
        p     = round(calibParamOut[minErrorRun, "p"], digits = 4),

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

GetBestPar <- function(masterCD4, data, calibParamOut, minErrorRun) {
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
        Theta = round(calibParamOut[minErrorRun, "theta"], digits = 4),
        p     = round(calibParamOut[minErrorRun, "p"],     digits = 4),
        Rho   = round(calibParamOut[minErrorRun, "rho"],   digits = 4),
        Kappa = round(calibParamOut[minErrorRun, "kappa"], digits = 4),
        Gamma = round(calibParamOut[minErrorRun, "gamma"], digits = 4),
        Omega = round(calibParamOut[minErrorRun, "omega"], digits = 4),
        q     = round(calibParamOut[minErrorRun, "q"],     digits = 4)
    )
    p
}
