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
        Theta   = calibParamOut[minErrorRun, "theta"],
        p       = calibParamOut[minErrorRun, "p"],
        Epsilon = calibParamOut[minErrorRun, "epsilon"],

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

GetOptRunPar <- function(masterCD4, data, iterationParam, calibParamOut, runNumber) {
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
        Theta   = calibParamOut[runNumber, "theta"],
        p       = calibParamOut[runNumber, "p"],
        Epsilon = calibParamOut[runNumber, "epsilon"],

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

GetBaselinePar <- function(masterCD4, data, calibParamOut, runNumber) {
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
        Theta   = calibParamOut[runNumber, "theta"],
        p       = calibParamOut[runNumber, "p"],
        Rho     = calibParamOut[runNumber, "rho"],
        Kappa   = calibParamOut[runNumber, "kappa"],
        Gamma   = calibParamOut[runNumber, "gamma"],
        Omega   = calibParamOut[runNumber, "omega"],
        Epsilon = calibParamOut[runNumber, "epsilon"],
        q       = calibParamOut[runNumber, "q"]
    )
    p
}

GetMeanPar <- function(masterCD4, data, calibParamOut) {
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

        # Just using mean parameter values for the momen #
        Theta = round(lapply(calibParamOut, function(x) {return(mean(x))})[["theta"]], digits = 4),
        p     = round(lapply(calibParamOut, function(x) {return(mean(x))})[["p"]],     digits = 4),
        Rho   = round(lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]],   digits = 4),
        Kappa = round(lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4),
        Gamma = round(lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4),
        Omega = round(lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4),
        q     = round(lapply(calibParamOut, function(x) {return(mean(x))})[["q"]],     digits = 4)
    )
    p
}

GetAverageCalibOut <- function(calibOut) {
    # subset only model output for 2015
    out <- calibOut[calibOut$year == 2015 & calibOut$source == "model",]
    # find only unique values
    indicator <- unique(out$indicator)
    # pick out values
    value <- c()
    for (j in 1:length(indicator)) {
        value[j] <- mean(out[out$indicator == indicator[j], "value"])
    }
    # build data.frame and return
    df <- data.frame(indicator, value)
    df
}

CallMeanModel <- function() {
    message("CallMeanModel() called.")

    # Setup #
    time <- seq(0, 5, 1)

    meanCalibInitial <- GetAverageCalibOut(CalibOut)

    p <- GetMeanPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut)

    y <- GetInitial(
        p = p,
        iterationResult = meanCalibInitial,
        masterCD4 = MasterCD4_2015)

    # Pass the mean incidence to GetBeta()
    p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = colMeans(CalibIncOut))

    result <- deSolve::ode(times = time, y = y, func = "derivs", parms = p, initfunc = "initmod", dllname = "cascade")

    result <- cbind(result, N = rowSums(result[, c(
        "UnDx_500", "UnDx_350500", "UnDx_250350", "UnDx_200250", "UnDx_100200", "UnDx_50100", "UnDx_50",
        "Dx_500", "Dx_350500", "Dx_250350", "Dx_200250", "Dx_100200", "Dx_50100", "Dx_50",
        "Care_500", "Care_350500", "Care_250350", "Care_200250", "Care_100200", "Care_50100", "Care_50",
        "PreLtfu_500", "PreLtfu_350500", "PreLtfu_250350", "PreLtfu_200250", "PreLtfu_100200", "PreLtfu_50100", "PreLtfu_50",
        "Tx_Na_500", "Tx_Na_350500", "Tx_Na_250350", "Tx_Na_200250", "Tx_Na_100200", "Tx_Na_50100", "Tx_Na_50",
        "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50",
        "Ltfu_500", "Ltfu_350500", "Ltfu_250350", "Ltfu_200250", "Ltfu_100200", "Ltfu_50100", "Ltfu_50")]
    ))

    result <- cbind(result,
        ART = rowSums(result[, c(
            "Tx_Na_500", "Tx_Na_350500", "Tx_Na_250350", "Tx_Na_200250", "Tx_Na_100200", "Tx_Na_50100", "Tx_Na_50",
            "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50"
            )]),

        UnDx = rowSums(result[, c(
            "UnDx_500", "UnDx_350500", "UnDx_250350", "UnDx_200250", "UnDx_100200", "UnDx_50100", "UnDx_50"
            )]),

        Dx = rowSums(result[, c(
            "Dx_500", "Dx_350500", "Dx_250350", "Dx_200250", "Dx_100200", "Dx_50100", "Dx_50"
            )]),

        Care = rowSums(result[, c(
                "Care_500", "Care_350500", "Care_250350", "Care_200250", "Care_100200", "Care_50100", "Care_50"
                )]),

        PreLtfu = rowSums(result[, c(
                "PreLtfu_500", "PreLtfu_350500", "PreLtfu_250350", "PreLtfu_200250", "PreLtfu_100200", "PreLtfu_50100", "PreLtfu_50"
                )]),

        Tx = rowSums(result[, c(
                "Tx_Na_500", "Tx_Na_350500", "Tx_Na_250350", "Tx_Na_200250", "Tx_Na_100200", "Tx_Na_50100", "Tx_Na_50",
                "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50"
                )]),

        Vs = rowSums(result[, c(
                "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50"
                )]),

        Ltfu = rowSums(result[, c(
                "Ltfu_500", "Ltfu_350500", "Ltfu_250350", "Ltfu_200250", "Ltfu_100200", "Ltfu_50100", "Ltfu_50"
                )]),

        NaturalMortalityProp = result[, "NaturalMortality"] / result[, "N"],

        HivMortalityProp = result[, "HivMortality"] / result[, "N"],

        NewInfProp = result[, "NewInf"] / result[, "N"],

        TotalCost = rowSums(result[, c(
                "Dx_Cost", "Linkage_Cost", "Annual_Care_Cost", "Annual_ART_Cost"
                )]),

        cd4_500 = rowSums(result[, c(
                "UnDx_500", "Dx_500", "Care_500", "PreLtfu_500", "Tx_Na_500", "Tx_A_500", "Ltfu_500"
                )]) / result[, "N"],

        cd4_350500 = rowSums(result[, c(
                "UnDx_350500", "Dx_350500", "Care_350500", "PreLtfu_350500", "Tx_Na_350500", "Tx_A_350500", "Ltfu_350500"
                )]) / result[, "N"],

        cd4_250350 = rowSums(result[, c(
                "UnDx_250350", "Dx_250350", "Care_250350", "PreLtfu_250350", "Tx_Na_250350", "Tx_A_250350", "Ltfu_250350"
                )]) / result[, "N"],

        cd4_200250 = rowSums(result[, c(
                "UnDx_200250", "Dx_200250", "Care_200250", "PreLtfu_200250", "Tx_Na_200250", "Tx_A_200250", "Ltfu_200250"
                )]) / result[, "N"],

        cd4_100200 = rowSums(result[, c(
                "UnDx_100200", "Dx_100200", "Care_100200", "PreLtfu_100200", "Tx_Na_100200", "Tx_A_100200", "Ltfu_100200"
                )]) / result[, "N"],

        cd4_50100 = rowSums(result[, c(
                "UnDx_50100", "Dx_50100", "Care_50100", "PreLtfu_50100", "Tx_Na_50100", "Tx_A_50100", "Ltfu_50100"
                )]) / result[, "N"],

        cd4_50 = rowSums(result[, c(
                "UnDx_50", "Dx_50", "Care_50", "PreLtfu_50", "Tx_Na_50", "Tx_A_50", "Ltfu_50"
                )]) / result[, "N"],

        DALY = (
            (rowSums(result[, c("UnDx_500", "Dx_500", "Care_500", "PreLtfu_500", "Tx_Na_500", "Ltfu_500",
                "UnDx_350500", "Dx_350500", "Care_350500", "PreLtfu_350500", "Tx_Na_350500", "Ltfu_350500")]) * 0.078) + # >350, no ART

            (rowSums(result[,c("UnDx_250350", "Dx_250350", "Care_250350", "PreLtfu_250350", "Tx_Na_250350", "Ltfu_250350",
                "UnDx_200250", "Dx_200250", "Care_200250", "PreLtfu_200250", "Tx_Na_200250", "Ltfu_200250")]) * 0.274) + # 200-350, no ART

            (rowSums(result[, c("UnDx_100200", "Dx_100200", "Care_100200", "PreLtfu_100200", "Tx_Na_100200", "Ltfu_100200",
                "UnDx_50100", "Dx_50100", "Care_50100", "PreLtfu_50100", "Tx_Na_50100", "Ltfu_50100",
                "UnDx_50", "Dx_50", "Care_50", "PreLtfu_50", "Tx_Na_50", "Ltfu_50")]) * 0.582) + # <200, no ART

            (rowSums(result[, c("Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50")]) * 0.078) # on ART & VS
        )
    )
    return(as.data.frame(result))
}
