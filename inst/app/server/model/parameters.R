ConvertYear2015 <- function(year) {
    if (is.na(year)) return(20)
    if (!is.numeric(year)) stop("Non-numeric value passed to ConvertYear2015()")
    if ((year - 2015) <= 0) {
        return(0)
    } else {
        return(year - 2015)
    }
}

GetParameters <- function(masterCD4, data, iterationParam) {
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
            Rho = iterationParam[["rho"]],
            Epsilon = iterationParam[["epsilon"]],
            Kappa = iterationParam[["kappa"]],
            Gamma = iterationParam[["gamma"]],
            Theta = iterationParam[["theta"]],
            Omega = iterationParam[["omega"]],
            p = iterationParam[["p"]],
            q = iterationParam[["q"]],
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
            Mu = AdvCalib$NatMort
      )
    p
}
