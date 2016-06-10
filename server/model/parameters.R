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
            q = iterationParam[["q"]]
      )
      p
}
