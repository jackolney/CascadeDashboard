# Calibration Model Call
CallCalibModel <- function(time, y, p, i) {
    # print("CallCalibModel() called.")

    # Put it all in a list
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")

    # The Model #
    result <- deSolve::ode(times = time, y = y, func = "calib_derivs", parms = plist, initfunc = "calib_initmod", dllname = "cascade")
    # --------- #

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
            )]),

        cd4_350500 = rowSums(result[, c(
            "UnDx_350500", "Dx_350500", "Care_350500", "PreLtfu_350500", "Tx_Na_350500", "Tx_A_350500", "Ltfu_350500"
            )]),

        cd4_250350 = rowSums(result[, c(
            "UnDx_250350", "Dx_250350", "Care_250350", "PreLtfu_250350", "Tx_Na_250350", "Tx_A_250350", "Ltfu_250350"
            )]),

        cd4_200250 = rowSums(result[, c(
            "UnDx_200250", "Dx_200250", "Care_200250", "PreLtfu_200250", "Tx_Na_200250", "Tx_A_200250", "Ltfu_200250"
            )]),

        cd4_100200 = rowSums(result[, c(
            "UnDx_100200", "Dx_100200", "Care_100200", "PreLtfu_100200", "Tx_Na_100200", "Tx_A_100200", "Ltfu_100200"
            )]),

        cd4_50100 = rowSums(result[, c(
            "UnDx_50100", "Dx_50100", "Care_50100", "PreLtfu_50100", "Tx_Na_50100", "Tx_A_50100", "Ltfu_50100"
            )]),

        cd4_50 = rowSums(result[, c(
            "UnDx_50", "Dx_50", "Care_50", "PreLtfu_50", "Tx_Na_50", "Tx_A_50", "Ltfu_50"
            )]),

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
