GetInitial <- function(p, iterationResult, masterCD4) {
    if (!is.list(iterationResult)) stop("Not a list.")

    i2015_PLHIV         <- iterationResult[iterationResult$indicator == "PLHIV",              "value"]
    i2015_PLHIV_Diag    <- iterationResult[iterationResult$indicator == "PLHIV Diagnosed",    "value"]
    i2015_PLHIV_Care    <- iterationResult[iterationResult$indicator == "PLHIV in Care",      "value"]
    i2015_PLHIV_ART     <- iterationResult[iterationResult$indicator == "PLHIV on ART",       "value"]
    i2015_PLHIV_Viral   <- iterationResult[iterationResult$indicator == "PLHIV Suppressed",   "value"]
    i2015_PLHIV_preLtfu <- iterationResult[iterationResult$indicator == "PLHIV Pre-ART LTFU", "value"]
    i2015_PLHIV_Ltfu    <- iterationResult[iterationResult$indicator == "PLHIV ART LTFU",     "value"]

    iCD4_500    <- masterCD4[1,"prop.Off.ART.500"][[1]]
    iCD4_350500 <- masterCD4[1,"prop.Off.ART.350500"][[1]]
    iCD4_250350 <- masterCD4[1,"prop.Off.ART.250350"][[1]]
    iCD4_200250 <- masterCD4[1,"prop.Off.ART.200250"][[1]]
    iCD4_100200 <- masterCD4[1,"prop.Off.ART.100200"][[1]]
    iCD4_50100  <- masterCD4[1,"prop.Off.ART.50100"][[1]]
    iCD4_50     <- masterCD4[1,"prop.Off.ART.50"][[1]]

    iCD4_ART_500    <- masterCD4[1,"prop.On.ART.500"][[1]]
    iCD4_ART_350500 <- masterCD4[1,"prop.On.ART.350500"][[1]]
    iCD4_ART_250350 <- masterCD4[1,"prop.On.ART.250350"][[1]]
    iCD4_ART_200250 <- masterCD4[1,"prop.On.ART.200250"][[1]]
    iCD4_ART_100200 <- masterCD4[1,"prop.On.ART.100200"][[1]]
    iCD4_ART_50100  <- masterCD4[1,"prop.On.ART.50100"][[1]]
    iCD4_ART_50     <- masterCD4[1,"prop.On.ART.50"][[1]]

    # Negative checks (to ensure no negative compartment values)
    if (i2015_PLHIV - i2015_PLHIV_Diag < 0)
        warning("\tNegative value in model compartment (UnDx)")

    if (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu) < 0)
        warning("\tNegative value in model compartment (Dx)")

    if (i2015_PLHIV_Care - i2015_PLHIV_ART < 0)
        warning("\tNegative value in model compartment (Care)")

    if (i2015_PLHIV_preLtfu < 0)
        warning("\tNegative value in model compartment (PreLtfu)")

    if (i2015_PLHIV_ART - i2015_PLHIV_Viral < 0)
        warning("\tNegative value in model compartment (Tx_Na)")

    if (i2015_PLHIV_Viral < 0)
        warning("\tNegative value in model compartment (Tx_A)")

    if (i2015_PLHIV_Ltfu < 0)
        warning("\tNegative value in model compartment (Ltfu)")

    default <- initial(
        p,
        UnDx_500 =       (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_500,
        UnDx_350500 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_350500,
        UnDx_250350 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_250350,
        UnDx_200250 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_200250,
        UnDx_100200 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_100200,
        UnDx_50100 =     (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_50100,
        UnDx_50 =        (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_50,

        Dx_500 =         (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_500,
        Dx_350500 =      (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_350500,
        Dx_250350 =      (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_250350,
        Dx_200250 =      (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_200250,
        Dx_100200 =      (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_100200,
        Dx_50100 =       (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_50100,
        Dx_50 =          (i2015_PLHIV_Diag - (i2015_PLHIV_Care + i2015_PLHIV_preLtfu + i2015_PLHIV_Ltfu)) * iCD4_50,

        Care_500 =       (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_500,
        Care_350500 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_350500,
        Care_250350 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_250350,
        Care_200250 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_200250,
        Care_100200 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_100200,
        Care_50100 =     (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_50100,
        Care_50 =        (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_50,

        PreLtfu_500 =    i2015_PLHIV_preLtfu * iCD4_500,
        PreLtfu_350500 = i2015_PLHIV_preLtfu * iCD4_350500,
        PreLtfu_250350 = i2015_PLHIV_preLtfu * iCD4_250350,
        PreLtfu_200250 = i2015_PLHIV_preLtfu * iCD4_200250,
        PreLtfu_100200 = i2015_PLHIV_preLtfu * iCD4_100200,
        PreLtfu_50100 =  i2015_PLHIV_preLtfu * iCD4_50100,
        PreLtfu_50 =     i2015_PLHIV_preLtfu * iCD4_50,

        Tx_Na_500 =      (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_500,
        Tx_Na_350500 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_350500,
        Tx_Na_250350 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_250350,
        Tx_Na_200250 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_200250,
        Tx_Na_100200 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_100200,
        Tx_Na_50100 =    (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_50100,
        Tx_Na_50 =       (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_50,

        Tx_A_500 =       i2015_PLHIV_Viral * iCD4_ART_500,
        Tx_A_350500 =    i2015_PLHIV_Viral * iCD4_ART_350500,
        Tx_A_250350 =    i2015_PLHIV_Viral * iCD4_ART_250350,
        Tx_A_200250 =    i2015_PLHIV_Viral * iCD4_ART_200250,
        Tx_A_100200 =    i2015_PLHIV_Viral * iCD4_ART_100200,
        Tx_A_50100 =     i2015_PLHIV_Viral * iCD4_ART_50100,
        Tx_A_50 =        i2015_PLHIV_Viral * iCD4_ART_50,

        Ltfu_500 =       i2015_PLHIV_Ltfu * iCD4_500,
        Ltfu_350500 =    i2015_PLHIV_Ltfu * iCD4_350500,
        Ltfu_250350 =    i2015_PLHIV_Ltfu * iCD4_250350,
        Ltfu_200250 =    i2015_PLHIV_Ltfu * iCD4_200250,
        Ltfu_100200 =    i2015_PLHIV_Ltfu * iCD4_100200,
        Ltfu_50100 =     i2015_PLHIV_Ltfu * iCD4_50100,
        Ltfu_50 =        i2015_PLHIV_Ltfu * iCD4_50
    )
    default
}
