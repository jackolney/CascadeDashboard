ConvertYear <- function(year) {
    if (is.na(year)) return(20)
    if (!is.numeric(year)) stop("Non-numeric value passed to ConvertYear()")
    if ((year - 2010) <= 0) {
        return(0)
    } else {
        return(year - 2010)
    }
}

GetCalibInitial <- function(p, data, init2010) {
    if (!is.list(data)) stop("Not a list.")

    i2010_PLHIV      <- init2010[["plhiv"]]
    i2010_PLHIV_Diag <- init2010[["plhiv_diag"]]
    i2010_PLHIV_Care <- init2010[["plhiv_care"]]
    i2010_PLHIV_ART  <- init2010[["plhiv_art"]]

    iCD4_500    <- data[["cd4"]][1,"prop.Off.ART.500"][[1]]
    iCD4_350500 <- data[["cd4"]][1,"prop.Off.ART.350500"][[1]]
    iCD4_250350 <- data[["cd4"]][1,"prop.Off.ART.250350"][[1]]
    iCD4_200250 <- data[["cd4"]][1,"prop.Off.ART.200250"][[1]]
    iCD4_100200 <- data[["cd4"]][1,"prop.Off.ART.100200"][[1]]
    iCD4_50100  <- data[["cd4"]][1,"prop.Off.ART.50100"][[1]]
    iCD4_50     <- data[["cd4"]][1,"prop.Off.ART.50"][[1]]

    iCD4_ART_500    <- data[["cd4"]][1,"prop.On.ART.500"][[1]]
    iCD4_ART_350500 <- data[["cd4"]][1,"prop.On.ART.350500"][[1]]
    iCD4_ART_250350 <- data[["cd4"]][1,"prop.On.ART.250350"][[1]]
    iCD4_ART_200250 <- data[["cd4"]][1,"prop.On.ART.200250"][[1]]
    iCD4_ART_100200 <- data[["cd4"]][1,"prop.On.ART.100200"][[1]]
    iCD4_ART_50100  <- data[["cd4"]][1,"prop.On.ART.50100"][[1]]
    iCD4_ART_50     <- data[["cd4"]][1,"prop.On.ART.50"][[1]]

    # System Checks here.
    # Throw alert if NEGATIVE value encountered.
    if (i2010_PLHIV - i2010_PLHIV_Diag < 0)
        warning("\tNegative value in model compartment (UnDx)")

    if (i2010_PLHIV_Diag - i2010_PLHIV_Care < 0)
        warning("\tNegative value in model compartment (Dx)")

    if (i2010_PLHIV_Care - i2010_PLHIV_ART < 0)
        warning("\tNegative value in model compartment (Care)")

    if (i2010_PLHIV_ART < 0)
        warning("\tNegative value in model compartment (Tx)")

    default <- initial(
        p,
        UnDx_500 =       (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_500,
        UnDx_350500 =    (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_350500,
        UnDx_250350 =    (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_250350,
        UnDx_200250 =    (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_200250,
        UnDx_100200 =    (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_100200,
        UnDx_50100 =     (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_50100,
        UnDx_50 =        (i2010_PLHIV - i2010_PLHIV_Diag) * iCD4_50,

        Dx_500 =         (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_500,
        Dx_350500 =      (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_350500,
        Dx_250350 =      (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_250350,
        Dx_200250 =      (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_200250,
        Dx_100200 =      (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_100200,
        Dx_50100 =       (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_50100,
        Dx_50 =          (i2010_PLHIV_Diag - i2010_PLHIV_Care) * iCD4_50,

        Care_500 =       (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_500,
        Care_350500 =    (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_350500,
        Care_250350 =    (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_250350,
        Care_200250 =    (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_200250,
        Care_100200 =    (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_100200,
        Care_50100 =     (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_50100,
        Care_50 =        (i2010_PLHIV_Care - i2010_PLHIV_ART) * iCD4_50,

        PreLtfu_500 =    0,
        PreLtfu_350500 = 0,
        PreLtfu_250350 = 0,
        PreLtfu_200250 = 0,
        PreLtfu_100200 = 0,
        PreLtfu_50100 =  0,
        PreLtfu_50 =     0,

        Tx_Na_500 =      i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_500,
        Tx_Na_350500 =   i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_350500,
        Tx_Na_250350 =   i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_250350,
        Tx_Na_200250 =   i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_200250,
        Tx_Na_100200 =   i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_100200,
        Tx_Na_50100 =    i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_50100,
        Tx_Na_50 =       i2010_PLHIV_ART * (1-p[["p"]]) * iCD4_ART_50,

        Tx_A_500 =       i2010_PLHIV_ART * p[["p"]] * iCD4_ART_500,
        Tx_A_350500 =    i2010_PLHIV_ART * p[["p"]] * iCD4_ART_350500,
        Tx_A_250350 =    i2010_PLHIV_ART * p[["p"]] * iCD4_ART_250350,
        Tx_A_200250 =    i2010_PLHIV_ART * p[["p"]] * iCD4_ART_200250,
        Tx_A_100200 =    i2010_PLHIV_ART * p[["p"]] * iCD4_ART_100200,
        Tx_A_50100 =     i2010_PLHIV_ART * p[["p"]] * iCD4_ART_50100,
        Tx_A_50 =        i2010_PLHIV_ART * p[["p"]] * iCD4_ART_50,

        Ltfu_500 =       0,
        Ltfu_350500 =    0,
        Ltfu_250350 =    0,
        Ltfu_200250 =    0,
        Ltfu_100200 =    0,
        Ltfu_50100 =     0,
        Ltfu_50 =        0
    )
    default
}
