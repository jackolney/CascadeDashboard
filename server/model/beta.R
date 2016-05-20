## Beta calculation
# Feed in incidence array from CalibIncOut
GetBeta <- function(y, p, iterationInc) {

    numerator <- as.double(iterationInc)[6]

    denominator <- as.double((

        # Entering with CD4 >500
        (y[1] + y[8] + y[15] + y[22] + y[29] + y[43]) * p[57]) +

        # Entering with CD4 350-500
        ((y[2] + y[9] + y[16] + y[23] + y[30] + y[44]) * p[58]) +

        # Entering with CD4 200-350
        ((y[3] + y[10] + y[17] + y[24] + y[31] + y[45] +
            y[4] + y[11] + y[18] + y[25] + y[32] + y[46]) * p[59]) +

        # Entering with CD4 <200
        ((y[5] + y[12] + y[19] + y[26] + y[33] + y[47] +
            y[6] + y[13] + y[20] + y[27] + y[34] + y[48] +
            y[7] + y[14] + y[21] + y[28] + y[35] + y[49]) * p[60]) +

        # Entering on ART and Virally Suppressed
        ((y[36] + y[37] + y[38] + y[39] + y[40] + y[41] + y[42]) * p[61]))

    beta <- numerator / denominator
    message(paste("Beta =", round(beta, digits = 4)))
    beta
}
