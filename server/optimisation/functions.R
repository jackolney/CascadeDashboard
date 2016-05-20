# The below parameters need to be fixed to the mean (from too, should be mean), length should be constant though!
GetParaMatrix <- function(calibParamOut) {
    ParRange <- expand.grid(
        Rho   = seq(from = if (input$TestingCheck) {
                input$userOptRho_Range[1]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 4)
            }, to = if (input$TestingCheck) {
                input$userOptRho_Range[2]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 4)
            }, length.out = input$optimParamLength
        ),
        Q     = seq(from = if (input$LinkageCheck) {
                input$userOptq_Range[1]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["q"]], digits = 4)
            }, to = if (input$LinkageCheck) {
                input$userOptq_Range[2]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["q"]], digits = 4)
            }, length.out = input$optimParamLength
        ),
        Kappa = seq(from = if (input$PreRetentionCheck) {
                input$userOptKappa_Range[2]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4)
            }, to = if (input$PreRetentionCheck) {
                input$userOptKappa_Range[1]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4)
            }, length.out = input$optimParamLength
        ),
        Gamma = seq(from = if (input$InitiationCheck) {
                input$userOptGamma_Range[1]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4)
            }, to = if (input$InitiationCheck) {
                input$userOptGamma_Range[2]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4)
            }, length.out = input$optimParamLength
        ),
        Sigma = seq(from = if (input$AdherenceCheck) {
                input$userOptSigma_Range[1]
            } else {
                0
            }, to = if (input$AdherenceCheck) {
                input$userOptSigma_Range[2]
            } else {
                0
            }, length.out = input$optimParamLength
        ),
        Omega = seq(from = if (input$RetentionCheck) {
                input$userOptOmega_Range[2]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4)
            }, to = if (input$RetentionCheck) {
                input$userOptOmega_Range[1]
            } else {
                round(lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4)
            }, length.out = input$optimParamLength
        )
    )
    out <- unique(ParRange)
    out
}

Calc_Cost <- function(out) {
    out$TotalCost[251]
}

Calc_DALY <- function(out) {
    sum(out$DALY)
}

BaselineModel <- function() {
    out <- CallModel()
    tmp <- array(unlist(out), c(dim(out[[1]]), length(out)), dimnames = c(dimnames(out[[1]]), NULL))
    res <- rowMeans(tmp, dims = 2)
    res <- as.data.frame(res) # Remove this at some point and just pass round a matrix (faster)
    res
}

Calc_DALYsAverted <- function(out, base_DALY) {
    return(base_DALY - Calc_DALY(out))
}

Calc_AdditionalCost <- function(out, base_COST) {
    return(out$TotalCost[251] - base_COST)
}

Calc_909090_Result <- function(out) {
    Extract909090DataSingle(out)$res
}

Calc_VS <- function(out) {
    out$Vs[251]
}

Calc_909090 <- function(out) {
    Extract909090DataSingle(out)
}

FindResults_909090 <- function(result) {
    res_list <- list()
    for (i in 1:length(result)) {
        print(i)
        t_909090 <- Calc_909090(result[[i]])
        test <- c(0,0,0)
        for (j in 1:length(t_909090$res)) {
            if (t_909090$res[j] > 0.9) {
                test[j] <- 1
            } else {
                test[j] <- 0
            }
        }
        if (sum(test) == 3) {
            res_list[[length(res_list) + 1]] <- result[[i]]
        }
    }
    return(res_list)
}

FindPar_909090 <- function(result, par) {
    res_list <- list()
    for (i in 1:length(result)) {
        print(i)
        t_909090 <- Calc_909090(result[[i]])
        Test <- c(0,0,0)
        for (j in 1:length(t_909090$res)) {
            if (t_909090$res[j] > 0.9) {
                Test[j] <- 1
            } else {
                Test[j] <- 0
            }
        }
        if (sum(Test) == 3) {
            res_list[[length(res_list) + 1]] <- par[i,]
        }
    }
    return(res_list)
}

Extract909090DataSingle <- function(data) {
    year <- 251
    PLHIV <- data$N[year]

    DX <- sum(
        data$Dx_500[year],      data$Dx_350500[year],       data$Dx_250350[year],       data$Dx_200250[year],       data$Dx_100200[year],       data$Dx_50100[year],        data$Dx_50[year],
        data$Care_500[year],    data$Care_350500[year],     data$Care_250350[year],     data$Care_200250[year],     data$Care_100200[year],     data$Care_50100[year],      data$Care_50[year],
        data$PreLtfu_500[year], data$PreLtfu_350500[year],  data$PreLtfu_250350[year],  data$PreLtfu_200250[year],  data$PreLtfu_100200[year],  data$PreLtfu_50100[year],   data$PreLtfu_50[year],
        data$Tx_Na_500[year],   data$Tx_Na_350500[year],    data$Tx_Na_250350[year],    data$Tx_Na_200250[year],    data$Tx_Na_100200[year],    data$Tx_Na_50100[year],     data$Tx_Na_50[year],
        data$Tx_A_500[year],    data$Tx_A_350500[year],     data$Tx_A_250350[year],     data$Tx_A_200250[year],     data$Tx_A_100200[year],     data$Tx_A_50100[year],      data$Tx_A_50[year],
        data$Ltfu_500[year],    data$Ltfu_350500[year],     data$Ltfu_250350[year],     data$Ltfu_200250[year],     data$Ltfu_100200[year],     data$Ltfu_50100[year],      data$Ltfu_50[year]
        )

    TX <- sum(
        data$Tx_Na_500[year], data$Tx_Na_350500[year], data$Tx_Na_250350[year], data$Tx_Na_200250[year], data$Tx_Na_100200[year], data$Tx_Na_50100[year], data$Tx_Na_50[year],
        data$Tx_A_500[year],  data$Tx_A_350500[year],  data$Tx_A_250350[year],  data$Tx_A_200250[year],  data$Tx_A_100200[year],  data$Tx_A_50100[year],  data$Tx_A_50[year]
        )

    VS <- sum(data$Tx_A_500[year], data$Tx_A_350500[year], data$Tx_A_250350[year], data$Tx_A_200250[year], data$Tx_A_100200[year], data$Tx_A_50100[year], data$Tx_A_50[year])

    un_90     <- DX / PLHIV
    un_9090   <- TX / DX
    un_909090 <- VS / TX

    res <- c(un_90, un_9090, un_909090)
    def <- c("% Diagnosed","% On Treatment","% Suppressed")
    scenario <- c("Baseline")
    df <- data.frame(def, res, scenario)
    df$def <- factor(df$def, levels = c("% Diagnosed", "% On Treatment", "% Suppressed"))
    df
}

GetBestCalibOut <- function(calibOut, minErrorRun) {
    out <- calibOut[calibOut$year == 2015 & calibOut$source == "model",][1:7 + 7 * (minErrorRun - 1),]
    out[, c("indicator", "value")]
}
