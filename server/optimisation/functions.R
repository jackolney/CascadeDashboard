# The below parameters need to be fixed to the mean (from too, should be mean), length should be constant though!

GetParaMatrix <- function(calibParamOut, minErrorRun) {
    ParRange <- expand.grid(

        Rho   = seq(
            from = if (intSwitch$testing) {
                    CalibParamOut[minErrorRun, "rho"]
                } else {
                    CalibParamOut[minErrorRun, "rho"]
                },
            to = if (intSwitch$testing) {
                    CalibParamOut[minErrorRun, "rho"] * input$opt_rho_factor
                } else {
                    CalibParamOut[minErrorRun, "rho"]
                },
            length.out = 4
        ),

        Q     = seq(
            from = if (intSwitch$linkage) {
                    calibParamOut[minErrorRun, "q"]
                } else {
                    calibParamOut[minErrorRun, "q"]
                },
            to = if (intSwitch$linkage) {
                    input$opt_q_factor
                } else {
                    calibParamOut[minErrorRun, "q"]
                },
            length.out = 4
        ),

        Kappa = seq(
            from = if (intSwitch$preRetention) {
                    CalibParamOut[minErrorRun, "kappa"] / input$opt_kappa_factor
                } else {
                    CalibParamOut[minErrorRun, "kappa"]
                },
            to = if (intSwitch$preRetention) {
                    CalibParamOut[minErrorRun, "kappa"]
                } else {
                    CalibParamOut[minErrorRun, "kappa"]
                },
            length.out = 4
        ),

        Gamma = seq(
            from = if (intSwitch$initiation) {
                    CalibParamOut[minErrorRun, "gamma"]
                } else {
                    CalibParamOut[minErrorRun, "gamma"]
                },
            to = if (intSwitch$initiation) {
                    CalibParamOut[minErrorRun, "gamma"] * input$opt_gamma_factor
                } else {
                    CalibParamOut[minErrorRun, "gamma"]
                },
            length.out = 4
        ),

        Sigma = seq(
            from = if (intSwitch$adherence) {
                    0
                } else {
                    0
                },
            to = if (intSwitch$adherence) {
                input$opt_sigma_factor
                } else {
                    0
                },
            length.out = 4
        ),

        Omega = seq(
            from = if (intSwitch$retention) {
                    CalibParamOut[minErrorRun, "omega"] / input$opt_omega_factor
                } else {
                    CalibParamOut[minErrorRun, "omega"]
                },
            to = if (intSwitch$retention) {
                    CalibParamOut[minErrorRun, "omega"]
                } else {
                    CalibParamOut[minErrorRun, "omega"]
                },
            length.out = 4
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
