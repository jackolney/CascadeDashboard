Calc_Cost <- function(out) {
    out$TotalCost[251]
}

Calc_DALY <- function(out) {
    sum(out$DALY)
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
            if (t_909090$res[j] >= 0.9) {
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
            if (t_909090$res[j] >= 0.9) {
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

ColorFromMiddle <- function(data, color1, color2) {
    max_val <- max(abs(data))
    JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
        max_val, color1, max_val, color1, color2, color2, max_val, max_val))
}

Calc_CareTesting <- function(baseResult, simResult) {
    baseAnswer <- baseResult$CumDiag[251] / 5
    simAnswer <- simResult$CumDiag[251] / 5
    out <- round(simAnswer - baseAnswer, digits = 0)
    out
}

Calc_CareLinkage <- function(baseResult, simResult) {
    baseAnswer <- baseResult$CumLink[251] / 5
    simAnswer <- simResult$CumLink[251] / 5
    out <- round(simAnswer - baseAnswer, digits = 0)
    out
}

Calc_CarePreRetention <- function(baseResult, simResult) {
    baseAnswer <- baseResult$CumPreL[251] / 5
    simAnswer <- simResult$CumPreL[251] / 5
    out <- round(simAnswer - baseAnswer, digits = 0)
    out
}

Calc_CareInitiation <- function(baseResult, simResult) {
    baseAnswer <- baseResult$CumInit[251] / 5
    simAnswer <- simResult$CumInit[251] / 5
    out <- round(simAnswer - baseAnswer, digits = 0)
    out
}

Calc_CareAdherence <- function(baseResult, simResult) {
    baseAnswer <- baseResult$CumAdhr[251] / 5
    simAnswer <- simResult$CumAdhr[251] / 5
    out <- round(simAnswer - baseAnswer, digits = 0)
    out
}

Calc_CareRetention <- function(baseResult, simResult) {
    baseAnswer <- baseResult$CumLoss[251] / 5
    simAnswer <- simResult$CumLoss[251] / 5
    out <- round(simAnswer - baseAnswer, digits = 0)
    out
}
