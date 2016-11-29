BuildCalibPlot_Thesis <- function(data, originalData, limit) {
    # Find Minimums & Maximums & Mean of data.
    out <- AppendCI(data[data$source == "model",])
    out$indicator <- factor(out$indicator, levels = c(
        "PLHIV",
        "PLHIV Diagnosed",
        "PLHIV in Care",
        "PLHIV on ART",
        "PLHIV Suppressed"
        )
    )

    OGout <- originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",]

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)
    barFill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(out[out$year == 2015,][1:5,], aes(x = indicator, y = mean))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity")
    ggOut <- ggOut + scale_fill_manual(values = barFill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = indicator, ymin = lower, ymax = upper), width = 0.2, size = 0.5)
    ggOut <- ggOut + geom_point(data = OGout, aes(x = indicator, y = value), size = 5.5)
    ggOut <- ggOut + geom_point(data = OGout, aes(x = indicator, y = value, color = weight), size = 5)
    if (round(max(out$upper), digits = -4) >= round(max(na.omit(OGout$value)), digits = -4)) {
        ggOut <- ggOut + expand_limits(y = round(max(out$upper), digits = -4) + 1e5)
    } else {
        ggOut <- ggOut + expand_limits(y = round(max(na.omit(OGout$value)), digits = -4) + 1e5)
    }
    ggOut <- ggOut + scale_y_continuous(expand = c(0, 0), labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + mycol
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle("Cascade in 2015", subtitle = "Error bars illustrate 95% CI, points are data")
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(title = element_text(size = 13))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut
}

BuildCalibDetailPlot_Thesis <- function(data, originalData, limit) {
    # Subset data to show only 'data'
    out <- data[data$source == "data",]

    # Find Minimums & Maximums & Mean of data.
    out2 <- AppendMinMaxMean(data[data$source == "model",])
    out2$indicator <- factor(out2$indicator, levels = c(
        "PLHIV",
        "PLHIV Diagnosed",
        "PLHIV in Care",
        "PLHIV on ART",
        "PLHIV Suppressed"
        )
    )

    out2$weight <- 0

    # 6 for six years (2010 to 2015), and 7 for seven indicators
    out2$sim <- rep(x = 1:limit, each = 6 * 7)

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    ggOne <- ggplot()
    ggOne <- ggOne + geom_line(data = na.omit(out2[out2$indicator == "PLHIV",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggOne <- ggOne + geom_line(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    ggOne <- ggOne + geom_point(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, lines represent each simulation")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 12))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 12))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 12))
    ggOne <- ggOne + theme(title =       element_text(size = 13))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_point(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, lines represent each simulation")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 12))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 12))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 12))
    ggTwo <- ggTwo + theme(title =       element_text(size = 13))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + theme(text = element_text(family = figFont))
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(out2[out2$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_point(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, lines represent each simulation")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 12))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 12))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 12))
    ggThree <- ggThree + theme(title =       element_text(size = 13))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + theme(text = element_text(family = figFont))
    ggThree <- ggThree + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(out2[out2$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_point(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, lines represent each simulation")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 12))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 12))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 12))
    ggFour <- ggFour + theme(title =       element_text(size = 13))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + theme(text = element_text(family = figFont))
    ggFour <- ggFour + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_point(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, lines represent each simulation")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 12))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 12))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 12))
    ggFive <- ggFive + theme(title =       element_text(size = 13))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + theme(text = element_text(family = figFont))
    ggFive <- ggFive + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}

BuildCalibrationHistogram_Thesis <- function(runError, maxError) {
    # Create data.frame to hold results
    run <- 1:length(runError)
    theError <- data.frame(run, runError)

    ggOut <- ggplot(theError, aes(runError))
    ggOut <- ggOut + geom_histogram(aes(fill = ..count..), bins = 30)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + geom_vline(xintercept = as.numeric(maxError))
    ggOut <- ggOut + scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.title = element_text(size = 12))
    ggOut <- ggOut + theme(legend.text = element_text(size = 12))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + ylab("frequency")
    ggOut <- ggOut + xlab("error")
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut
}

BuildCalibrationParameterHistGroup_Thesis <- function() {
    ggA <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "rho")
    ggB <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "q")
    ggC <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "epsilon")
    ggD <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "kappa")
    ggE <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "gamma")
    ggF <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "theta")
    ggG <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "omega")
    ggH <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "p")

    gridExtra::grid.arrange(ggA, ggB, ggC, ggD, ggE, ggF, ggG, ggH, ncol = 4, nrow = 2)
}

BuildCalibrationParamHist_Thesis <- function(pOut, param) {
    out <- as.data.frame(CalibParamOut)
    ggOut <- ggplot(out, aes_string(param))
    ggOut <- ggOut + geom_histogram(aes(fill = ..count..), bins = 10)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title = element_text(size = 8))
    ggOut <- ggOut + theme(legend.text = element_text(size = 8))
    ggOut <- ggOut + theme(legend.position = "non")
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + ylab("frequency")
    ggOut
}

GetModel <- function() {
    MasterOut <- vector("list", dim(CalibParamOut)[1])

    # FOR EACH PARAMETER SET
    for (i in 1:dim(CalibParamOut)[1]) {

        p <- GetParameters(
            masterCD4 = MasterData$cd4_2015,
            data = MasterData,
            iterationParam = CalibParamOut[i,])

        # Now we need the initials.
        y <- GetInitial(
            p = p,
            iterationResult = CalibOut[CalibOut$year == 2015 & CalibOut$source == "model",][1:7 + 7 * (i - 1),],
            masterCD4 = MasterData$cd4_2015
            )

        p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[i,])

        MasterOut[[i]] <- RunSim_Abs(y = y, p = p)
    }
    return(MasterOut)
}

GetCascadeData <- function(year) {
    result <- GetModel()

    # We iterate across _all_ results, and present the mean, and ranges.
    # Also switched to absolute values, not proportions.
    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    CX_data <- unlist(lapply(result, function(x) sum(x$Care[year], x$ART[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    NX <- Quantile_95(NX_data)
    DX <- Quantile_95(DX_data)
    CX <- Quantile_95(CX_data)
    TX <- Quantile_95(TX_data)
    VS <- Quantile_95(VS_data)

    res <- c(NX[["mean"]], DX[["mean"]], CX[["mean"]], TX[["mean"]], VS[["mean"]])
    min <- c(NX[["lower"]], DX[["lower"]], CX[["lower"]], TX[["lower"]], VS[["lower"]])
    max <- c(NX[["upper"]], DX[["upper"]], CX[["upper"]], TX[["upper"]], VS[["upper"]])

    def <- c("PLHIV", "Diagnosed", "In Care", "Treatment", "Suppressed")
    df <- data.frame(def, res, min, max)
    df$def <- factor(df$def, levels = c("PLHIV", "Diagnosed", "In Care", "Treatment", "Suppressed"))
    df
}

GenCascadePlot_Thesis <- function() {
    t0 <- GetCascadeData(1)   # t0 = 1
    t5 <- GetCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    t0$year <- 2015
    t5$year <- 2020
    out <- rbind(t0, t5)

    ggOne <- ggplot(out, aes(x = def, y = res))
    ggOne <- ggOne + geom_bar(aes(fill = as.factor(year)), position = 'dodge', stat = 'identity')
    ggOne <- ggOne + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max, fill = as.factor(year)), position = position_dodge(width = 0.9), stat = "identity", width = 0.2, size = 0.5)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 9))
    ggOne <- ggOne + scale_fill_manual(values = c(c.fill[2],c.fill[5]), guide = guide_legend(title = ""))
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(title = element_text(size = 13))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 12))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 12))
    ggOne <- ggOne + theme(axis.ticks.x = element_blank())
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(legend.title = element_text(size = 12))
    ggOne <- ggOne + theme(legend.text = element_text(size = 12))
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(axis.line.y = element_line())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + expand_limits(y = round(max(out$max), digits = -5) + 1e5)
    ggOne
}

Gen909090Plot_Thesis <- function() {
    out    <- Get909090Data()

    cfill <- rev(brewer.pal(9,"Blues")[6:8])

    vbOut1 <- round(out[out$def == "Diagnosed / PLHIV",    "res"] * 100, digits = 0)
    vbOut2 <- round(out[out$def == "On Treatment / Diagnosed", "res"] * 100, digits = 0)
    vbOut3 <- round(out[out$def == "Virally Suppressed / On Treatment",   "res"] * 100, digits = 0)

    ggOut <- ggplot(out, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
    ggOut <- ggOut + scale_fill_manual(values = cfill)
    ggOut <- ggOut + geom_abline(intercept = 0.9, slope = 0)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 20))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + geom_label(aes(x = def, label = scales::percent(round(out$res, digits = 2))), size = 4)
    ggOut
}

Get909090Data <- function() {
    result <- GetModel()

    # Always aiming for 2020 here (5.02 / 0.02)
    year <- 251

    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    UN_90 <- Quantile_95(DX_data / NX_data)
    UN_9090 <- Quantile_95(TX_data / DX_data)
    UN_909090 <- Quantile_95(VS_data / TX_data)

    res <- c(UN_90[["mean"]], UN_9090[["mean"]], UN_909090[["mean"]])
    min <- c(UN_90[["lower"]], UN_9090[["lower"]], UN_909090[["lower"]])
    max <- c(UN_90[["upper"]], UN_9090[["upper"]], UN_909090[["upper"]])
    def <- c("Diagnosed / PLHIV", "On Treatment / Diagnosed", "Virally Suppressed / On Treatment")
    out <- data.frame(def, res, min, max)
    out$def <- factor(out$def, levels = c("Diagnosed / PLHIV", "On Treatment / Diagnosed", "Virally Suppressed / On Treatment"))
    out
}

GenPowersCascadePlot_Thesis <- function() {
    t0 <- GetPowersCascadeData(1)
    t5 <- GetPowersCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggOne <- ggplot(t0, aes(x = order, y = res, fill = state))
    ggOne <- ggOne + geom_bar(stat = 'identity')
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = p.col, guide = guide_legend(title = ""))
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 13))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 12))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 12))
    ggOne <- ggOne + theme(legend.text = element_text(size = 11))
    ggOne <- ggOne + theme(legend.title = element_text(size = 12))
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + theme(axis.line.y = element_line())

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggTwo <- ggplot(t5, aes(x = order, y = res, fill = state))
    ggTwo <- ggTwo + geom_bar(stat = 'identity')
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + scale_fill_manual(values = p.col, guide = guide_legend(title = ""))
    ggTwo <- ggTwo + ggtitle("2020")
    ggTwo <- ggTwo + theme_classic()
    ggTwo <- ggTwo + theme(plot.title = element_text(hjust = 0.5))
    ggTwo <- ggTwo + theme(title = element_text(size = 13))
    ggTwo <- ggTwo + theme(axis.title = element_blank())
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 12))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 12))
    ggTwo <- ggTwo + theme(legend.text = element_text(size = 11))
    ggTwo <- ggTwo + theme(legend.title = element_text(size = 12))
    ggTwo <- ggTwo + theme(legend.position = "right")
    ggTwo <- ggTwo + theme(plot.background = element_blank())
    ggTwo <- ggTwo + theme(panel.background = element_blank())
    ggTwo <- ggTwo + theme(text = element_text(family = figFont))
    ggTwo <- ggTwo + theme(axis.line.y = element_line())

    if (sum(t5[t5$order == "All","res"]) >= sum(t0[t0$order == "All","res"])) {
        val <- sum(t5[t5$order == "All","res"])
        ggOne <- ggOne + expand_limits(y = round(val, -5))
        ggTwo <- ggTwo + expand_limits(y = round(val, -5))
    } else {
        val <- sum(t0[t0$order == "All","res"])
        ggOne <- ggOne + expand_limits(y = round(val, -5))
        ggTwo <- ggTwo + expand_limits(y = round(val, -5))
    }


    my.legend <- GrabLegend(ggOne)
    l.width <- sum(my.legend$width)

    gridExtra::grid.arrange(
        gridExtra::arrangeGrob(
            ggOne + theme(legend.position = "none"),
            ggTwo + theme(legend.position = "none"),
            ncol = 2),
        my.legend,
        widths = grid::unit.c(unit(1, "npc") - l.width, l.width),
        nrow = 1)
}

GrabLegend <- function(a.ggplot) {
    tmp <- ggplot_gtable(ggplot_build(a.ggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

GetPowersCascadeData <- function(year) {
    result <- GetModel()

    UNDX <- mean(unlist(lapply(result, function(x) sum(x$UnDx[year]))))
    DX   <- mean(unlist(lapply(result, function(x) sum(x$Dx[year]))))
    CX   <- mean(unlist(lapply(result, function(x) sum(x$Care[year]))))
    PLX  <- mean(unlist(lapply(result, function(x) sum(x$PreLtfu[year]))))
    TXN  <- mean(unlist(lapply(result, function(x) sum(x$Tx[year] - x$Vs[year]))))
    VS   <- mean(unlist(lapply(result, function(x) sum(x$Vs[year]))))
    LX   <- mean(unlist(lapply(result, function(x) sum(x$Ltfu[year]))))

    res <- c(VS, TXN, CX, DX, UNDX, PLX, LX,
             VS, TXN, CX, DX, PLX, LX,
             VS, TXN, CX,
             VS, TXN,
             VS)

    state <- c("On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Undiagnosed", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)",
               "On ART\n virally suppressed")

    order <- c(rep("All"       ,7),
               rep("Diagnosed" ,6),
               rep("Care"      ,3),
               rep("Treatment" ,2),
               rep("Suppressed",1))

    df <- data.frame(state, res, order)
    df$order <- factor(df$order, levels = c("All", "Diagnosed", "Care", "Treatment", "Suppressed"))
    df$state <- factor(df$state, levels = c("On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Undiagnosed", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART"))
    df
}

GenNewInfPlot_Thesis <- function() {
    result <- GetModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Quantile_95(unlist(lapply(result, function(x) sum(x$NewInf[j]))))
        out[j] <- dat[["mean"]]
        min[j] <- dat[["lower"]]
        max[j] <- dat[["upper"]]
    }

    timeOne <- seq(0, 5, 0.02)
    NewInfOne <- out / timeOne
    minOne <- min / timeOne
    maxOne <- max / timeOne

    time <- c(2, seq(51, 251, 1 * (1/0.02)))
    NewInf <- NewInfOne[time]
    min <- minOne[time]
    max <- maxOne[time]

    timeOut <- seq(2015, 2020, 1)
    df <- data.frame(timeOut, NewInf, min, max)

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(df, aes(x = timeOut, NewInf))
    ggOut <- ggOut + geom_bar(stat = "identity", size = 2, fill = c.fill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + expand_limits(y = round(max(df$max), digits = -4))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.title =  element_blank())
    ggOut <- ggOut + expand_limits(y = round(max(df$max), -5))
    ggOut
}

GenAidsDeathsPlot_Thesis <- function(wizard) {
    result <- GetModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Quantile_95(unlist(lapply(result, function(x) sum(x$HivMortality[j]))))
        out[j] <- dat[["mean"]]
        min[j] <- dat[["lower"]]
        max[j] <- dat[["upper"]]
    }

    timeOne <- seq(0, 5, 0.02)
    HivMortalityOne <- out / timeOne
    minOne <- min / timeOne
    maxOne <- max / timeOne

    time <- c(2, seq(51, 251, 1 * (1/0.02)))
    HivMortality <- HivMortalityOne[time]
    min <- minOne[time]
    max <- maxOne[time]

    timeOut <- seq(2015, 2020, 1)
    df <- data.frame(timeOut, HivMortality, min, max)

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(df, aes(x = timeOut, HivMortality))
    ggOut <- ggOut + geom_bar(stat = "identity", size = 2, fill = c.fill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + expand_limits(y = round(max(df$max), digits = -4))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.title =  element_blank())
    ggOut <- ggOut + expand_limits(y = 7e4)
    ggOut
}

GenPieChartComparison_Thesis <- function() {
    t0 <- GetPowersCascadeData(1)
    t5 <- GetPowersCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    t2015 <- t0[t0$order == "All",]
    t2020 <- t5[t5$order == "All",]

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    # Calculate proportion
    t2015$prop <- t2015$res / sum(t2015$res)
    t2020$prop <- t2020$res / sum(t2020$res)

    # Position
    t2015$pos <- 1-(cumsum(t2015$prop) - t2015$prop / 2)
    t2020$pos <- 1-(cumsum(t2020$prop) - t2020$prop / 2)

    gg2015 <- ggplot(t2015, aes(x = "", y = prop, fill = state))
    gg2015 <- gg2015 + geom_bar(width = 1, stat = "identity")
    gg2015 <- gg2015 + theme_classic()
    gg2015 <- gg2015 + coord_polar(theta = "y")
    gg2015 <- gg2015 + geom_label_repel(aes(y = pos, label = scales::percent(round(prop, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
    gg2015 <- gg2015 + theme(text = element_text(family = figFont))
    gg2015 <- gg2015 + scale_fill_manual(values = p.col)
    gg2015 <- gg2015 + theme(legend.position = "right")
    gg2015 <- gg2015 + theme(axis.title = element_blank())
    gg2015 <- gg2015 + theme(legend.title = element_blank())
    gg2015 <- gg2015 + theme(axis.text = element_blank())
    gg2015 <- gg2015 + theme(axis.line.x = element_blank())
    gg2015 <- gg2015 + theme(axis.line.y = element_blank())
    gg2015 <- gg2015 + theme(axis.ticks = element_blank())
    gg2015 <- gg2015 + theme(plot.background = element_blank())
    gg2015 <- gg2015 + theme(legend.background = element_blank())
    gg2015 <- gg2015 + theme(panel.background = element_blank())
    gg2015 <- gg2015 + theme(legend.text = element_text(size = 12))
    gg2015 <- gg2015 + theme(legend.key.size = unit(1, "cm"))
    gg2015 <- gg2015 + ggtitle("2015")
    gg2015 <- gg2015 + theme(plot.title = element_text(hjust = 0.5, size = 13))

    gg2020 <- ggplot(t2020, aes(x = "", y = prop, fill = state))
    gg2020 <- gg2020 + geom_bar(width = 1, stat = "identity")
    gg2020 <- gg2020 + theme_classic()
    gg2020 <- gg2020 + coord_polar(theta = "y")
    gg2020 <- gg2020 + geom_label_repel(aes(y = pos, label = scales::percent(round(prop, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
    gg2020 <- gg2020 + theme(text = element_text(family = figFont))
    gg2020 <- gg2020 + scale_fill_manual(values = p.col)
    gg2020 <- gg2020 + theme(legend.position = "none")
    gg2020 <- gg2020 + theme(axis.title = element_blank())
    gg2020 <- gg2020 + theme(legend.title = element_blank())
    gg2020 <- gg2020 + theme(axis.text = element_blank())
    gg2020 <- gg2020 + theme(axis.line.x = element_blank())
    gg2020 <- gg2020 + theme(axis.line.y = element_blank())
    gg2020 <- gg2020 + theme(axis.ticks = element_blank())
    gg2020 <- gg2020 + theme(plot.background = element_blank())
    gg2020 <- gg2020 + theme(legend.background = element_blank())
    gg2020 <- gg2020 + theme(panel.background = element_blank())
    gg2020 <- gg2020 + theme(legend.text = element_text(size = 12))
    gg2020 <- gg2020 + theme(legend.key.size = unit(1, "cm"))
    gg2020 <- gg2020 + ggtitle("2020")
    gg2020 <- gg2020 + theme(plot.title = element_text(hjust = 0.5, size = 13))

    gridExtra::grid.arrange(gg2015, gg2020, ncol = 2, nrow = 1)

    my.legend <- GrabLegend(gg2015)
    l.width <- sum(my.legend$width)

    gridExtra::grid.arrange(
        gridExtra::arrangeGrob(
            gg2015 + theme(legend.position = "none"),
            gg2020 + theme(legend.position = "none"),
            ncol = 2),
        my.legend,
        widths = grid::unit.c(unit(1, "npc") - l.width, l.width),
        nrow = 1)
}

GenDiscreteCascade_Thesis <- function() {
    t0 <- GetPowersCascadeData(1)
    t5 <- GetPowersCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    t2015 <- t0[t0$order == "All",]
    t2020 <- t5[t5$order == "All",]

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    # Calculate proportion
    t2015$prop <- t2015$res / sum(t2015$res)
    t2020$prop <- t2020$res / sum(t2020$res)

    # Position
    t2015$pos <- 1-(cumsum(t2015$prop) - t2015$prop / 2)
    t2020$pos <- 1-(cumsum(t2020$prop) - t2020$prop / 2)


    tcomp <- reshape2::melt(rbind(t2015, t2020))

    # states <- c(as.character(t2015$state), as.character(t2020$state))
    state <- c(
        "On ART\n virally suppressed",
        "On ART\n (non-adherent)",
        "In care,\nnot on ART",
        "Diagnosed,\nnot in care",
        "Undiagnosed",
        "Diagnosed,\nLTFU pre-ART",
        "Diagnosed,\nLTFU post-ART"
    )
    states <- rep(state, 2)
    proportion <- c(t2015$prop, t2020$prop)
    year <- c(rep(2015, 7), rep(2020, 7))

    out <- data.frame(states, year, proportion)

    out$states <- factor(out$states, levels = c(
        "Undiagnosed",
        "Diagnosed,\nnot in care",
        "In care,\nnot on ART",
        "Diagnosed,\nLTFU pre-ART",
        "On ART\n virally suppressed",
        "On ART\n (non-adherent)",
        "Diagnosed,\nLTFU post-ART"
    ))

    ggOut <- ggplot(out, aes(x = states, y = proportion, group = year))
    ggOut <- ggOut + geom_bar(aes(fill = as.factor(year)), stat = "identity", position = "dodge")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + scale_fill_manual(values = brewer.pal(9, "Set1"))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_y_continuous(expand = c(0, 0), labels = scales::percent, limits = c(0, 0.8))
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(legend.text = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + ylab("Proportion of PLHIV")
    ggOut <- ggOut + geom_text(aes(x = states, label = scales::percent(round(proportion, 2))), position = position_dodge(0.9), vjust = -0.25)
    ggOut
}

BuildFrontierPlot_Thesis <- function(CalibParamOut, optResults, target = target) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]

    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength, target = target)

    optResults$sim <- rep(x = 1:(dim(optResults)[1] / simLength), each = simLength)

    allRuns <- GetFrontiers(simData = optResults, optRuns = 1:(dim(optResults)[1] / simLength), simLength = simLength)

    interpol <- list()
    for(n in 1:(dim(optResults)[1] / simLength)) {
        lower <- (1 + simLength * (n - 1))
        upper <- (simLength + simLength * (n - 1))
        vals <- optResults[lower:upper,]

        interpolation <- approx(x = vals[,"VS"][allRuns[[n]]], y = vals[,"Cost"][allRuns[[n]]])
        interpol[[n]] <- interpolation
    }

    ggPlot <- ggplot(optResults, aes(x = VS, y = Cost))
    ggPlot <- ggPlot + geom_point(col = '#4F8ABA', alpha = 0.2)
    for(n in 1:(dim(optResults)[1] / simLength)) {
        ggPlot <- ggPlot + geom_line(data = as.data.frame(interpol[[n]]), mapping = aes(x = x, y = y), col = 'black', alpha = 0.2, size = 0.5)
    }
    for(n in 1:length(optRuns)) {
        ggPlot <- ggPlot + geom_line(data = as.data.frame(interpol[[optRuns[n]]]), mapping = aes(x = x, y = y), col = "red", alpha = 0.5, size = 0.75)
    }
    ggPlot <- ggPlot + geom_vline(xintercept = 0.9^3, alpha = 1)
    ggPlot <- ggPlot + theme_classic()
    ggPlot <- ggPlot + expand_limits(y = round(max(optResults$Cost), digits = -9))
    ggPlot <- ggPlot + scale_y_continuous(labels = scales::scientific, breaks = scales::pretty_breaks(n = 5))
    ggPlot <- ggPlot + scale_x_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 5))
    ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 10))
    ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 10))
    ggPlot <- ggPlot + theme(axis.title = element_text(size = 11))
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + xlab("Viral Suppression")
    ggPlot <- ggPlot + ylab("Additional Cost of Care")
    ggPlot <- ggPlot + theme(text = element_text(family = figFont))
    ggPlot
}

BuildChangesPlot_Thesis <- function(CalibParamOut, optResults, target) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength, target = target)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = target)

    # Result Formatting
    intResult <- intResult[,c("iTest","iLink","iPreR","iInit","iAdhr","iRetn")]
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0


    # Assign a 'run' number to simulations
    intResult$run <- 1:dim(intResult)[1]

    # Melt them
    mRes <- reshape2::melt(intResult, id = "run")

    ## DIVIDE ALL VALUES BY FIVE
    # Conversion from 5 year values to single years
    mRes$value <- mRes$value / 5


    # RENAME VARIABLES
    mRes$variable <- as.character(mRes$variable)
    mRes[mRes$variable == "iTest", "variable"] <- "Testing"
    mRes[mRes$variable == "iLink", "variable"] <- "Linkage"
    mRes[mRes$variable == "iPreR", "variable"] <- "Pre-ART\nRetention"
    mRes[mRes$variable == "iInit", "variable"] <- "ART\nInitiation"
    mRes[mRes$variable == "iAdhr", "variable"] <- "Viral\nSuppression"
    mRes[mRes$variable == "iRetn", "variable"] <- "ART\nRetention"

    mRes$variable <- factor(mRes$variable, levels = c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "ART\nRetention", "Viral\nSuppression"))

    # EDITS FROM HERE
    variable <- c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "ART\nRetention", "Viral\nSuppression")

    mean <- c(
        Quantile_95(mRes[mRes$variable == "Testing", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Viral\nSuppression", "value"])[["mean"]]
    )

    upper <- c(
        Quantile_95(mRes[mRes$variable == "Testing", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Viral\nSuppression", "value"])[["upper"]]
    )

    lower <- c(
        Quantile_95(mRes[mRes$variable == "Testing", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Viral\nSuppression", "value"])[["lower"]]
    )

    strategy <- "Intervention"

    outData <- data.frame(variable, mean, lower, upper, strategy)

    theBase <-  rbind(
        data.frame(variable = "Testing",            mean = mean(BaselineTest) / 5, strategy = "Baseline"),
        data.frame(variable = "Linkage",            mean = mean(BaselineLink) / 5, strategy = "Baseline"),
        data.frame(variable = "Pre-ART\nRetention", mean = mean(BaselinePreR) / 5, strategy = "Baseline"),
        data.frame(variable = "ART\nInitiation",    mean = mean(BaselineInit) / 5, strategy = "Baseline"),
        data.frame(variable = "ART\nRetention",     mean = mean(BaselineRetn) / 5, strategy = "Baseline"),
        data.frame(variable = "Viral\nSuppression", mean = mean(BaselineAdhr) / 5, strategy = "Baseline")
        )

    theBase$upper <- NA
    theBase$lower <- NA

    final <- rbind(theBase, outData)
    final$strategy <- factor(final$strategy, levels = c("Intervention", "Baseline"))

    # Now we need the error_bar data.frame
    value <- mean
    mean <- mean + theBase$mean
    upper <- upper + theBase$mean
    lower <- lower + theBase$mean
    theLabel <- data.frame(variable, value, mean, upper, lower)


    ggOut <- ggplot(final, aes(x = variable, y = mean, fill = strategy))
    ggOut <- ggOut + geom_bar(stat = "identity", alpha = 1)
    ggOut <- ggOut + geom_errorbar(data = theLabel, aes(x = variable, y = mean, ymax = upper, ymin = lower), alpha = 1, width = 0.25, size = 0.5)
    ggOut <- ggOut + geom_label(data = theLabel, aes(x = variable, y = mean, label = paste0("+", scales::comma(round(value, 0)))), vjust = +0.5, family = "Avenir Next", colour = "black", size = 3, alpha = 1, show.legend = FALSE)
    ggOut <- ggOut + scale_fill_manual(values = c("#4F8ABA","#E41A1C"))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ylab("Changes to Care Per Year")
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5), expand = c(0, 0))
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut <- ggOut + theme(legend.position = 'right')
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
    ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut
}
