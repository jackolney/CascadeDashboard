BuildCalibrationPlot_Report <- function(data, calibData) {
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

    OGout <- calibData[calibData$year == 2015 & calibData$indicator != "PLHIV Retained",]
    # OGout$value <- as.numeric(OGout$value)

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)
    barFill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(out[out$year == 2015,][1:5,], aes(x = indicator, y = mean))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity")
    ggOut <- ggOut + scale_fill_manual(values = barFill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = indicator, ymin = lower, ymax = upper), width = 0.2, size = 0.5)
    ggOut <- ggOut + geom_point(data = OGout, aes(x = indicator, y = value), size = 3.5)
    ggOut <- ggOut + geom_point(data = OGout, aes(x = indicator, y = value, color = weight), size = 3)
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
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(title = element_text(size = 10))
    ggOut <- ggOut + theme(plot.subtitle = element_text(size = 8))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut
}

BuildCalibrationPlotDetail_Report <- function(data, originalData, limit) {
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
    ggOne <- ggOne + geom_point(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, lines represent each simulation")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 8))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 8))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 8))
    ggOne <- ggOne + theme(title =       element_text(size = 8))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_point(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, lines represent each simulation")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 8))
    ggTwo <- ggTwo + theme(title =       element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(out2[out2$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_point(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, lines represent each simulation")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 8))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 8))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 8))
    ggThree <- ggThree + theme(title =       element_text(size = 8))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(out2[out2$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_point(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, lines represent each simulation")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 8))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 8))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 8))
    ggFour <- ggFour + theme(title =       element_text(size = 8))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_point(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, lines represent each simulation")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 8))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 8))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 8))
    ggFive <- ggFive + theme(title =       element_text(size = 8))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}


BuildCalibrationHistogram_Report <- function(runError, maxError) {
    # Create data.frame to hold results
    run <- 1:length(runError)
    theError <- data.frame(run, runError)

    ggOut <- ggplot(theError, aes(runError))
    ggOut <- ggOut + geom_histogram(aes(fill = ..count..), bins = 30)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + geom_vline(xintercept = as.numeric(maxError))
    ggOut <- ggOut + scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title = element_text(size = 10))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + ylab("frequency")
    ggOut <- ggOut + xlab("error")
    ggOut
}

BuildCalibrationParameterHistGroup <- function() {
    ggA <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "rho")
    ggB <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "q")
    ggC <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "epsilon")
    ggD <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "kappa")
    ggE <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "gamma")
    ggF <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "theta")
    ggG <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "omega")
    ggH <- BuildCalibrationParamHist_Report(pOut = CalibParamOut, param = "p")

    gridExtra::grid.arrange(ggA, ggB, ggC, ggD, ggE, ggF, ggG, ggH, ncol = 4, nrow = 2)
}

BuildCalibrationParamHist_Report <- function(pOut, param) {
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


GenCascadePlot_Report <- function() {
    t0 <- ExtractCascadeData(1)   # t0 = 1
    t5 <- ExtractCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    t0$year <- 2015
    t5$year <- 2020
    out <- rbind(t0, t5)

    ggOne <- ggplot(out, aes(x = def, y = res))
    ggOne <- ggOne + geom_bar(aes(fill = as.factor(year)), position = 'dodge', stat = 'identity')
    ggOne <- ggOne + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max, fill = as.factor(year)), position = position_dodge(width = 0.9), stat = "identity", width = 0.2, size = 0.5)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = c(c.fill[2],c.fill[5]), guide = guide_legend(title = "Year"))
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(title = element_text(size = 18))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 8))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 8))
    ggOne <- ggOne + theme(axis.ticks.x = element_blank())
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(legend.title = element_text(size = 8))
    ggOne <- ggOne + theme(legend.text = element_text(size = 7))
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(axis.line.y = element_line())
    ggOne <- ggOne + expand_limits(y = round(max(out$max), digits = -4) + 1e5)
    ggOne
}

GenPowersCascadePlot_Report <- function() {
    t0 <- ExtractPowersCascadeData(1)
    t5 <- ExtractPowersCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggOne <- ggplot(t0, aes(x = order, y = res, fill = state))
    ggOne <- ggOne + geom_bar(stat = 'identity')
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = p.col)
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 10))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 7))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 8))
    ggOne <- ggOne + theme(axis.ticks.x = element_blank())
    ggOne <- ggOne + theme(legend.text = element_text(size = 8))
    ggOne <- ggOne + theme(legend.title = element_blank())
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggTwo <- ggplot(t5, aes(x = order, y = res, fill = state))
    ggTwo <- ggTwo + geom_bar(stat = 'identity')
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + scale_fill_manual(values = p.col)
    ggTwo <- ggTwo + ggtitle("2020")
    ggTwo <- ggTwo + theme_classic()
    ggTwo <- ggTwo + theme(plot.title = element_text(hjust = 0.5))
    ggTwo <- ggTwo + theme(title = element_text(size = 10))
    ggTwo <- ggTwo + theme(axis.title = element_blank())
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 7))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.ticks.x = element_blank())
    ggTwo <- ggTwo + theme(legend.text = element_text(size = 8))
    ggTwo <- ggTwo + theme(legend.title = element_blank())
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(plot.background = element_blank())
    ggTwo <- ggTwo + theme(panel.background = element_blank())

        # Same y-axis
    a <- sum(t0[t0$order == "All", "res"])
    b <- sum(t5[t5$order == "All", "res"])
    if (a >= b) {
        ggOne <- ggOne + expand_limits(y = round(a, digits = -4) + 1e5)
        ggTwo <- ggTwo + expand_limits(y = round(a, digits = -4) + 1e5)
    } else {
        ggOne <- ggOne + expand_limits(y = round(b, digits = -4) + 1e5)
        ggTwo <- ggTwo + expand_limits(y = round(b, digits = -4) + 1e5)
    }

    GridArrangeSharedLegend(ggOne, ggTwo, ncol = 2, nrow = 1, position = "bottom")

    # my.legend <- GrabLegend(ggOne)
    # l.width <- sum(my.legend$width)

    # gridExtra::grid.arrange(
    #     gridExtra::arrangeGrob(
    #         ggOne + theme(legend.position = "none"),
    #         ggTwo + theme(legend.position = "none"),
    #         ncol = 2),
    #     my.legend,
    #     widths = grid::unit.c(unit(1, "npc") - l.width, l.width),
    #     nrow = 1)
}

Gen909090Plot_Report <- function() {
    out    <- Extract909090Data()

    red    <- rgb(red = 223, green = 74,  blue = 50, max = 255)
    yellow <- rgb(red = 245, green = 157, blue = 0,  max = 255)
    green  <- rgb(red = 0,   green = 167, blue = 87, max = 255)
    cfill  <- c(red, yellow, green)

    ranking <- c(
        which(sort(out$res) == out$res[1]),
        which(sort(out$res) == out$res[2]),
        which(sort(out$res) == out$res[3])
    )

    ggOut <- ggplot(out, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
    ggOut <- ggOut + scale_fill_manual(values = cfill[ranking])
    ggOut <- ggOut + geom_abline(intercept = 0.9, slope = 0)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 10))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + geom_label(aes(x = def, label = scales::percent(round(out$res, digits = 2))))
    ggOut
}

GenNewInfPlot_Report <- function(wizard) {
    result <- CallModel()

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
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + ylab("Cumulative New Infections / Time")
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    if (wizard) {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    } else {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    }
    ggOut
}

GenAidsDeathsPlot_Report <- function(wizard) {
    result <- CallModel()

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
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + xlab("Year")
    ggOut <- ggOut + ylab("Cumulative AIDS Deaths / Time")
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    if (wizard) {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    } else {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    }
    ggOut
}

BuildCalibrationBestFitRunsPlot_Report <- function(data, originalData, limit, minErrorRun, selectedRuns, propRuns) {
    # subset the 'model' results (42 for each simulation, 6*7)
    modelledRuns <- data[data$source == "model",]

    dataPoints <- data[data$source == "data",]

    # sort runs by error (lowest to highest)
    orderedRuns <- order(runError[selectedRuns])

    # identify the best _% (10% by default)
    bestRuns <- orderedRuns[1:(length(orderedRuns) * propRuns)]

    # extract values for each indicator and bind together
    bestRunValues <- modelledRuns[1:42 + 42 * (bestRuns[1] - 1),]
    for(i in 2:length(bestRuns)) {
        bestRunValues <- rbind(bestRunValues, modelledRuns[1:42 + 42 * (bestRuns[i] - 1),])
    }

    # Find max / min (for y-limit of plots)
    modelledRuns <- AppendMinMaxMean(data[data$source == "model",])

    # re-factor indicators
    modelledRuns$indicator <- factor(modelledRuns$indicator, levels = c(
        "PLHIV",
        "PLHIV Diagnosed",
        "PLHIV in Care",
        "PLHIV on ART",
        "PLHIV Suppressed"
        )
    )

    # ZERO all weights
    modelledRuns$weight <- 0

    # Create 'sim' column for grouping runs
    # Six years by seven indicators
    modelledRuns$sim <- rep(x = 1:limit, each = 6 * 7)
    bestRunValues$sim <- rep(x = 1:(limit * 0.1), each = 6 * 7)

    # Set weight colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    ggOne <- ggplot()
    ggOne <- ggOne + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggOne <- ggOne + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggOne <- ggOne + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    ggOne <- ggOne + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, red line denotes best fitting simulation")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 8))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 8))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 8))
    ggOne <- ggOne + theme(title =       element_text(size = 8))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggTwo <- ggTwo + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, red line denotes best fitting simulation")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 8))
    ggTwo <- ggTwo + theme(title =       element_text(size = 8))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV in Care",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggThree <- ggThree + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, red line denotes best fitting simulation")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 8))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 8))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 8))
    ggThree <- ggThree + theme(title =       element_text(size = 8))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV on ART",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFour <- ggFour + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, red line denotes best fitting simulation")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 8))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 8))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 8))
    ggFour <- ggFour + theme(title =       element_text(size = 8))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFive <- ggFive + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, red line denotes best fitting simulation")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 8))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 8))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 8))
    ggFive <- ggFive + theme(title =       element_text(size = 8))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}

BuildDataReviewPlot_Report <- function(data) {
    data$indicator <- factor(data$indicator, levels = c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"))
    ggOut <- ggplot(data, aes(x = year, y = value))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
    ggOut <- ggOut + expand_limits(y = round(max(data$value), digits = -5))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        labels = scales::comma,
        expand = c(0, 0))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title = element_text(size = 10))
    ggOut <- ggOut + theme(legend.text = element_text(size = 8))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_blank())
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + xlab("Year")
    ggOut
}

BuildFrontierPlot_Report <- function(CalibParamOut, optResults) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]

    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)

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
    ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.title = element_text(size = 9))
    ggPlot <- ggPlot + theme(title = element_text(size = 10))
    ggPlot <- ggPlot + theme(plot.subtitle = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + xlab("Viral Suppression")
    ggPlot <- ggPlot + ylab("Additional Cost of Care")
    ggPlot <- ggPlot + ggtitle(label = "Cost-effectiveness Frontiers", subtitle = "Red frontiers indicate simulations achieving 73% viral suppression by 2020")
    ggPlot <- ggPlot + coord_cartesian(xlim = plotFrontier.ranges$x, ylim = plotFrontier.ranges$y)
    ggPlot
}

BuildCD4Plot2010_Report <- function(data) {
    Proportion <- as.numeric(data$cd4[2:15])
    ART <- c(rep("Off ART", 7), rep("On ART", 7))
    Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
    DF = data.frame(ART, Category, Proportion)

    DF_Off <- DF[1:7,]
    DF_Off$pos <- cumsum(DF_Off$Proportion) - DF_Off$Proportion / 2
    DF_Off$Category <- factor(DF_Off$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
    ggOff <- ggplot(DF_Off, aes(x = "", y = Proportion, fill = Category))
    ggOff <- ggOff + geom_bar(width = 1, stat = "identity")
    ggOff <- ggOff + theme_classic()
    ggOff <- ggOff + coord_polar(theta = "y")
    ggOff <- ggOff + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 3, show.legend = FALSE)
    ggOff <- ggOff + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
    ggOff <- ggOff + theme(legend.position = "none")
    ggOff <- ggOff + theme(axis.title = element_blank())
    ggOff <- ggOff + theme(legend.title = element_blank())
    ggOff <- ggOff + theme(axis.text = element_blank())
    ggOff <- ggOff + theme(axis.ticks = element_blank())
    ggOff <- ggOff + theme(plot.background = element_blank())
    ggOff <- ggOff + theme(legend.background = element_blank())
    ggOff <- ggOff + theme(panel.background = element_blank())
    ggOff <- ggOff + theme(legend.text = element_text(size = 8))
    ggOff <- ggOff + theme(legend.key.size = unit(0.5, "cm"))
    ggOff <- ggOff + ggtitle("Off ART", subtitle = "2010")
    ggOff <- ggOff + theme(plot.title = element_text(hjust = 0.5, size = 10))
    ggOff <- ggOff + theme(plot.subtitle = element_text(hjust = 0.5, size = 8))

    DF_On <- DF[8:14,]
    DF_On$pos <- cumsum(DF_On$Proportion) - DF_On$Proportion / 2
    DF_On$Category <- factor(DF_On$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
    ggOn <- ggplot(DF_On, aes(x = "", y = Proportion, fill = Category))
    ggOn <- ggOn + geom_bar(width = 1, stat = "identity")
    ggOn <- ggOn + theme_classic()
    ggOn <- ggOn + coord_polar(theta = "y")
    ggOn <- ggOn + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 3, show.legend = FALSE)
    ggOn <- ggOn + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
    ggOn <- ggOn + theme(legend.position = "none")
    ggOn <- ggOn + theme(axis.title = element_blank())
    ggOn <- ggOn + theme(legend.title = element_blank())
    ggOn <- ggOn + theme(axis.text = element_blank())
    ggOn <- ggOn + theme(axis.ticks = element_blank())
    ggOn <- ggOn + theme(plot.background = element_blank())
    ggOn <- ggOn + theme(legend.background = element_blank())
    ggOn <- ggOn + theme(panel.background = element_blank())
    ggOn <- ggOn + ggtitle("On ART", subtitle = "2010")
    ggOn <- ggOn + theme(plot.title = element_text(hjust = 0.5, size = 10))
    ggOn <- ggOn + theme(plot.subtitle = element_text(hjust = 0.5, size = 8))

    suppressWarnings(GridArrangeSharedLegend(ggOff, ggOn, ncol = 2, nrow = 1, position = "right"))
}

BuildCD4Plot2015_Report <- function(data) {
    Proportion <- as.numeric(data$cd4_2015[2:15])
    ART <- c(rep("Off ART", 7), rep("On ART", 7))
    Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
    DF = data.frame(ART, Category, Proportion)

    DF_Off <- DF[1:7,]
    DF_Off$pos <- cumsum(DF_Off$Proportion) - DF_Off$Proportion / 2
    DF_Off$Category <- factor(DF_Off$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
    ggOff <- ggplot(DF_Off, aes(x = "", y = Proportion, fill = Category))
    ggOff <- ggOff + geom_bar(width = 1, stat = "identity")
    ggOff <- ggOff + theme_classic()
    ggOff <- ggOff + coord_polar(theta = "y")
    ggOff <- ggOff + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 3, show.legend = FALSE)
    ggOff <- ggOff + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
    ggOff <- ggOff + theme(legend.position = "none")
    ggOff <- ggOff + theme(axis.title = element_blank())
    ggOff <- ggOff + theme(legend.title = element_blank())
    ggOff <- ggOff + theme(axis.text = element_blank())
    ggOff <- ggOff + theme(axis.ticks = element_blank())
    ggOff <- ggOff + theme(plot.background = element_blank())
    ggOff <- ggOff + theme(legend.background = element_blank())
    ggOff <- ggOff + theme(panel.background = element_blank())
    ggOff <- ggOff + theme(legend.text = element_text(size = 8))
    ggOff <- ggOff + theme(legend.key.size = unit(0.5, "cm"))
    ggOff <- ggOff + ggtitle("Off ART", subtitle = "2015")
    ggOff <- ggOff + theme(plot.title = element_text(hjust = 0.5, size = 10))
    ggOff <- ggOff + theme(plot.subtitle = element_text(hjust = 0.5, size = 8))

    DF_On <- DF[8:14,]
    DF_On$pos <- cumsum(DF_On$Proportion) - DF_On$Proportion / 2
    DF_On$Category <- factor(DF_On$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
    ggOn <- ggplot(DF_On, aes(x = "", y = Proportion, fill = Category))
    ggOn <- ggOn + geom_bar(width = 1, stat = "identity")
    ggOn <- ggOn + theme_classic()
    ggOn <- ggOn + coord_polar(theta = "y")
    ggOn <- ggOn + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 3, show.legend = FALSE)
    ggOn <- ggOn + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
    ggOn <- ggOn + theme(legend.position = "none")
    ggOn <- ggOn + theme(axis.title = element_blank())
    ggOn <- ggOn + theme(legend.title = element_blank())
    ggOn <- ggOn + theme(axis.text = element_blank())
    ggOn <- ggOn + theme(axis.ticks = element_blank())
    ggOn <- ggOn + theme(plot.background = element_blank())
    ggOn <- ggOn + theme(legend.background = element_blank())
    ggOn <- ggOn + theme(panel.background = element_blank())
    ggOn <- ggOn + ggtitle("On ART", subtitle = "2015")
    ggOn <- ggOn + theme(plot.title = element_text(hjust = 0.5, size = 10))
    ggOn <- ggOn + theme(plot.subtitle = element_text(hjust = 0.5, size = 8))

    suppressWarnings(GridArrangeSharedLegend(ggOff, ggOn, ncol = 2, nrow = 1, position = "right"))
}

BuildIncidencePlot_Report <- function(data) {
    dat <- reshape2::melt(data)
    theData <- subset(dat, dat$type == "Median")
    theData$lower <- subset(dat$value, dat$type == "Lower")
    theData$upper <- subset(dat$value, dat$type == "Upper")
    ggOut <- ggplot(data = theData, aes(x = variable, y = value, group = type))
    ggOut <- ggOut + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#4F8ABA")
    ggOut <- ggOut + geom_point(col = "black", size = 5)
    ggOut <- ggOut + geom_line(col = "black")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + expand_limits(y = round(max(theData$value), digits = -3))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.text = element_text(size = 8))
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut
}
