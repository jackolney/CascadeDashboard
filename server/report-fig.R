BuildCalibrationPlot_Report <- function(data, originalData) {
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
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = indicator, ymin = lower, ymax = upper), width = 0.2, size = 1)
    ggOut <- ggOut + geom_point(data = OGout, aes(x = indicator, y = value), size = 3.5)
    ggOut <- ggOut + geom_point(data = OGout, aes(x = indicator, y = value, color = weight), size = 3)
    if (round(max(out$upper), digits = -4) >= round(max(na.omit(OGout$value)), digits = -4)) {
        ggOut <- ggOut + expand_limits(y = round(max(out$upper), digits = -4) + 1e5)
    } else {
        ggOut <- ggOut + expand_limits(y = round(max(na.omit(OGout$value)), digits = -4) + 1e5)
    }
    ggOut <- ggOut + scale_y_continuous(expand = c(0, 0), labels = scales::comma)
    ggOut <- ggOut + mycol
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle("Cascade in 2015", subtitle = "Error bars illustrate 95% CI, points are data")
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(title = element_text(size = 10))
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
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma)
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
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma)
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
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma)
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
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma)
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
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma)
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
    ggOut <- ggOut + scale_y_continuous(expand = c(0,0))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title = element_text(size = 10))
    ggOut <- ggOut + theme(legend.text = element_text(size = 8))
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
    ggOut <- ggOut + scale_y_continuous(expand = c(0,0))
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

    ggOne <- ggplot(t0, aes(x = def, y = res))
    ggOne <- ggOne + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOne <- ggOne + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggOne <- ggOne + scale_fill_manual(values = c.fill)
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 10))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 5))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 8))
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())

    ggTwo <- ggplot(t5, aes(x = def, y = res))
    ggTwo <- ggTwo + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggTwo <- ggTwo + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 1)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggTwo <- ggTwo + scale_fill_manual(values = c.fill)
    ggTwo <- ggTwo + ggtitle("2020")
    ggTwo <- ggTwo + theme_classic()
    ggTwo <- ggTwo + theme(plot.title = element_text(hjust = 0.5))
    ggTwo <- ggTwo + theme(title = element_text(size = 10))
    ggTwo <- ggTwo + theme(axis.title = element_blank())
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 5))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 8))
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(plot.background = element_blank())
    ggTwo <- ggTwo + theme(panel.background = element_blank())

    # Expansion of y.axis
    if (max(t0$max) >= max(t5$max)) {
        ggOne <- ggOne + expand_limits(y = round(max(t0$max), digits = -4) + 1e5)
        ggTwo <- ggTwo + expand_limits(y = round(max(t0$max), digits = -4) + 1e5)
    } else {
        ggOne <- ggOne + expand_limits(y = round(max(t5$max), digits = -4) + 1e5)
        ggTwo <- ggTwo + expand_limits(y = round(max(t5$max), digits = -4) + 1e5)
    }

    grid.arrange(ggOne, ggTwo, nrow = 1, ncol = 2)
}

GenPowersCascadePlot_Report <- function() {
    t0 <- ExtractPowersCascadeData(1)
    t5 <- ExtractPowersCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggOne <- ggplot(t0, aes(x = order, y = res, fill = state))
    ggOne <- ggOne + geom_bar(stat = 'identity')
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggOne <- ggOne + scale_fill_manual(values = p.col)
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 10))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 5))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 8))
    ggOne <- ggOne + theme(legend.text = element_text(size = 5))
    ggOne <- ggOne + theme(legend.title = element_text(size = 5))
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggTwo <- ggplot(t5, aes(x = order, y = res, fill = state))
    ggTwo <- ggTwo + geom_bar(stat = 'identity')
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggTwo <- ggTwo + scale_fill_manual(values = p.col)
    ggTwo <- ggTwo + ggtitle("2020")
    ggTwo <- ggTwo + theme_classic()
    ggTwo <- ggTwo + theme(plot.title = element_text(hjust = 0.5))
    ggTwo <- ggTwo + theme(title = element_text(size = 10))
    ggTwo <- ggTwo + theme(axis.title = element_blank())
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 5))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 8))
    ggTwo <- ggTwo + theme(legend.text = element_text(size = 5))
    ggTwo <- ggTwo + theme(legend.title = element_text(size = 5))
    ggTwo <- ggTwo + theme(legend.position = "right")
    ggTwo <- ggTwo + theme(plot.background = element_blank())
    ggTwo <- ggTwo + theme(panel.background = element_blank())

    my.legend <- GrabLegend(ggOne)
    l.width <- sum(my.legend$width)

    grid.arrange(
        arrangeGrob(
            ggOne + theme(legend.position = "none"),
            ggTwo + theme(legend.position = "none"),
            ncol = 2),
        my.legend,
        widths = grid::unit.c(unit(1, "npc") - l.width, l.width),
        nrow = 1)
}

Gen909090Plot_Report <- function() {
    out    <- Extract909090Data()

    red    <- rgb(red = 223, green = 74,  blue = 50, max = 255)
    yellow <- rgb(red = 245, green = 157, blue = 0,  max = 255)
    green  <- rgb(red = 0,   green = 167, blue = 87, max = 255)
    cfill  <- c(red, yellow, green)

    vbOut1 <- round(out[out$def == "% Diagnosed",    "res"] * 100, digits = 0)
    vbOut2 <- round(out[out$def == "% On Treatment", "res"] * 100, digits = 0)
    vbOut3 <- round(out[out$def == "% Suppressed",   "res"] * 100, digits = 0)

    output$vb_90            <- renderValueBox({ valueBox(paste0(vbOut1, "%"), "Diagnosed",          color = "red",    icon = icon("medkit", lib = "font-awesome")) })
    output$vb_9090          <- renderValueBox({ valueBox(paste0(vbOut2, "%"), "On Treatment",       color = "yellow", icon = icon("medkit", lib = "font-awesome")) })
    output$vb_909090        <- renderValueBox({ valueBox(paste0(vbOut3, "%"), "Virally Suppressed", color = "green",  icon = icon("medkit", lib = "font-awesome")) })
    output$vb_90_wizard     <- renderValueBox({ valueBox(paste0(vbOut1, "%"), "Diagnosed",          color = "red",    icon = icon("medkit", lib = "font-awesome")) })
    output$vb_9090_wizard   <- renderValueBox({ valueBox(paste0(vbOut2, "%"), "On Treatment",       color = "yellow", icon = icon("medkit", lib = "font-awesome")) })
    output$vb_909090_wizard <- renderValueBox({ valueBox(paste0(vbOut3, "%"), "Virally Suppressed", color = "green",  icon = icon("medkit", lib = "font-awesome")) })

    ggOut <- ggplot(out, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
    ggOut <- ggOut + scale_fill_manual(values = cfill)
    ggOut <- ggOut + geom_abline(intercept = 0.9, slope = 0)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 10))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut
}

GenNewInfPlot_Report <- function(wizard) {
    result <- CallModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Rmisc::CI(unlist(lapply(result, function(x) sum(x$NewInf[j]))), ci = 0.95)
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
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + xlab("Year")
    ggOut <- ggOut + ylab("Cumulative New Infections / Time")
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    if (wizard) {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
        ggOut <- ggOut + theme(axis.title = element_text(size  = 12))
    } else {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
        ggOut <- ggOut + theme(axis.title = element_text(size  = 10))
    }
    ggOut
}

GenAidsDeathsPlot_Report <- function(wizard) {
    result <- CallModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Rmisc::CI(unlist(lapply(result, function(x) sum(x$HivMortality[j]))), ci = 0.95)
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
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + xlab("Year")
    ggOut <- ggOut + ylab("Cumulative AIDS Deaths / Time")
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    if (wizard) {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
        ggOut <- ggOut + theme(axis.title = element_text(size  = 12))
    } else {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
        ggOut <- ggOut + theme(axis.title = element_text(size  = 10))
    }
    ggOut
}

BuildCalibrationBestFitPlot_Report <- function(data, originalData, limit, minErrorRun) {

    optimal <- data[1:72 + 72 * (minErrorRun - 1),]

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

    optimal$sim <- "optimal"

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    ggOne <- ggplot()
    ggOne <- ggOne + geom_line(data = na.omit(out2[out2$indicator == "PLHIV",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggOne <- ggOne + geom_line(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    ggOne <- ggOne + geom_line(data = optimal[optimal$indicator == "PLHIV" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggOne <- ggOne + geom_point(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma)
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, red line denotes best fitting simulation")
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
    ggTwo <- ggTwo + geom_line(data = optimal[optimal$indicator == "PLHIV Diagnosed" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggTwo <- ggTwo + geom_point(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma)
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, red line denotes best fitting simulation")
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
    ggThree <- ggThree + geom_line(data = optimal[optimal$indicator == "PLHIV in Care" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggThree <- ggThree + geom_point(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma)
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, red line denotes best fitting simulation")
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
    ggFour <- ggFour + geom_line(data = optimal[optimal$indicator == "PLHIV on ART" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggFour <- ggFour + geom_point(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma)
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, red line denotes best fitting simulation")
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
    ggFive <- ggFive + geom_line(data = optimal[optimal$indicator == "PLHIV Suppressed" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggFive <- ggFive + geom_point(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 3)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma)
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, red line denotes best fitting simulation")
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

BuildDataReviewPlot_Report <- function(data) {
    data$indicator <- factor(data$indicator, levels = c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"))
    ggOut <- ggplot(data, aes(x = year, y = value))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
    ggOut <- ggOut + expand_limits(y = round(max(data$value), digits = -5))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(
        breaks = base::pretty(seq(0, round(max(data$value), digits = -5)), n = 5),
        labels = scales::comma,
        expand = c(0, 0))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title = element_text(size = 10))
    ggOut <- ggOut + theme(legend.text = element_text(size = 8))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.title.y = element_blank())
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + xlab("Year")
    ggOut
}
