BuildCalibrationPlots_Report <- function(data, originalData) {

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
    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    p1 <- ggplot(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    p1 <- p1 + geom_ribbon(data = out2[out2$indicator == "PLHIV",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p1 <- p1 + geom_line()
    p1 <- p1 + geom_point(aes(color = weight), size = 3)
    p1 <- p1 + mycol
    p1 <- p1 + ggtitle("PLHIV", subtitle = "Points are data, shading shows upper and lower model estimates")
    p1 <- p1 + theme(legend.position = "none")
    p1 <- p1 + theme(axis.title = element_text(size = 8))
    p1 <- p1 + theme(axis.text = element_text(size = 8))
    p1 <- p1 + theme(title = element_text(size = 10))
    # p1 <- p1 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p2 <- ggplot(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    p2 <- p2 + geom_ribbon(data = out2[out2$indicator == "PLHIV Diagnosed",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p2 <- p2 + geom_line()
    p2 <- p2 + geom_point(aes(color = weight), size = 3)
    p2 <- p2 + mycol
    p2 <- p2 + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, shading shows upper and lower model estimates")
    p2 <- p2 + theme(legend.position = "none")
    p2 <- p2 + theme(axis.title = element_text(size = 8))
    p2 <- p2 + theme(axis.text = element_text(size = 8))
    p2 <- p2 + theme(title = element_text(size = 10))
    # p2 <- p2 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p3 <- ggplot(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    p3 <- p3 + geom_ribbon(data = out2[out2$indicator == "PLHIV in Care",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p3 <- p3 + geom_line()
    p3 <- p3 + geom_point(aes(color = weight), size = 3)
    p3 <- p3 + mycol
    p3 <- p3 + ggtitle("PLHIV in Care", subtitle = "Points are data, shading shows upper and lower model estimates")
    p3 <- p3 + theme(legend.position = "none")
    p3 <- p3 + theme(axis.title = element_text(size = 8))
    p3 <- p3 + theme(axis.text = element_text(size = 8))
    p3 <- p3 + theme(title = element_text(size = 10))
    # p3 <- p3 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p4 <- ggplot(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    p4 <- p4 + geom_ribbon(data = out2[out2$indicator == "PLHIV on ART",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p4 <- p4 + geom_line()
    p4 <- p4 + geom_point(aes(color = weight), size = 3)
    p4 <- p4 + mycol
    p4 <- p4 + ggtitle("PLHIV on ART", subtitle = "Points are data, shading shows upper and lower model estimates")
    p4 <- p4 + theme(legend.position = "none")
    p4 <- p4 + theme(axis.title = element_text(size = 8))
    p4 <- p4 + theme(axis.text = element_text(size = 8))
    p4 <- p4 + theme(title = element_text(size = 10))
    # p4 <- p4 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p5 <- ggplot(out2[out2$year == 2010,][1:5,], aes(x = indicator, y = mean))
    p5 <- p5 + geom_bar(aes(fill = indicator), stat = "identity")
    p5 <- p5 + ggtitle("Cascade in 2010")
    p5 <- p5 + theme_classic()
    p5 <- p5 + theme(legend.position = "none", axis.title = element_blank())
    p5 <- p5 + theme(axis.text.y = element_text(size = 8))
    p5 <- p5 + theme(axis.text.x = element_text(size = 5))
    p5 <- p5 + theme(title = element_text(size = 8))
    # p5 <- p5 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"), axis.title = element_blank())

    p6 <- ggplot(out2[out2$year == 2015,][1:5,], aes(x = indicator, y = mean))
    p6 <- p6 + geom_bar(aes(fill = indicator), stat = "identity")
    p6 <- p6 + geom_errorbar(mapping = aes(x = indicator, ymin = min, ymax = max), width = 0.2, size = 0.5)
    p6 <- p6 + geom_point(data = originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value), size = 2.5)
    p6 <- p6 + geom_point(data = originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value, color = weight), size = 2)
    p6 <- p6 + mycol
    p6 <- p6 + ggtitle("Cascade in 2015", subtitle = "Error bars illustrate result ranges, points are data")
    p6 <- p6 + theme_classic()
    p6 <- p6 + theme(legend.position = "none", axis.title = element_blank())
    p6 <- p6 + theme(axis.text.y = element_text(size = 8))
    p6 <- p6 + theme(axis.text.x = element_text(size = 5))
    p6 <- p6 + theme(title = element_text(size = 8))
    # p6 <- p6 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"), axis.title = element_blank())

    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

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
    df     <- out[[1]]
    dfData <- out[[2]]

    red    <- rgb(red = 223, green = 74,  blue = 50, max = 255)
    yellow <- rgb(red = 245, green = 157, blue = 0,  max = 255)
    green  <- rgb(red = 0,   green = 167, blue = 87, max = 255)
    cfill  <- c(red, yellow, green)
    cfill_violin <- c(
        rep(red,    length(dfData[dfData$defData == "% Diagnosed",    "value"])),
        rep(yellow, length(dfData[dfData$defData == "% On Treatment", "value"])),
        rep(green,  length(dfData[dfData$defData == "% Suppressed",   "value"]))
    )

    vbOut1 <- round(df[df$def == "% Diagnosed",    "res"] * 100, digits = 0)
    vbOut2 <- round(df[df$def == "% On Treatment", "res"] * 100, digits = 0)
    vbOut3 <- round(df[df$def == "% Suppressed",   "res"] * 100, digits = 0)

    output$vb_90            <- renderValueBox({ valueBox(paste0(vbOut1, "%"), "Diagnosed",          color = "red",    icon = icon("medkit", lib = "font-awesome")) })
    output$vb_9090          <- renderValueBox({ valueBox(paste0(vbOut2, "%"), "On Treatment",       color = "yellow", icon = icon("medkit", lib = "font-awesome")) })
    output$vb_909090        <- renderValueBox({ valueBox(paste0(vbOut3, "%"), "Virally Suppressed", color = "green",  icon = icon("medkit", lib = "font-awesome")) })
    output$vb_90_wizard     <- renderValueBox({ valueBox(paste0(vbOut1, "%"), "Diagnosed",          color = "red",    icon = icon("medkit", lib = "font-awesome")) })
    output$vb_9090_wizard   <- renderValueBox({ valueBox(paste0(vbOut2, "%"), "On Treatment",       color = "yellow", icon = icon("medkit", lib = "font-awesome")) })
    output$vb_909090_wizard <- renderValueBox({ valueBox(paste0(vbOut3, "%"), "Virally Suppressed", color = "green",  icon = icon("medkit", lib = "font-awesome")) })

    ggOut <- ggplot(df, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 1)
    # ggOut <- ggOut + geom_violin(data = dfData, mapping = aes(x = defData, y = value), stat = "ydensity",
        # position = "dodge", trim = TRUE, alpha = 1 / 2, fill = "black", size = 0, scale = "area")
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
        out[j] <-  mean(unlist(lapply(result, function(x) sum(x$NewInf[j]))))
        min[j] <- range(unlist(lapply(result, function(x) sum(x$NewInf[j]))))[1]
        max[j] <- range(unlist(lapply(result, function(x) sum(x$NewInf[j]))))[2]
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
        out[j] <-  mean(unlist(lapply(result, function(x) sum(x$HivMortality[j]))))
        min[j] <- range(unlist(lapply(result, function(x) sum(x$HivMortality[j]))))[1]
        max[j] <- range(unlist(lapply(result, function(x) sum(x$HivMortality[j]))))[2]
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
