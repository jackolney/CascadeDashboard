BuildBaselinePlots <- function(data) {

    # Filter out all error values
    out <- data[data$source != "error",]

    # Sort out colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # A plot of model vs. data
    p1 <- ggplot(out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    p1 <- p1 + geom_line()
    p1 <- p1 + geom_point(aes(color = weight), size = 3)
    p1 <- p1 + mycol
    p1 <- p1 + ggtitle("PLHIV")

    p2 <- ggplot(out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    p2 <- p2 + geom_line()
    p2 <- p2 + geom_point(aes(color = weight), size = 3)
    p2 <- p2 + mycol
    p2 <- p2 + ggtitle("PLHIV Diagnosed")

    p3 <- ggplot(out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    p3 <- p3 + geom_line()
    p3 <- p3 + geom_point(aes(color = weight), size = 3)
    p3 <- p3 + mycol
    p3 <- p3 + ggtitle("PLHIV in Care")

    p4 <- ggplot(out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    p4 <- p4 + geom_line()
    p4 <- p4 + geom_point(aes(color = weight), size = 3)
    p4 <- p4 + mycol
    p4 <- p4 + ggtitle("PLHIV on ART")

    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
}

BuildBaselineErrorPlots <- function(data) {

    # Filter out all except error values
    out <- data[data$source == "error",]

    # A plot of model vs. data
    p1 <- ggplot(out[out$indicator == "PLHIV",], aes(x = year, y = value, group = source))
    p1 <- p1 + geom_line()
    p1 <- p1 + geom_point(aes(color = indicator, shape = source), size = 3)

    p2 <- ggplot(out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = source))
    p2 <- p2 + geom_line()
    p2 <- p2 + geom_point(aes(color = indicator, shape = source), size = 3)

    p3 <- ggplot(out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = source))
    p3 <- p3 + geom_line()
    p3 <- p3 + geom_point(aes(color = indicator, shape = source), size = 3)

    p4 <- ggplot(out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = source))
    p4 <- p4 + geom_line()
    p4 <- p4 + geom_point(aes(color = indicator, shape = source), size = 3)

    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
}

BuildCalibrationPlotDetail <- function(data, originalData) {
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

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    ggOne <- ggplot(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    ggOne <- ggOne + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma)
    ggOne <- ggOne + geom_line()
    ggOne <- ggOne + geom_point(aes(color = weight), size = 5)
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, shading shows upper and lower model estimates")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 14))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 14))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 15))
    ggOne <- ggOne + theme(title =       element_text(size = 15))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())

    ggTwo <- ggplot(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV Diagnosed",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma)
    ggTwo <- ggTwo + geom_line()
    ggTwo <- ggTwo + geom_point(aes(color = weight), size = 5)
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, shading shows upper and lower model estimates")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 15))
    ggTwo <- ggTwo + theme(title =       element_text(size = 15))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())

    ggThree <- ggplot(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV in Care",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma)
    ggThree <- ggThree + geom_line()
    ggThree <- ggThree + geom_point(aes(color = weight), size = 5)
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, shading shows upper and lower model estimates")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 14))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 14))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 15))
    ggThree <- ggThree + theme(title =       element_text(size = 15))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())

    ggFour <- ggplot(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV on ART",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma)
    ggFour <- ggFour + geom_line()
    ggFour <- ggFour + geom_point(aes(color = weight), size = 5)
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, shading shows upper and lower model estimates")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 14))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 14))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 15))
    ggFour <- ggFour + theme(title =       element_text(size = 15))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())

    ggFive <- ggplot(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV Suppressed",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma)
    ggFive <- ggFive + geom_line()
    ggFive <- ggFive + geom_point(aes(color = weight), size = 5)
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, shading shows upper and lower model estimates")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 14))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 14))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 15))
    ggFive <- ggFive + theme(title =       element_text(size = 15))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}

BuildCalibrationPlot <- function(data, originalData) {
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
    barFill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(out2[out2$year == 2015,][1:5,], aes(x = indicator, y = mean))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity")
    ggOut <- ggOut + scale_fill_manual(values = barFill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = indicator, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + geom_point(data = originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value), size = 5.5)
    ggOut <- ggOut + geom_point(data = originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value, color = weight), size = 5)
    ggOut <- ggOut + expand_limits(y = round(max(out2$max), digits = -5))
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
    ggOut <- ggOut + mycol
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle("Cascade in 2015", subtitle = "Error bars illustrate result ranges, points are data")
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 17))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 17))
    ggOut <- ggOut + theme(title = element_text(size = 18))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut
}


BuildDataReviewPlot <- function(data) {
    data$indicator <- factor(data$indicator, levels = c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"))
    ggOut <- ggplot(data, aes(x = year, y = value))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
    ggOut <- ggOut + expand_limits(y = round(max(data$value), digits = -5))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(
        breaks = base::pretty(seq(0, round(max(data$value), digits = -5)), n = 5),
        labels = scales::comma,
        expand = c(0, 0))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
    ggOut <- ggOut + theme(axis.title = element_text(size = 14))
    ggOut <- ggOut + theme(legend.text = element_text(size = 13))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.title.y = element_blank())
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + xlab("Year")
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut
}

BuildCalibrationHistogram <- function(runError, maxError) {
    # Create data.frame to hold results
    run <- 1:length(runError)
    theError <- data.frame(run, runError)

    ggOut <- ggplot(theError, aes(runError))
    ggOut <- ggOut + geom_histogram(aes(fill = ..count..), bins = 30)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + geom_vline(xintercept = as.numeric(maxError))
    ggOut <- ggOut + scale_y_continuous(expand = c(0,0))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.title = element_text(size = 12))
    ggOut <- ggOut + theme(legend.text = element_text(size = 12))
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + ylab("frequency")
    ggOut <- ggOut + xlab("error")
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut
}
