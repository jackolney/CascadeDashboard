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

BuildCalibrationPlots <- function(data, originalData) {

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
    p1 <- p1 + theme(legend.position = "none", text = element_text(family = "Avenir Next"))
    # p1 <- p1 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p2 <- ggplot(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    p2 <- p2 + geom_ribbon(data = out2[out2$indicator == "PLHIV Diagnosed",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p2 <- p2 + geom_line()
    p2 <- p2 + geom_point(aes(color = weight), size = 3)
    p2 <- p2 + mycol
    p2 <- p2 + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, shading shows upper and lower model estimates")
    p2 <- p2 + theme(legend.position = "none", text = element_text(family = "Avenir Next"))

    # p2 <- p2 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p3 <- ggplot(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    p3 <- p3 + geom_ribbon(data = out2[out2$indicator == "PLHIV in Care",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p3 <- p3 + geom_line()
    p3 <- p3 + geom_point(aes(color = weight), size = 3)
    p3 <- p3 + mycol
    p3 <- p3 + ggtitle("PLHIV in Care", subtitle = "Points are data, shading shows upper and lower model estimates")
    p3 <- p3 + theme(legend.position = "none", text = element_text(family = "Avenir Next"))

    # p3 <- p3 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p4 <- ggplot(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    p4 <- p4 + geom_ribbon(data = out2[out2$indicator == "PLHIV on ART",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70")
    p4 <- p4 + geom_line()
    p4 <- p4 + geom_point(aes(color = weight), size = 3)
    p4 <- p4 + mycol
    p4 <- p4 + ggtitle("PLHIV on ART", subtitle = "Points are data, shading shows upper and lower model estimates")
    p4 <- p4 + theme(legend.position = "none", text = element_text(family = "Avenir Next"))

    # p4 <- p4 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p5 <- ggplot(out2[out2$year == 2010,][1:5,], aes(x = indicator, y = mean))
    p5 <- p5 + geom_bar(aes(fill = indicator), stat = "identity")
    p5 <- p5 + ggtitle("Cascade in 2010")
    p5 <- p5 + theme_classic()
    p5 <- p5 + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.title = element_blank())

    # p5 <- p5 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"), axis.title = element_blank())

    p6 <- ggplot(out2[out2$year == 2015,][1:5,], aes(x = indicator, y = mean))
    p6 <- p6 + geom_bar(aes(fill = indicator), stat = "identity")
    p6 <- p6 + geom_errorbar(mapping = aes(x = indicator, ymin = min, ymax = max), width = 0.2, size = 0.5)
    p6 <- p6 + geom_point(data = originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value), size = 2.5)
    p6 <- p6 + geom_point(data = originalData[["calib"]][originalData[["calib"]]$year == 2015 & originalData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value, color = weight), size = 2)
    p6 <- p6 + mycol
    p6 <- p6 + ggtitle("Cascade in 2015", subtitle = "Error bars illustrate result ranges, points are data")
    p6 <- p6 + theme_classic()
    p6 <- p6 + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.title = element_blank())

    # p6 <- p6 + theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"), axis.title = element_blank())

    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

}

BuildDataReviewPlot <- function(data) {
    ggOut <- ggplot(data, aes(x = year, y = value))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(
        breaks = base::pretty(seq(0, round(max(data$value), digits = 0)), n = 5),
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
    ggOut
}
