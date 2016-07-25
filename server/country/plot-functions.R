BuildDataReviewPlotMINI <- function(data) {
    data <- subset(data, !is.na(data$value))
    if (dim(data)[1] != 0) {
        data$indicator <- factor(data$indicator, levels = c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"))
        ggOut <- ggplot(data, aes(x = year, y = value))
        ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
        ggOut <- ggOut + expand_limits(y = round(max(data$value), digits = -5))
        ggOut <- ggOut + theme_classic()
        ggOut <- ggOut + scale_y_continuous(
            labels = scales::comma,
            expand = c(0, 0))
        ggOut <- ggOut + scale_x_continuous(breaks = seq(2010, 2015, 1), labels = seq(2010, 2015, 1))
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
        ggOut <- ggOut + theme(axis.title = element_text(size = 10))
        ggOut <- ggOut + theme(legend.text = element_text(size = 10))
        ggOut <- ggOut + theme(legend.position = "top")
        ggOut <- ggOut + theme(axis.line.x = element_line())
        ggOut <- ggOut + theme(axis.line.y = element_line())
        ggOut <- ggOut + theme(axis.title.y = element_blank())
        ggOut <- ggOut + theme(legend.title = element_blank())
        ggOut <- ggOut + theme(plot.background = element_blank())
        ggOut <- ggOut + theme(legend.background = element_blank())
        ggOut <- ggOut + theme(panel.background = element_blank())
        ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
        ggOut
    }
}
