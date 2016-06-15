# Optimisation Test Bed

observeEvent(input$NEXT_optIntro, {
    # Run Optimsation Function
    optResult <<- RunOptimisation()
})

output$plotOptim_result <- renderPlot({
    input$NEXT_optIntro
    BuildOptimisationPlot(theOut = optResult)
}, height = 400, width = 'auto', bg = 'transparent')

BuildOptimisationPlot <- function(theOut) {

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(theOut, theOut$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    Intervention <- c(
        "Testing",
        "Linkage",
        "Pre-ART Retention",
        "Initiation",
        "Adherence",
        "ART Retention"
    )

    # The values used in uiOutput()
    Value <- c(
        round(  (cumsum(alt$Dx)[251]           - alt$Dx[1]          ) - (cumsum(baseline$Dx)[251]   - baseline$Dx[1]    ),   digits = 0),
        round(  (cumsum(alt$Care)[251]         - alt$Care[1]        ) - (cumsum(baseline$Care)[251] - baseline$Care[1]  ),   digits = 0),
        round(  (cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]) - (cumsum(alt$PreLtfu)[251]   - alt$PreLtfu[1]    ),   digits = 0),
        round(  (cumsum(alt$Tx)[251]           - alt$Tx[1]          ) - (cumsum(baseline$Tx)[251]   - baseline$Tx[1]    ),   digits = 0),
        round(  (cumsum(alt$Vs)[251]           - alt$Vs[1]          ) - (cumsum(baseline$Vs)[251]   - baseline$Vs[1]    ),   digits = 0),
        round(  (cumsum(baseline$Ltfu)[251]    - baseline$Ltfu[1]   ) - (cumsum(alt$Ltfu)[251]      - alt$Ltfu[1]       ),   digits = 0)
    )

    resultOut <- data.frame(Intervention, Value)
    resultOut$Intervention <- factor(resultOut$Intervention, levels = Intervention)

    ggOut <- ggplot(resultOut, aes(x = Intervention, y = Value, fill = Intervention))
    ggOut <- ggOut + geom_bar(stat = "identity")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle(
        label = "Required Changes to Care",
        subtitle =
            paste0("Average changes to care required to achieve ",
                input$opt_VS_cutoff,
                "% viral suppression"
            )
        )
    ggOut <- ggOut + theme(title = element_text(size = 15))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut
}

# Best Fit Calibration Plot
output$optCalibBestFit <- renderPlot({
    input$NEXT_calib
    BuildCalibrationBestFitPlot(data = CalibOut, originalData = KenyaData, limit = input$minResults, minErrorRun = minErrorRun)
}, height = 750, width = 'auto', bg = 'transparent')

BuildCalibrationBestFitPlot <- function(data, originalData, limit, minErrorRun) {

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
    ggOne <- ggOne + geom_point(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma)
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, red line denotes best fitting simulation")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 14))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 14))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 15))
    ggOne <- ggOne + theme(title =       element_text(size = 15))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = "Avenir Next"))
    ggOne <- ggOne + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_line(data = optimal[optimal$indicator == "PLHIV Diagnosed" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggTwo <- ggTwo + geom_point(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma)
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, red line denotes best fitting simulation")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 15))
    ggTwo <- ggTwo + theme(title =       element_text(size = 15))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + theme(text = element_text(family = "Avenir Next"))
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(out2[out2$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_line(data = optimal[optimal$indicator == "PLHIV in Care" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggThree <- ggThree + geom_point(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma)
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, red line denotes best fitting simulation")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 14))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 14))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 15))
    ggThree <- ggThree + theme(title =       element_text(size = 15))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + theme(text = element_text(family = "Avenir Next"))
    ggThree <- ggThree + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(out2[out2$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_line(data = optimal[optimal$indicator == "PLHIV on ART" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggFour <- ggFour + geom_point(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma)
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, red line denotes best fitting simulation")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 14))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 14))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 15))
    ggFour <- ggFour + theme(title =       element_text(size = 15))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + theme(text = element_text(family = "Avenir Next"))
    ggFour <- ggFour + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_line(data = optimal[optimal$indicator == "PLHIV Suppressed" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggFive <- ggFive + geom_point(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma)
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, red line denotes best fitting simulation")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 14))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 14))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 15))
    ggFive <- ggFive + theme(title =       element_text(size = 15))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + theme(text = element_text(family = "Avenir Next"))
    ggFive <- ggFive + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}

