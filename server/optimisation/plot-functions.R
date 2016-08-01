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

    Value <- c(
        round(  (alt$CumDiag[251] / 5) - (baseline$CumDiag[251] / 5),   digits = 0),
        round(  (alt$CumLink[251] / 5) - (baseline$CumLink[251] / 5),   digits = 0),
        round(  (alt$CumPreL[251] / 5) - (baseline$CumPreL[251] / 5),   digits = 0),
        round(  (alt$CumInit[251] / 5) - (baseline$CumInit[251] / 5),   digits = 0),
        round(  (alt$CumAdhr[251] / 5) - (baseline$CumAdhr[251] / 5),   digits = 0),
        round(  (alt$CumLoss[251] / 5) - (baseline$CumLoss[251] / 5),   digits = 0)
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

BuildCalibrationBestFitRunsPlot <- function(data, originalData, limit, minErrorRun, selectedRuns, propRuns) {
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
    ggOne <- ggOne + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 5)
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
    ggOne <- ggOne + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggTwo <- ggTwo + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
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
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV in Care",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggThree <- ggThree + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 5)
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
    ggThree <- ggThree + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV on ART",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFour <- ggFour + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 5)
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
    ggFour <- ggFour + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFive <- ggFive + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
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
    ggFive <- ggFive + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}
