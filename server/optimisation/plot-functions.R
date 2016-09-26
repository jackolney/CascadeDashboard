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
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, red lines denote best fitting simulations")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 14))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 14))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 15))
    ggOne <- ggOne + theme(title =       element_text(size = 15))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggTwo <- ggTwo + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, red lines denote best fitting simulations")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 15))
    ggTwo <- ggTwo + theme(title =       element_text(size = 15))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + theme(text = element_text(family = figFont))
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV in Care",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggThree <- ggThree + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, red lines denote best fitting simulations")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 14))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 14))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 15))
    ggThree <- ggThree + theme(title =       element_text(size = 15))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + theme(text = element_text(family = figFont))
    ggThree <- ggThree + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV on ART",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFour <- ggFour + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, red lines denote best fitting simulations")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 14))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 14))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 15))
    ggFour <- ggFour + theme(title =       element_text(size = 15))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + theme(text = element_text(family = figFont))
    ggFour <- ggFour + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFive <- ggFive + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, red lines denote best fitting simulations")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 14))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 14))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 15))
    ggFive <- ggFive + theme(title =       element_text(size = 15))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + theme(text = element_text(family = figFont))
    ggFive <- ggFive + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}

BuildCalibrationRandomFitRunsPlot <- function(data, originalData, limit, minErrorRun, selectedRuns, propRuns) {
    # subset the 'model' results (42 for each simulation, 6*7)
    modelledRuns <- data[data$source == "model",]

    dataPoints <- data[data$source == "data",]

    # sort runs by error (lowest to highest)
    orderedRuns <- order(runError[selectedRuns])

    # sort runs by error (lowest to highest)
    if (!exists("shuffledRuns")) {
        shuffledRuns <<- sample(orderedRuns)
    }

    # identify the best _% (10% by default)
    bestRuns <- shuffledRuns[1:(length(shuffledRuns) * propRuns)]

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
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, red lines denote best fitting simulations")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 14))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 14))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 15))
    ggOne <- ggOne + theme(title =       element_text(size = 15))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggTwo <- ggTwo + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, red lines denote best fitting simulations")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 15))
    ggTwo <- ggTwo + theme(title =       element_text(size = 15))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + theme(text = element_text(family = figFont))
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV in Care",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggThree <- ggThree + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, red lines denote best fitting simulations")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 14))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 14))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 15))
    ggThree <- ggThree + theme(title =       element_text(size = 15))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + theme(text = element_text(family = figFont))
    ggThree <- ggThree + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV on ART",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFour <- ggFour + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, red lines denote best fitting simulations")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 14))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 14))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 15))
    ggFour <- ggFour + theme(title =       element_text(size = 15))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + theme(text = element_text(family = figFont))
    ggFour <- ggFour + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(modelledRuns[modelledRuns$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.1, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = bestRunValues[bestRunValues$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = sim), col = "red", size = 1, alpha = 0.2)
    ggFive <- ggFive + geom_line(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_point(data = dataPoints[dataPoints$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, red lines denote best fitting simulations")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 14))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 14))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 15))
    ggFive <- ggFive + theme(title =       element_text(size = 15))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + theme(text = element_text(family = figFont))
    ggFive <- ggFive + expand_limits(y = c(0, round(max(modelledRuns$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
}


BuildFrontierPlot <- function(CalibParamOut, optResults) {

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
    ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 14))
    ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 14))
    ggPlot <- ggPlot + theme(axis.title = element_text(size = 15))
    ggPlot <- ggPlot + theme(title = element_text(size = 15))
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + xlab("Viral Suppression")
    ggPlot <- ggPlot + ylab("Additional Cost of Care")
    ggPlot <- ggPlot + ggtitle(label = "Cost-effectiveness Frontiers", subtitle = "Red frontiers indicate simulations achieving 73% viral suppression by 2020")
    ggPlot <- ggPlot + theme(text = element_text(family = figFont))
    ggPlot <- ggPlot + coord_cartesian(xlim = plotFrontier.ranges$x, ylim = plotFrontier.ranges$y)
    ggPlot
}

BuildOGCostImpactPlot <- function() {
    bestPar <- GetBestPar(
        masterCD4 = MasterData$cd4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    optResults <- dplyr::mutate(optResults,
        'Testing'           = round(as.numeric(optResults$Rho),   digits = 4),
        'Linkage'           = round(as.numeric(optResults$Q),     digits = 4),
        'Pre-ART Retention' = round(as.numeric(optResults$Kappa), digits = 4),
        'Initiation'        = round(as.numeric(optResults$Gamma), digits = 4),
        'Adherence'         = round(as.numeric(optResults$Sigma), digits = 4),
        'ART Retention'     = round(as.numeric(optResults$Omega), digits = 4)
    )

    optResults[["Testing"]]           <- factor(optResults[["Testing"]],           levels = unique(optResults[["Testing"]]))
    optResults[["Linkage"]]           <- factor(optResults[["Linkage"]],           levels = unique(optResults[["Linkage"]]))
    optResults[["Pre-ART Retention"]] <- factor(optResults[["Pre-ART Retention"]], levels = unique(optResults[["Pre-ART Retention"]]))
    optResults[["Initiation"]]        <- factor(optResults[["Initiation"]],        levels = unique(optResults[["Initiation"]]))
    optResults[["Adherence"]]         <- factor(optResults[["Adherence"]],         levels = unique(optResults[["Adherence"]]))
    optResults[["ART Retention"]]     <- factor(optResults[["ART Retention"]],     levels = unique(optResults[["ART Retention"]]))

    theStratPoint <<- input$userStratPoint

    ggOut <- ggplot(optResults, aes(x = VS, y = Cost))
    ggOut <- ggOut + geom_point(aes(color = as.factor(get(theStratPoint))), alpha = 0.75, size = 5)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + expand_limits(y = round(max(optResults$Cost), digits = -4))
    ggOut <- ggOut + scale_color_discrete(name = input$userStratPoint)
    ggOut <- ggOut + theme(legend.title = element_text(size = 14))
    ggOut <- ggOut + theme(legend.text = element_text(size = 13))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
    ggOut <- ggOut + theme(axis.title = element_text(size = 15))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + geom_vline(xintercept = input$opt_VS_cutoff / 100)
    ggOut <- ggOut + xlab("Proportion achieving viral suppression by 2020")
    ggOut <- ggOut + ylab("Additional cost of care (2013 USD)")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + coord_cartesian(xlim = plotOptimCostImpact.ranges$x, ylim = plotOptimCostImpact.ranges$y)
    ggOut
}

BuildChangesPlot <- function(CalibParamOut, optResults) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

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
    mRes[mRes$variable == "iAdhr", "variable"] <- "Adherence"
    mRes[mRes$variable == "iRetn", "variable"] <- "ART\nRetention"

    mRes$variable <- factor(mRes$variable, levels = c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention"))

    # Calculate means
    mRes$mean <- c(
        rep(mean(mRes[mRes$variable == "Testing","value"]), max(mRes$run)),
        rep(mean(mRes[mRes$variable == "Linkage","value"]), max(mRes$run)),
        rep(mean(mRes[mRes$variable == "Pre-ART\nRetention","value"]), max(mRes$run)),
        rep(mean(mRes[mRes$variable == "ART\nInitiation","value"]), max(mRes$run)),
        rep(mean(mRes[mRes$variable == "Adherence","value"]), max(mRes$run)),
        rep(mean(mRes[mRes$variable == "ART\nRetention","value"]), max(mRes$run))
    )

    names <- c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention")
    means <- c(
        mean(mRes[mRes$variable == "Testing","value"]),
        mean(mRes[mRes$variable == "Linkage","value"]),
        mean(mRes[mRes$variable == "Pre-ART\nRetention","value"]),
        mean(mRes[mRes$variable == "ART\nInitiation","value"]),
        mean(mRes[mRes$variable == "Adherence","value"]),
        mean(mRes[mRes$variable == "ART\nRetention","value"])
        )

    # Create labels df
    labels <- data.frame(names, means)

    # Assign as 'interventions'
    labels$strategy <- "Intervention"

    # Save mean values for text-output
    labels$values <- labels$means

    # Add baseline to values for stack-bar
    labels$means[1] <- labels$means[1] + (mean(BaselineTest) / 5)
    labels$means[2] <- labels$means[2] + (mean(BaselineLink) / 5)
    labels$means[3] <- labels$means[3] + (mean(BaselinePreR) / 5)
    labels$means[4] <- labels$means[4] + (mean(BaselineInit) / 5)
    labels$means[5] <- labels$means[5] + (mean(BaselineAdhr) / 5)
    labels$means[6] <- labels$means[6] + (mean(BaselineRetn) / 5)

    # Assign as 'interventions'
    mRes$strategy <- c("Intervention")


    # BIND IN THE MEAN BASELINE VALUES
    mRes <- rbind(mRes,
        data.frame(run = 1:max(mRes$run), variable = "Testing",            value = mean(BaselineTest) / 5, mean = mean(BaselineTest) / 5, strategy = "Baseline"),
        data.frame(run = 1:max(mRes$run), variable = "Linkage",            value = mean(BaselineLink) / 5, mean = mean(BaselineLink) / 5, strategy = "Baseline"),
        data.frame(run = 1:max(mRes$run), variable = "Pre-ART\nRetention", value = mean(BaselinePreR) / 5, mean = mean(BaselinePreR) / 5, strategy = "Baseline"),
        data.frame(run = 1:max(mRes$run), variable = "ART\nInitiation",    value = mean(BaselineInit) / 5, mean = mean(BaselineInit) / 5, strategy = "Baseline"),
        data.frame(run = 1:max(mRes$run), variable = "Adherence",          value = mean(BaselineAdhr) / 5, mean = mean(BaselineAdhr) / 5, strategy = "Baseline"),
        data.frame(run = 1:max(mRes$run), variable = "ART\nRetention",     value = mean(BaselineRetn) / 5, mean = mean(BaselineRetn) / 5, strategy = "Baseline")
    )

    # Add the mean to the data
    mRes[mRes$variable == "Testing"            & mRes$strategy == "Intervention", "value"] <- mRes[mRes$variable == "Testing"            & mRes$strategy == "Intervention", "value"] + (mean(BaselineTest) / 5)
    mRes[mRes$variable == "Linkage"            & mRes$strategy == "Intervention", "value"] <- mRes[mRes$variable == "Linkage"            & mRes$strategy == "Intervention", "value"] + (mean(BaselineLink) / 5)
    mRes[mRes$variable == "Pre-ART\nRetention" & mRes$strategy == "Intervention", "value"] <- mRes[mRes$variable == "Pre-ART\nRetention" & mRes$strategy == "Intervention", "value"] + (mean(BaselinePreR) / 5)
    mRes[mRes$variable == "ART\nInitiation"    & mRes$strategy == "Intervention", "value"] <- mRes[mRes$variable == "ART\nInitiation"    & mRes$strategy == "Intervention", "value"] + (mean(BaselineInit) / 5)
    mRes[mRes$variable == "Adherence"          & mRes$strategy == "Intervention", "value"] <- mRes[mRes$variable == "Adherence"          & mRes$strategy == "Intervention", "value"] + (mean(BaselineAdhr) / 5)
    mRes[mRes$variable == "ART\nRetention"     & mRes$strategy == "Intervention", "value"] <- mRes[mRes$variable == "ART\nRetention"     & mRes$strategy == "Intervention", "value"] + (mean(BaselineRetn) / 5)

    # Set levels
    mRes$strategy <- factor(mRes$strategy, levels = c("Intervention", "Baseline"))

    ggOut <- ggplot(mRes, aes(x = variable, y = value, fill = strategy))
    ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.1, position = "identity")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_fill_manual(values = c(brewer.pal(9, "Set1")[2], brewer.pal(9, "Set1")[1]))
    ggOut <- ggOut + ylab("Changes to Care Per Year")
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + expand_limits(y = c(0, round(max(mRes$value), digits = -4)))
    ggOut <- ggOut + scale_y_continuous(labels = scales::scientific, breaks = scales::pretty_breaks(n = 5), expand = c(0, 0))
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut <- ggOut + geom_errorbar(data = labels, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
    ggOut <- ggOut + geom_text(data = labels, aes(x = names, y = means, label = paste0("+", scales::comma(round(values, 0)))), vjust = -0.5, family = "Avenir Next")
    ggOut <- ggOut + theme(legend.position = 'bottom')
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut
}
