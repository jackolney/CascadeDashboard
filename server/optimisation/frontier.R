WhichAchieved73 <- function(simData, simLength) {
    simRepeats <- dim(simData)[1] / simLength
    ach73 <- c()
    iter <- 1L
    for(n in 1:simRepeats) {
        lower <- (1 + simLength * (n - 1))
        upper <- (simLength + simLength * (n - 1))
        vals <- simData[lower:upper,]
        if (any(vals[,"VS"] >= (0.9^3))) {
            ach73[iter] <- n
            iter <- iter + 1L
        }
    }
    ach73
}

GetFrontiers <- function(simData, optRuns, simLength) {
    frontierList <- list()
    for(n in 1:length(optRuns)) {
        lower <- (1 + simLength * (optRuns[n] - 1))
        upper <- (simLength + simLength * (optRuns[n] - 1))
        vals <- simData[lower:upper,]
        frontierList[[n]] <- FindFrontier(x = vals$VS, y = vals$Cost)
    }
    frontierList
}

PlotInterpolation <- function(vs, indicator, target) {
    interpolation <- approx(x = vs, y = indicator)
    intIndex <- which.min(abs(target - interpolation$x))
    interpolation$y[intIndex]

    dat <- data.frame(vs, indicator)
    inter <- as.data.frame(interpolation)

    ggPlot <- ggplot(dat, aes(x = vs, y = indicator))
    ggPlot <- ggPlot + geom_line(data = inter, mapping = aes(x = x, y = y), alpha = 0.5, col = 'red')
    ggPlot <- ggPlot + geom_point(alpha = 0.2)
    ggPlot <- ggPlot + geom_point(alpha = 0.5, col = 'red')
    ggPlot <- ggPlot + geom_vline(xintercept = interpolation$x[intIndex], alpha = 0.5)
    ggPlot <- ggPlot + geom_hline(yintercept = interpolation$y[intIndex], alpha = 0.5)
    ggPlot <- ggPlot + theme_classic()
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + expand_limits(y = round(max(indicator), digits = -9))
    ggPlot <- ggPlot + scale_y_continuous(labels = scales::scientific, breaks = scales::pretty_breaks())
    ggPlot <- ggPlot + scale_x_continuous(labels = scales::percent, breaks = scales::pretty_breaks())
    ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.title = element_text(size = 9))
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + theme(legend.title = element_text(size = 9))
    ggPlot <- ggPlot + theme(legend.text = element_text(size = 8))
    ggPlot <- ggPlot + xlab("Viral Suppression")
    ggPlot <- ggPlot + ylab("Additional Cost of Care")
    ggPlot <- ggPlot + theme(text = element_text(family = "Avenir Next"))
    ggPlot <- ggPlot + annotate(geom = "text", x = 0.62, y = round(interpolation$y[intIndex], -8), label = paste("73% viral suppression\nat a cost of", scales::dollar(interpolation$y[intIndex])), family = "Avenir Next")
    ggPlot
}

Interpolate <- function(vs, indicator, target) {
    interpolation <- approx(x = vs, y = indicator)
    intIndex <- which.min(abs(target - interpolation$x))
    interpolation$y[intIndex]
}

RunInterpolation <- function(simData, optRuns, simLength, frontierList) {
    iCost <- c()
    iTest <- c()
    iLink <- c()
    iPreR <- c()
    iInit <- c()
    iAdhr <- c()
    iRetn <- c()
    iTCst <- c()

    for(n in 1:length(optRuns)) {
        lower <- (1 + simLength * (optRuns[n] - 1))
        upper <- (simLength + simLength * (optRuns[n] - 1))
        vals <- simData[lower:upper,]

        iCost[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Cost"][frontierList[[n]]],              target = 0.729)
        iTest[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Testing"][frontierList[[n]]],           target = 0.729)
        iLink[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Linkage"][frontierList[[n]]],           target = 0.729)
        iPreR[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Pre-ART Retention"][frontierList[[n]]], target = 0.729)
        iInit[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Initiation"][frontierList[[n]]],        target = 0.729)
        iAdhr[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Adherence"][frontierList[[n]]],         target = 0.729)
        iRetn[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"ART Retention"][frontierList[[n]]],     target = 0.729)
        iTCst[n] <- Interpolate(vs = vals[,"VS"][frontierList[[n]]], indicator = vals[,"Total Cost"][frontierList[[n]]],        target = 0.729)
    }
    careOutput <- data.frame(iCost, iTest, iLink, iPreR, iInit, iAdhr, iRetn, iTCst)
    careOutput
}

FindFrontier <- function(x, y) {
    # Create data.frame of x and y
    df <- data.frame(x = x, y = y)
    # Zero the index vector
    frontierIndex <- c()
    # Finding the cost frontier
    rankCost <- order(df$y)
    frontierIndex[1] <- rankCost[1]
    for (i in 1:dim(df)[1]) {
        # Remove rows on the frontier
        noFront <- df[-(frontierIndex),]
        # Only consider values with larger impact
        remain <- noFront[noFront$x > max(df[frontierIndex,1]),]
        # break if remain is empty
        if (dim(remain)[1] == 0) break;
        # calculate gradient of last point on frontier to all remaining
        grad <- (df[frontierIndex[i],2] - remain[,2]) / (df[frontierIndex[i],1] - remain[,1])
        # calculate gradient to all points, everywhere
        ref <- (df[frontierIndex[i],2] - df[,2]) / (df[frontierIndex[i],1] - df[,1])
        # find the smallest, non-zero gradient from those remaining and pin-point
        # it's index in the whole data.frame
        frontierIndex[i+1] <- which(ref == min(grad[grad >= 0], na.rm = TRUE))
    }
    frontierIndex
}

FindFrontierPlot <- function(x, y) {
    # Create data.frame of x and y
    df <- data.frame(x = x, y = y)
    # Zero the index vector
    frontierIndex <- c()
    # Finding the cost frontier
    rankCost <- order(df$y)
    frontierIndex[1] <- rankCost[1]
    for (i in 1:dim(df)[1]) {
        # Remove rows on the frontier
        noFront <- df[-(frontierIndex),]
        # Only consider values with larger impact
        remain <- noFront[noFront$x > max(df[frontierIndex,1]),]
        # break if remain is empty
        if (dim(remain)[1] == 0) break;
        # calculate gradient of last point on frontier to all remaining
        grad <- (df[frontierIndex[i],2] - remain[,2]) / (df[frontierIndex[i],1] - remain[,1])
        # calculate gradient to all points, everywhere
        ref <- (df[frontierIndex[i],2] - df[,2]) / (df[frontierIndex[i],1] - df[,1])
        # find the smallest, non-zero gradient from those remaining and pin-point
        # it's index in the whole data.frame
        frontierIndex[i+1] <- which(ref == min(grad[grad >= 0], na.rm = TRUE))
    }
    ggPlot <- ggplot(df, aes(x = x, y = y))
    ggPlot <- ggPlot + geom_point(alpha = 0.2)
    ggPlot <- ggPlot + geom_line(data = df[frontierIndex,], aes(x = x, y = y), col = "red", alpha = 0.5)
    ggPlot <- ggPlot + geom_point(data = df[frontierIndex,], aes(x = x, y = y), col = "red", alpha = 0.5)
    ggPlot <- ggPlot + theme_classic()
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + expand_limits(y = round(max(y), digits = -9))
    ggPlot <- ggPlot + scale_y_continuous(labels = scales::scientific, breaks = scales::pretty_breaks())
    ggPlot <- ggPlot + scale_x_continuous(labels = scales::percent, breaks = scales::pretty_breaks())
    ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 8))
    ggPlot <- ggPlot + theme(axis.title = element_text(size = 9))
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + theme(legend.title = element_text(size = 9))
    ggPlot <- ggPlot + theme(legend.text = element_text(size = 8))
    ggPlot <- ggPlot + xlab("Viral Suppression")
    ggPlot <- ggPlot + ylab("Additional Cost of Care")
    ggPlot <- ggPlot + theme(text = element_text(family = "Avenir Next"))
    ggPlot
}
