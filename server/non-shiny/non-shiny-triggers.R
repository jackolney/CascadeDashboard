# non-shiny optimisation triggers and setup
# input stuff etc.
graphics.off()
quartz.options(w = 10, h = 8)

source("server/model/baseline-model.R",                local = FALSE)
source("server/model/best-fit-model.R",                local = FALSE)
source("server/model/beta.R",                          local = FALSE)
source("server/model/initial.R",                       local = FALSE)
source("server/model/parameters.R",                    local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",    local = FALSE)
source("server/optimisation/input-functions.R",        local = FALSE)
source("server/optimisation/output-functions.R",       local = FALSE)
source("server/optimisation/parameters.R",             local = FALSE)
source("server/optimisation/plot-functions.R",         local = FALSE)
source("server/optimisation/sim.R",                    local = FALSE)
source("server/projection/CD4-distribution.R",         local = FALSE)
source("server/optimisation/frontier.R",               local = FALSE)

# reactive input setup
MasterCD4_2015 <- GetCD4Distribution2015("Kenya")
MasterData <- GetCountryData("Kenya")

intSwitch <- data.frame(
    testing =      TRUE,
    linkage =      TRUE,
    preRetention = TRUE,
    initiation =   TRUE,
    adherence =    TRUE,
    retention =    TRUE
    )

OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.1
OptInput$intValue_omega <- parRange["rho", "min"]

# ------------ #
# OPTIMISATION #
# ------------ #

BuildCalibrationBestFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = 1000, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)

theOut <- RunNSOptimisation(propRuns = 0.1)
head(theOut)
dim(theOut)

mean(theOut$Testing)

a = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Rho), alpha = 0.2) + theme_minimal()
b = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Q), alpha = 0.2) + theme_minimal()
c = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Kappa), alpha = 0.2) + theme_minimal()
d = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Gamma), alpha = 0.2) + theme_minimal()
e = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Sigma), alpha = 0.2) + theme_minimal()

gridExtra::grid.arrange(a, b, c, d, e, ncol = 2, nrow = 3)

# See one big data.frame may not be the best solution.

# We want to calculate the frontier, or approx the function, for EACH of the (j) parameter sets run.

# What if we just take one simulation


##### HERE WE GO #####
simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]

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

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength)
optRuns

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

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

PlotInterpolation <- function(vs, indicator, target) {
    interpolation <- approx(x = vs, y = indicator)
    intIndex <- which.min(abs(target - interpolation$x))
    interpolation$y[intIndex]
    plot(x = vs, y = indicator)
    points(interpolation$x, interpolation$y, col = "red", pch = "*")
    abline(v = target, h = interpolation$y[intIndex])
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
    }
    careOutput <- data.frame(iCost, iTest, iLink, iPreR, iInit, iAdhr, iRetn)
    careOutput
}

test <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

mean(test[,"iCost"])
mean(test[,"iTest"])
mean(test[,"iLink"])
mean(test[,"iPreR"])
mean(test[,"iInit"])
mean(test[,"iAdhr"])
mean(test[,"iRetn"])
