# non-shiny optimisation triggers and setup
# input stuff etc.
graphics.off()
quartz.options(w = 10, h = 8)

source("server/model/baseline-model.R",                local = FALSE)
source("server/model/best-fit-model.R",                local = FALSE)
source("server/model/beta.R",                          local = FALSE)
source("server/model/initial.R",                       local = FALSE)
source("server/model/parameters.R",                    local = FALSE)
source("server/model/sim-abs.R",                       local = FALSE)
source("server/model/sim-prop.R",                      local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",    local = FALSE)
source("server/optimisation/frontier.R",               local = FALSE)
source("server/optimisation/input-functions.R",        local = FALSE)
source("server/optimisation/output-functions.R",       local = FALSE)
source("server/optimisation/parameters.R",             local = FALSE)
source("server/optimisation/plot-functions.R",         local = FALSE)

# reactive input setup
MasterData <- GetMasterDataSet("Kenya")

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

intLength = 2

theOut <- RunNSOptimisation(propRuns = 0.1, intLength = intLength)

a = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Rho), alpha = 0.2) + theme_minimal()
b = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Q), alpha = 0.2) + theme_minimal()
c = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Kappa), alpha = 0.2) + theme_minimal()
d = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Gamma), alpha = 0.2) + theme_minimal()
e = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Sigma), alpha = 0.2) + theme_minimal()

gridExtra::grid.arrange(a, b, c, d, e, ncol = 2, nrow = 3)


# See one big data.frame may not be the best solution.

# We want to calculate the frontier, or approx the function, for EACH of the (j) parameter sets run.

# What if we just take one simulation


##### Frontier Finding #####

simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

FindFrontier

x = vals$VS
y = vals$Cost

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


test <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

mean(test[,"iCost"])
mean(test[,"iTest"])
mean(test[,"iLink"])
mean(test[,"iPreR"])
mean(test[,"iInit"])
mean(test[,"iAdhr"])
mean(test[,"iRetn"])
mean(test[,"iTCst"])



names(optResults)

length(optResults[,"Total Cost"])

length(BaselineCost)

scales::dollar(mean(BaselineCost) / 5)
scales::dollar(mean(optResults[,"Total Cost"]) / 5)

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

hi <- theOut
names(hi)[18] <- "TotalCost"

a = ggplot(hi, aes(x = VS, y = Cost)) + geom_point(aes(col = Rho), alpha = 0.2) + theme_minimal()
b = ggplot(hi, aes(x = VS, y = TotalCost)) + geom_point(aes(col = Rho), alpha = 0.2) + theme_minimal()

gridExtra::grid.arrange(a, b, ncol = 2, nrow = 1)
