# non-shiny optimisation triggers and setup
# input stuff etc.
graphics.off()
quartz.options(w = 10, h = 8)

source("server/misc-functions.R",                      local = FALSE)
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
OptInput$intValue_sigma <- 0.5
OptInput$intValue_omega <- parRange["omega", "min"]

reactiveCost <- c(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

# This is used by the function 'AdjustHIVTetsCost'
SafeReactiveCost <- c(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

custom <- data.frame(target = 0.9^3)

AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

# ------------ #
# OPTIMISATION #
# ------------ #

BuildCalibrationBestFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = 1000, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)
BuildCalibrationRandomFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = 1000, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)

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

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength, target = custom$target)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

test <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = custom$target)

NonZeroVectorCheck(colMeans(test))
Quantile_95(test[,"iCost"])
Quantile_95(test[,"iTest"])
Quantile_95(test[,"iLink"])
Quantile_95(test[,"iPreR"])
Quantile_95(test[,"iInit"])
Quantile_95(test[,"iAdhr"])
Quantile_95(test[,"iRetn"])
Quantile_95(test[,"iTCst"])


#############################
# Take Two at Interpolation #
#############################

simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength, target = custom$target)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = custom$target)

NonZeroVectorCheck(test)
Quantile_95(test[,"iCost"])
Quantile_95(test[,"iTest"])
Quantile_95(test[,"iLink"])
Quantile_95(test[,"iPreR"])
Quantile_95(test[,"iInit"])
Quantile_95(test[,"iAdhr"])
Quantile_95(test[,"iRetn"])
Quantile_95(test[,"iTCst"])
