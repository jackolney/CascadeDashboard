# Zimbabwe Cascade Tool Formal Analysis Script
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("formal/initial.R")

# GLOBAL
MasterName <- "Zimbabwe"
MasterData <- GetMasterDataSet(MasterName)

# ---- #
set.seed(100)
# ---- #

MaxError <- 0.06
MinNumber <- 1000

# Define Parameter Range
# function can now be edited
# e.g. DefineParmRange(p = c(0, 1))
# parRange <- DefineParmRange()

# parRange <- DefineParmRange(p = c(0.86, 1), omega = c(0, 0.01))
parRange <- DefineParmRange(p = c(0.7, 1), omega = c(0, 0.01))

# Run Calibration
RunNSCalibration(
    country = MasterName,
    data = MasterData,
    maxIterations = 1e4,
    maxError = MaxError,
    limit = MinNumber,
    parRange = parRange,
    targetIterations = 1e5)

# Cascade in 2015
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = "formal/zimbabwe/fig/cal/cascade-2015.pdf", type = "pdf")

# Error Histogram
graphics.off(); quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(
    runError = runError,
    maxError = 0.06)
quartz.save(file = "formal/zimbabwe/fig/cal/calib-hist.pdf", type = "pdf")

# Calibration Detail
graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = "formal/zimbabwe/fig/cal/calib-detail.pdf", type = "pdf")

# Parameter Histograms
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()
quartz.save(file = "formal/zimbabwe/fig/cal/par-hist.pdf", type = "pdf")

# Parameter means
round(colMeans(CalibParamOut), 4)

# Parameter values
a <- paste0(round(Quantile_95(CalibParamOut[["rho"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["rho"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["rho"]])[["upper"]], 4), "]")
b <- paste0(round(Quantile_95(CalibParamOut[["q"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["q"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["q"]])[["upper"]], 4), "]")
c <- paste0(round(Quantile_95(CalibParamOut[["epsilon"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["epsilon"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["epsilon"]])[["upper"]], 4), "]")
d <- paste0(round(Quantile_95(CalibParamOut[["kappa"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["kappa"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["kappa"]])[["upper"]], 4), "]")
e <- paste0(round(Quantile_95(CalibParamOut[["gamma"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["gamma"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["gamma"]])[["upper"]], 4), "]")
f <- paste0(round(Quantile_95(CalibParamOut[["theta"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["theta"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["theta"]])[["upper"]], 4), "]")
g <- paste0(round(Quantile_95(CalibParamOut[["p"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["p"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["p"]])[["upper"]], 4), "]")
h <- paste0(round(Quantile_95(CalibParamOut[["omega"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["omega"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["omega"]])[["upper"]], 4), "]")

list(a,b,c,d,e,f,g,h)

Quantile_95(CalibParamOut[["rho"]])
Quantile_95(CalibParamOut[["q"]])
Quantile_95(CalibParamOut[["epsilon"]])
Quantile_95(CalibParamOut[["kappa"]])
Quantile_95(CalibParamOut[["gamma"]])
Quantile_95(CalibParamOut[["theta"]])
Quantile_95(CalibParamOut[["p"]])
Quantile_95(CalibParamOut[["omega"]])

# Mean of over-riding parameter bounds from non-shiny interface

################################################################################
# Projection

AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

# CareCascade Plot
graphics.off(); quartz.options(w = 10, h = 4)
GenCascadePlot_Thesis()
quartz.save(file = "formal/zimbabwe/fig/pro/cascade-projection.pdf", type = "pdf")

# 90-90-90 Plot
graphics.off(); quartz.options(w = 9, h = 4)
Gen909090Plot_Thesis()
quartz.save(file = "formal/zimbabwe/fig/pro/90-90-90.pdf", type = "pdf")

# Powers Plot
graphics.off(); quartz.options(w = 15, h = 4)
GenPowersCascadePlot_Thesis()
quartz.save(file = "formal/zimbabwe/fig/pro/cascade-powers.pdf", type = "pdf")

# New Infections
graphics.off(); quartz.options(w = 6, h = 4)
GenNewInfPlot_Thesis()
quartz.save(file = "formal/zimbabwe/fig/pro/new-infections.pdf", type = "pdf")

# AIDS Deaths
graphics.off(); quartz.options(w = 6, h = 4)
GenAidsDeathsPlot_Thesis()
quartz.save(file = "formal/zimbabwe/fig/pro/AIDS-deaths.pdf", type = "pdf")

# Discrete Cascade
graphics.off(); quartz.options(w = 10, h = 4)
GenDiscreteCascade_Thesis()
quartz.save(file = "formal/zimbabwe/fig/pro/cascade-discrete.pdf", type = "pdf")

################################################################################
# Optimisation

intSwitch <- data.frame(
    testing      = TRUE,
    linkage      = TRUE,
    preRetention = TRUE,
    initiation   = TRUE,
    adherence    = TRUE,
    retention    = TRUE
)

OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.5
OptInput$intValue_omega <- parRange["omega", "min"]

reactiveCost <- data.frame(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

# This is used by the function 'AdjustHIVTetsCost'
SafeReactiveCost <- data.frame(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

custom <- data.frame(target = 0.9^3)

AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

reactiveAdjustCost <- data.frame(switch = TRUE)

intLength = 2

AdjustHIVTestCost()

theOut <- RunNSOptimisation(propRuns = 0.1, intLength = intLength)

# Frontier Plot (optResults comes from RunNSOptimisation
graphics.off(); quartz.options(w = 8, h = 4)
BuildFrontierPlot_Thesis(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3)
quartz.save(file = "formal/zimbabwe/fig/opt/frontier.pdf", type = "pdf")

# FIGURE GENERATION
graphics.off(); quartz.options(w = 8, h = 4)
BuildChangesPlot_Thesis(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3)
quartz.save(file = "formal/zimbabwe/fig/opt/changes.pdf", type = "pdf")

################################################################################
# TABULATE RESULTS #

simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength, target = custom$target)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

intRes <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = custom$target)
colMeans(intRes)

results <- intRes[,c("iTest","iLink","iPreR","iInit","iAdhr","iRetn")]
results$iPreR <- abs(results$iPreR)
results$iRetn <- abs(results$iRetn)
results[results$iTest < 0, "iTest"] <- 0
results[results$iLink < 0, "iLink"] <- 0
results[results$iInit < 0, "iInit"] <- 0
results[results$iAdhr < 0, "iAdhr"] <- 0
results$run <- 1:dim(results)[1]

resTable <- results

resTable[resTable$iTest < 0, "iTest"] <- 0
resTable[resTable$iLink < 0, "iLink"] <- 0
resTable[resTable$iInit < 0, "iInit"] <- 0
resTable[resTable$iAdhr < 0, "iAdhr"] <- 0

colMeans(resTable)

# DIVIDE BY FIVE!!!!!!! (because we are displaying the PER YEAR change)
round(Quantile_95(intRes[,"iCost"])["mean"] / 1e6, 2)
i1 <- paste0("iCost = ", scales::dollar(Quantile_95(intRes[,"iCost"])["mean"] / 5), " [", scales::dollar(Quantile_95(intRes[,"iCost"])["lower"] / 5), " to ", scales::dollar(Quantile_95(intRes[,"iCost"])["upper"] / 5), "]")
i2 <- paste0("iTest = ", scales::comma(round(Quantile_95(resTable[,"iTest"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iTest"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iTest"])["upper"] / 5, 0)), "]")
i3 <- paste0("iLink = ", scales::comma(round(Quantile_95(resTable[,"iLink"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iLink"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iLink"])["upper"] / 5, 0)), "]")
i4 <- paste0("iPreR = ", scales::comma(round(Quantile_95(resTable[,"iPreR"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iPreR"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iPreR"])["upper"] / 5, 0)), "]")
i5 <- paste0("iInit = ", scales::comma(round(Quantile_95(resTable[,"iInit"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iInit"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iInit"])["upper"] / 5, 0)), "]")
i6 <- paste0("iAdhr = ", scales::comma(round(Quantile_95(resTable[,"iAdhr"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iAdhr"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iAdhr"])["upper"] / 5, 0)), "]")
i7 <- paste0("iRetn = ", scales::comma(round(Quantile_95(resTable[,"iRetn"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iRetn"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iRetn"])["upper"] / 5, 0)), "]")

i1
i2
i3
i4
i5
i6
i7


Quantile_95(resTable[,"iTest"]) / 5
Quantile_95(resTable[,"iLink"]) / 5
Quantile_95(resTable[,"iPreR"]) / 5
Quantile_95(resTable[,"iInit"]) / 5
Quantile_95(resTable[,"iAdhr"]) / 5
Quantile_95(resTable[,"iRetn"]) / 5
Quantile_95(intRes[,"iTCst"]) / 5

################################################################################
# SAVE IMAGE #

# save.image("formal/zimbabwe/data.RData")
# save.image("formal/zimbabwe/big-data.RData")
