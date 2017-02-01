####################################################################################################
# PEPFAR PHIA FILE
# Zimbabwe Cascade Tool Formal Analysis Script
# Model version include CD4 dependency on theta for only those with CD4 <200
# This version will attempt to use 'clustr' to interface with the DIDE cluster
####################################################################################################
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard/inst/app")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")

# GLOBAL
MasterName <- "Zimbabwe"
MasterData <- GetMasterDataSet(MasterName)

# ---- #
set.seed(100)
# ---- #

# These will be adjusted in due course:
MaxError <- 1
MinNumber <- 100

# After first simulation, run this function (default = 5%)
# MaxError <- find_error_bound(runError, prop = 0.05)

# Define Parameter Range
parRange <- DefineParmRange(
    rho     = c(0, 0.005),
    q       = c(0.9, 1),
    epsilon = c(100, 100),
    kappa   = c(0, 3),
    gamma   = c(2, 4),
    theta   = c(0, 0.5),
    p       = c(0.7, 1),
    omega   = c(0, 0.01)
)

# Run Calibration
obj <- clustr::login()
# to run backup cluster
# obj <- clustr::login(cluster = "fi--dideclusthn")

t <- obj$enqueue(
    RunClusterCalibration(
        country = MasterName,
        data = MasterData,
        maxIterations = 1e4,
        maxError = MaxError,
        limit = MinNumber,
        parRange = parRange,
        targetIterations = 1e5)
)

t$status()
t$log()



t$id

obj$cluster_load()

out <- t$result()

# strip calibration results from object
for(i in 1:length(out)) assign(names(out)[i], out[[i]])

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = CalibOut)

# Cascade in 2015
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)

# Error Histogram
graphics.off(); quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(
    runError = runError,
    maxError = MaxError)

# Calibration Detail
graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)

# Parameter Histograms
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()

####################################################################################################
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
OptInput$intValue_rho   <- 0.1
OptInput$intValue_q     <- 1
OptInput$intValue_kappa <- 0
OptInput$intValue_gamma <- 10
OptInput$intValue_sigma <- 0.1
OptInput$intValue_omega <- 0

reactiveCost <- data.frame(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

# This is used by the function 'AdjustHIVTestCost'
SafeReactiveCost <- data.frame(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

custom <- data.frame(target = 0.9^3)

AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

reactiveAdjustCost <- data.frame(switch = TRUE)

AdjustHIVTestCost(reactiveAdjustCost, reactiveCost, SafeReactiveCost)

job <- obj$enqueue(
    RunClusterOptimisation(
        AdvCalib = AdvCalib,
        CalibOut = CalibOut,
        CalibParamOut = CalibParamOut,
        intLength = 2,
        intSwitch = intSwitch,
        OptInput = OptInput,
        propRuns = 0.1,
        reactiveAdjustCost = reactiveAdjustCost,
        reactiveCost = reactiveCost,
        runError = runError,
        SafeReactiveCost = SafeReactiveCost,
        selectedRuns = selectedRuns
    )
)
job$status()
job$log()
