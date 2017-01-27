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
obj <- clustr::login(cluster = "fi--dideclusthn")

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

# Run Calibration
# start.time <- proc.time()
# RunNSCalibration(
#     country = MasterName,
#     data = MasterData,
#     maxIterations = 1e4,
#     maxError = MaxError,
#     limit = MinNumber,
#     parRange = parRange,
#     targetIterations = 1e5)
# finish.time <- proc.time() - start.time
# finish.time[3] / 60
# 95 min runtime

graphics.off(); quartz.options(w = 8, h = 6)
BuildCD4CalibData_Thesis(year = 1, modelOut = modelOut)
# quartz.save(file = "../../formal/zimbabwe/PHIA/fig/cal/CD4-2010.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 6)
BuildCD4CalibData_Thesis(year = 6, modelOut = modelOut)
# quartz.save(file = "../../formal/zimbabwe/PHIA/fig/cal/CD4-2015.pdf", type = "pdf")


graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = CalibOut)
# quartz.save(file = "../../formal/zimbabwe/PHIA/fig/cal/PHIA.pdf", type = "pdf")

