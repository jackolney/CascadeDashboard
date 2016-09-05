# Thesis Script.R

# Create all functions external to this script.
rm(list=ls())
setwd("~/git/CascadeDashboard")
# dir()
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# -------- #
# WORKFLOW #
# -------- #

# --------- #
# STAGE ONE #
# --------- #

# source all the relevant files
require(RColorBrewer)
source("server/calibration/master.R",  local = FALSE)
source("server/calibration/initial.R", local = FALSE)
source("server/calibration/model.R",   local = FALSE)
source("server/calibration/error.R",   local = FALSE)

# This contains simple function calls for the models in various permutations
source("server/calibration/calibration.R",           local = FALSE)
source("server/calibration/assumptions.R",           local = FALSE)
source("server/calibration/calibration-data.R",      local = FALSE)
source("server/calibration/marrakech-data.R",        local = FALSE)
source("server/calibration/misc-functions.R",        local = FALSE)
source("server/misc-functions.R",                    local = FALSE)
source("server/calibration/plot-functions.R",        local = FALSE)
source("server/non-shiny/non-shiny-calibration.R",   local = FALSE)
source("server/country/misc-functions.R",            local = FALSE)
source("server/non-shiny/thesis/thesis-figures.R",   local = FALSE)
source("server/misc-functions.R",                    local = FALSE)
source("server/model/baseline-model.R",              local = FALSE)
source("server/model/best-fit-model.R",              local = FALSE)
source("server/model/beta.R",                        local = FALSE)
source("server/model/initial.R",                     local = FALSE)
source("server/model/parameters.R",                  local = FALSE)
source("server/model/sim-abs.R",                     local = FALSE)
source("server/model/sim-prop.R",                    local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",  local = FALSE)
source("server/optimisation/frontier.R",             local = FALSE)
source("server/optimisation/input-functions.R",      local = FALSE)
source("server/optimisation/output-functions.R",     local = FALSE)
source("server/optimisation/parameters.R",           local = FALSE)
source("server/optimisation/plot-functions.R",       local = FALSE)


# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/cascade")

# Unit tests
devtools::test(pkg = "~/git/cascade")
testthat::test_dir("tests")

# SIMULATIONS #
set.seed(100)
ZimbabweData <- GetMasterDataSet("Zimbabwe")
RunNSCalibration(country = "Zimbabwe", data = ZimbabweData, maxIterations = 1e4, maxError = 0.4, limit = 1000)

graphics.off()
quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut, originalData = ZimbabweData, limit = 1000)

graphics.off()
quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(data = CalibOut, originalData = ZimbabweData, limit = 1000)

graphics.off()
quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(runError, maxError = 0.4)

graphics.off()
quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()

round(colMeans(CalibParamOut), 4)

DefineParmRange()

a <- paste0(round(Quantile_95(CalibParamOut[["rho"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["rho"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["rho"]])[["upper"]], 4), "]")
b <- paste0(round(Quantile_95(CalibParamOut[["q"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["q"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["q"]])[["upper"]], 4), "]")
c <- paste0(round(Quantile_95(CalibParamOut[["epsilon"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["epsilon"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["epsilon"]])[["upper"]], 4), "]")
d <- paste0(round(Quantile_95(CalibParamOut[["kappa"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["kappa"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["kappa"]])[["upper"]], 4), "]")
e <- paste0(round(Quantile_95(CalibParamOut[["gamma"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["gamma"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["gamma"]])[["upper"]], 4), "]")
f <- paste0(round(Quantile_95(CalibParamOut[["theta"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["theta"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["theta"]])[["upper"]], 4), "]")
g <- paste0(round(Quantile_95(CalibParamOut[["p"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["p"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["p"]])[["upper"]], 4), "]")
h <- paste0(round(Quantile_95(CalibParamOut[["omega"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["omega"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["omega"]])[["upper"]], 4), "]")

list(a,b,c,d,e,f,g,h)

Quantile_95(CalibParamOut[["q"]])
Quantile_95(CalibParamOut[["epsilon"]])
Quantile_95(CalibParamOut[["kappa"]])
Quantile_95(CalibParamOut[["gamma"]])
Quantile_95(CalibParamOut[["theta"]])
Quantile_95(CalibParamOut[["p"]])
Quantile_95(CalibParamOut[["omega"]])

####################################################################################################
####################################################################################################
## Projection Model

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

MasterData <- ZimbabweData

graphics.off()
quartz.options(w = 10, h = 4)
GenCascadePlot_Thesis()

graphics.off()
quartz.options(w = 9, h = 4)
Gen909090Plot_Thesis()

graphics.off()
quartz.options(w = 15, h = 4)
GenPowersCascadePlot_Thesis()

graphics.off()
quartz.options(w = 6, h = 4)
GenNewInfPlot_Thesis()

graphics.off()
quartz.options(w = 6, h = 4)
GenAidsDeathsPlot_Thesis()


