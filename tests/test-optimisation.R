context("Optimisation")

setwd("..")

source("server/calibration/master.R",              local = FALSE)
source("server/calibration/initial.R",             local = FALSE)
source("server/calibration/model.R",               local = FALSE)
source("server/calibration/error.R",               local = FALSE)
source("server/calibration/calibration.R",         local = FALSE)
source("server/calibration/assumptions.R",         local = FALSE)
source("server/calibration/calibration-data.R",    local = FALSE)
source("server/calibration/marrakech-data.R",      local = FALSE)
source("server/calibration/misc-functions.R",      local = FALSE)
source("server/misc-functions.R",                  local = FALSE)
source("server/calibration/plot-functions.R",      local = FALSE)
source("server/non-shiny/non-shiny-calibration.R", local = FALSE)
source("server/country/misc-functions.R",          local = FALSE)

require("cascade")


# Write an optimisation test sequence
# But, begin with a test comparison of optimisation parameters against baseline
# parameters (as below). What we are checking is whether, for the best 10% of
# simulations, the calls to GetOptRunpar() and GetBaselinePar() are equal
# They should be for all intervention combinations (i = 1), except preRetention
# and retention. so need to walk to the next i to get those bad boys.

# intSwitch <- data.frame(
#     testing =      TRUE,
#     linkage =      TRUE,
#     preRetention = FALSE,
#     initiation =   TRUE,
#     adherence =    TRUE,
#     retention =    FALSE
#     )


# jLength <- 10
# i <- 1
# # i should always be one because thats the baseline

# for (j in 1:jLength) {
#     message(paste("Sim", j))
#     par <- GetParaMatrixRunLimits(cParamOut = CalibParamOut, runNumber = orderedRuns[j], length = 2)
#     pOpt <- GetOptRunPar(
#         masterCD4 = MasterData$cd4_2015,
#         data = MasterData,
#         iterationParam = par[i,],
#         calibParamOut = CalibParamOut,
#         runNumber = orderedRuns[j])

#     pBase <- GetBaselinePar(
#         masterCD4 = MasterData$cd4_2015,
#         data = MasterData,
#         calibParamOut = CalibParamOut,
#         runNumber = orderedRuns[j])

#     testthat::expect_equal(pOpt, pBase)
# }


# Next test, turning beta off (ZERO) will stop any new infections from happening.
# and some run comparisons between the model
