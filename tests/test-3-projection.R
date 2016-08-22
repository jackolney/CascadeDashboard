context("Projection")

setwd("..")

source("server/calibration/assumptions.R",          local = FALSE)
source("server/calibration/calibration-data.R",     local = FALSE)
source("server/calibration/calibration.R",          local = FALSE)
source("server/calibration/error.R",                local = FALSE)
source("server/calibration/initial.R",              local = FALSE)
source("server/calibration/marrakech-data.R",       local = FALSE)
source("server/calibration/master.R",               local = FALSE)
source("server/calibration/misc-functions.R",       local = FALSE)
source("server/calibration/model.R",                local = FALSE)
source("server/calibration/plot-functions.R",       local = FALSE)
source("server/country/misc-functions.R",           local = FALSE)
source("server/misc-functions.R",                   local = FALSE)
source("server/model/baseline-model.R",             local = FALSE)
source("server/model/best-fit-model.R",             local = FALSE)
source("server/model/beta.R",                       local = FALSE)
source("server/model/initial.R",                    local = FALSE)
source("server/model/parameters.R",                 local = FALSE)
source("server/model/sim-abs.R",                    local = FALSE)
source("server/model/sim-prop.R",                   local = FALSE)
source("server/optimisation/frontier.R",            local = FALSE)
source("server/optimisation/input-functions.R",     local = FALSE)
source("server/optimisation/output-functions.R",    local = FALSE)
source("server/optimisation/parameters.R",          local = FALSE)
source("server/optimisation/plot-functions.R",      local = FALSE)
source("tests/src/calibration.R",                   local = FALSE)
source("tests/src/optimisation.R",                  local = FALSE)

require("cascade")

# Next test, turning beta off (ZERO) will stop any new infections from happening.
# and some run comparisons between the model

# Test that no errors occur
test_that("Simple cascade projection", {
    # Setup
    MasterData <<- GetMasterDataSet("Kenya")
    # Calibration
    p <- GetBaselinePar(
        masterCD4 = MasterData$cd4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        runNumber = 1)

    y <- GetInitial(
        p = p,
        iterationResult = CalibOut[CalibOut$source == "model" & CalibOut$year == 2015,][1:7,],
        masterCD4 = MasterData$cd4_2015)

    p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[1,])

    result <- RunSim_Abs(y = y, p = p)
    result

    testthat::expect_true(dim(result)[1] == 251, info = "Projection not returng 5 years of results")
})

test_that("Zero transmission", {
    # Setup
    MasterData <<- GetMasterDataSet("Kenya")
    # Calibration
    p <- GetBaselinePar(
        masterCD4 = MasterData$cd4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        runNumber = 1)

    y <- GetInitial(
        p = p,
        iterationResult = CalibOut[CalibOut$source == "model" & CalibOut$year == 2015,][1:7,],
        masterCD4 = MasterData$cd4_2015)

    p[["beta"]] <- 0

    result <- RunSim_Abs(y = y, p = p)
    result

    any(result$NewInf != 0)
    testthat::expect_false(any(result$NewInf != 0), info = "With transmission probability at zero (beta), new infections are still occurring", label = "New infections")
})

test_that("Absolute vs. proportional models", {
    MasterData <<- GetMasterDataSet("Kenya")
    # Calibration
    p <- GetBaselinePar(
        masterCD4 = MasterData$cd4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        runNumber = 1)

    y <- GetInitial(
        p = p,
        iterationResult = CalibOut[CalibOut$source == "model" & CalibOut$year == 2015,][1:7,],
        masterCD4 = MasterData$cd4_2015)

    p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[1,])

    resOne <- RunSim_Abs(y = y, p = p)
    resTwo <- RunSim_Prop(y = y, p = p)

    testthat::expect_equal(resOne$N, resTwo$N, label = )

    # ignore differences
    resOne_trunc <- subset(resOne, select = -c(ART, UnDx, Dx, Care, PreLtfu, Tx, Vs, Ltfu))
    resTwo_trunc <- subset(resTwo, select = -c(ART, UnDx, Dx, Care, PreLtfu, Tx, Vs, Ltfu))

    testthat::expect_equal(resOne_trunc, resTwo_trunc, info = "Truncated results not equal", label = "Truncated result test")
})
