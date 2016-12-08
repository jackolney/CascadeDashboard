context("Optimisation")

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

test_that("Full optimisation test", {
    # Intervention Switches
    intSwitch <<- data.frame(
        testing =      TRUE,
        linkage =      TRUE,
        preRetention = TRUE,
        initiation =   TRUE,
        adherence =    TRUE,
        retention =    TRUE
        )
    # Intervention Values
    OptInput <<- c()
    OptInput$intValue_rho   <<- parRange["rho", "max"]
    OptInput$intValue_q     <<- parRange["q", "max"]
    OptInput$intValue_kappa <<- parRange["kappa", "min"]
    OptInput$intValue_gamma <<- parRange["gamma", "max"]
    OptInput$intValue_sigma <<- 0.1
    OptInput$intValue_omega <<- parRange["rho", "min"]
    # Optimisation
    theOut <- RunNSOptimisation(propRuns = 0.1)
    testthat::expect_false(any(colSums(theOut) == 0), info = "ZERO values in result table", label = "Optimisation test")
})
