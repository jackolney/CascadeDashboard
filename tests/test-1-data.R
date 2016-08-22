context("Data")

setwd("..")

source("server/calibration/assumptions.R",         local = FALSE)
source("server/calibration/calibration-data.R",    local = FALSE)
source("server/calibration/calibration.R",         local = FALSE)
source("server/calibration/error.R",               local = FALSE)
source("server/calibration/initial.R",             local = FALSE)
source("server/calibration/marrakech-data.R",      local = FALSE)
source("server/calibration/master.R",              local = FALSE)
source("server/calibration/misc-functions.R",      local = FALSE)
source("server/calibration/model.R",               local = FALSE)
source("server/calibration/plot-functions.R",      local = FALSE)
source("server/country/misc-functions.R",          local = FALSE)
source("server/misc-functions.R",                  local = FALSE)
source("server/non-shiny/non-shiny-calibration.R", local = FALSE)
source("ui/global-lists.R",                        local = FALSE)

test_that("Kenya", {
    testData <- GetMasterDataSet("Kenya")
    testthat::expect_true(is.list(testData), label = "Kenya dataset", info = "failed to load")
})

test_that("Tanzania", {
    testData <- GetMasterDataSet("Tanzania")
    testthat::expect_true(is.list(testData), label = "Tanzania dataset", info = "failed to load")
})

test_that("Zimbabwe", {
    testData <- GetMasterDataSet("Zimbabwe")
    testthat::expect_true(is.list(testData), label = "Zimbabwe dataset", info = "failed to load")
})

test_that("Clashing data", {
    testthat::expect_silent(for(i in 1:length(CountryList)) checkForClashes(GetMasterDataSet(CountryList[i])))
})
