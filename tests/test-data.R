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
    expect_true(is.list(testData), label = "Kenya dataset failed to load")
})

test_that("Tanzania", {
    testData <- GetMasterDataSet("Tanzania")
    expect_true(is.list(testData), label = "Tanzania dataset failed to load")
})

test_that("Zimbabwe", {
    testData <- GetMasterDataSet("Zimbabwe")
    expect_true(is.list(testData), label = "Zimbabwe dataset failed to load")
})

test_that("Clashing data", {
    expect_silent(for(i in 1:length(CountryList)) checkForClashes(GetMasterDataSet(CountryList[i])))
})
