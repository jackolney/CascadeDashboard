context("Data")

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

test_that("Bolney", {
    expect_error(GetMasterDataSet("Bolney"), label = "Bolney is not a valid location")
})

# Keep going for ALL countries, ensure that a MasterData frame is created
