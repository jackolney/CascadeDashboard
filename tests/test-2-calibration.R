context("Calibration")

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
source("server/country/misc-functions.R",          local = FALSE)
source("tests/src/calibration.R",                  local = FALSE)

require("cascade", quietly = TRUE)

test_that("Get parameters", {
    data <- GetMasterDataSet("Kenya")
    p <- parameters(
        prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
    )
    testthat::expect_true(round(sum(p), digits = 4) == 525.1713, label = "Parameters", info = "not valid")
})

test_that("LHS parameter range", {
    parRange <- DefineParmRange()
    expect_silent(FME::Latinhyper(parRange, num = 1e4))
})

test_that("LHS initial range", {
    data <- GetMasterDataSet("Kenya")
    initRange <- DefineInitRange(data = data, min = 0.75, max = 1.25)
    lhsInitial <- FME::Latinhyper(initRange, num = 1e4 * 3)
    lhsInitial_Sense <- FindSense(samples = lhsInitial)
    testthat::expect_true(dim(lhsInitial_Sense)[1] > 100, label = "LHS initial range", info = "Model generated less than 100 sensical initial compartment values")
})

test_that("LHS incidence", {
    data <- GetMasterDataSet("Kenya")
    incRange <- DefineIncidenceRange(incidenceData = data$incidence)
    testthat::expect_silent(FME::Latinhyper(incRange, num = 1e4))
})

test_that("Full calibration test", {
    data <- GetMasterDataSet("Kenya")
    limit = 100
    maxError = 3
    RunNSCalibration(country = "Kenya", data = data, maxIterations = 1e4, maxError = 3, limit = 100)
    testthat::expect_true(sum(runError <= maxError) == limit, info = "Overstepping limit", label = "Calibration")
})
