# Script to source all non-shiny components of the model

require(RColorBrewer)
devtools::load_all(pkg = "~/git/cascade")

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
source("server/misc-functions.R",                   local = FALSE)
source("server/model/baseline-model.R",             local = FALSE)
source("server/model/best-fit-model.R",             local = FALSE)
source("server/model/beta.R",                       local = FALSE)
source("server/model/initial.R",                    local = FALSE)
source("server/model/parameters.R",                 local = FALSE)
source("server/model/sim-abs.R",                    local = FALSE)
source("server/model/sim-prop.R",                   local = FALSE)
source("server/non-shiny/non-shiny-calibration.R",  local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R", local = FALSE)
source("server/non-shiny/thesis/thesis-figures.R",  local = FALSE)
source("server/optimisation/frontier.R",            local = FALSE)
source("server/optimisation/input-functions.R",     local = FALSE)
source("server/optimisation/output-functions.R",    local = FALSE)
source("server/optimisation/parameters.R",          local = FALSE)
source("server/optimisation/plot-functions.R",      local = FALSE)

AdjustHIVTestCost <- function() {
    if (reactiveAdjustCost$switch == TRUE) {
        message("AdjustCost == TRUE")
        if (exists("CalibOut")) {
            if (exists("MasterData")) {
                if (!is.na(MasterData$pop$value)) {
                    # pop value is not NA

                    # From calibration (CalibOut), calculate mean # 'PLHIV' in 2015
                    meanPLHIV <- mean(CalibOut[CalibOut$indicator == "PLHIV" & CalibOut$year == 2015 & CalibOut$source == "model", "value"])
                    # From calibration (CalibOut), calculate mean # 'PLHIV in Care' in 2015
                    meanCARE <- mean(CalibOut[CalibOut$indicator == "PLHIV in Care" & CalibOut$year == 2015 & CalibOut$source == "model", "value"])
                    # Calculate those persons Not In Care
                    NotInCare <- meanPLHIV - meanCARE
                    # Calculate the HIV-negative population size
                    Negative <- MasterData$pop$value - meanPLHIV

                    # Jeff's assumption
                    # HIV-negative persons are 0.75 times as likely to test as HIV-positive in general population
                    # Lancet GH Cost-Effectiveness Paper (suppl info page 10)
                    jeff <- 0.65

                    # Using the assumption that persons are tested randomly
                    CostFactor <- ((jeff * Negative) + NotInCare) / NotInCare
                    # print(paste("CostFactor =", CostFactor))
                    # Another way of thinking about this is as the:
                    # probability of testing a positive individual
                    # given the size of the undiagnosed (not in care) population
                    # 1/(NotInCare / ((jeff * Negative) + NotInCare))

                    # CAREFUL
                    # print(paste("OLD reactiveCost$test =", reactiveCost$test))
                    reactiveCost$test <<- CostFactor * SafeReactiveCost$test
                    # print(paste("NEW reactiveCost$test =", reactiveCost$test))

                } else {
                    # pop value is NA
                    # DEFAULT of FIVE
                    CostFactor <- 5
                    # print("DEFAULT")
                    # print(paste("OLD reactiveCost$test =", reactiveCost$test))
                    reactiveCost$test <<- CostFactor * SafeReactiveCost$test
                    # print(paste("NEW reactiveCost$test =", reactiveCost$test))

                }

            } else {
                warning("MasterData does not exist")
            }
        } else {
            warning("CalibOut is does not exist")
        }
    } else {
        message("AdjustCost == FALSE")
    }
}

message("Good to go...")
