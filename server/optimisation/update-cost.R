# Update cost
reactiveCost <- reactiveValues(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

# This is used by the function 'AdjustHIVTetsCost'
SafeReactiveCost <- reactiveValues(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

# RENDER UI
output$UI_DxUnitCost <- renderUI({
    sliderInput(inputId = 'userDxUnitCost',        label = 'Unit cost of diagnosing a patient (USD):',                min = 0, max = 100, value = 10,  step = 1)
})

output$UI_LinkageUnitCost <- renderUI({
    sliderInput(inputId = 'userLinkageUnitCost',   label = 'Unit cost of linking a patient to care (USD):',           min = 0, max = 100, value = 40,  step = 1)
})

output$UI_AnnualCareCost <- renderUI({
    sliderInput(inputId = 'userAnnualCareUnit',    label = 'Annual cost of keeping a patient in pre-ART care (USD):', min = 0, max = 100, value = 40,  step = 1)
})

output$UI_AnnualARTCost <- renderUI({
    sliderInput(inputId = 'userAnnualARTUnitCost', label = 'Annual cost of ART (USD):',                               min = 0, max = 500, value = 367, step = 1)
})

# Observe Event for user changes to cost.
observeEvent(input$userDxUnitCost, {
    # print(paste("Dx Cost = ", input$userDxUnitCost))
    reactiveCost$test <<- input$userDxUnitCost
    SafeReactiveCost$test <<- input$userDxUnitCost
})

observeEvent(input$userLinkageUnitCost, {
    # print(paste("Linkage Cost = ", input$userLinkageUnitCost))
    reactiveCost$link <<- input$userLinkageUnitCost
    SafeReactiveCost$link <<- input$userLinkageUnitCost
})

observeEvent(input$userAnnualCareUnit, {
    # print(paste("Pre-ART Care Cost = ", input$userAnnualCareUnit))
    reactiveCost$care <<- input$userAnnualCareUnit
    SafeReactiveCost$care <<- input$userAnnualCareUnit
})

observeEvent(input$userAnnualARTUnitCost, {
    # print(paste("ART Cost = ", input$userAnnualARTUnitCost))
    reactiveCost$art <<- input$userAnnualARTUnitCost
    SafeReactiveCost$art <<- input$userAnnualARTUnitCost
})

# Pass to parameter() functions in optimisation section
# reactiveCost elements passed to functions in server/optimisation/parameters.R

# Include elements of MasterData$pop else DEFAULT of 50USD?
# Write a function to call at the beginning of optimisation?

# Adjust cost switch
# print(paste("Check input$adjustCost =", input$adjustCost))
reactiveAdjustCost <- reactiveValues(switch = TRUE)

observeEvent(input$adjustCost, {
    # print(paste("input$adjustCost =", input$adjustCost))
    if (input$adjustCost == FALSE) {
        reactiveAdjustCost$switch <<- FALSE
    } else {
        reactiveAdjustCost$switch <<- TRUE
    }
    # print(paste("reactiveAdjustCost$switch =", reactiveAdjustCost$switch))
})

output$UI_AdjustCostValue <- renderUI({
    if (!is.na(MasterData$pop$value)) {
        numericInput(inputId = "adjustCostValue", label = "Population size in 2015", value = MasterData$pop$value, min = 0, max = NA, step = 1)
    } else {
        numericInput(inputId = "adjustCostValue", label = "Population size in 2015", value = NA, min = 0, max = NA, step = 1)
    }
})

observeEvent(input$adjustCostValue, {
    # If a value is entered / edited.
    # Then update MasterData$pop$value
    if (!is.na(input$adjustCostValue)) {
        MasterData$pop$value <<- input$adjustCostValue
        print(paste("MasterData$pop$value =", MasterData$pop$value))
    }
})

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
