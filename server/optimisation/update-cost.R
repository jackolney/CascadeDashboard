# Update cost
reactiveCost <- reactiveValues(
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
    print(paste("Dx Cost = ", input$userDxUnitCost))
    reactiveCost$test <<- input$userDxUnitCost
})

observeEvent(input$userLinkageUnitCost, {
    print(paste("Linkage Cost = ", input$userLinkageUnitCost))
    reactiveCost$link <<- input$userLinkageUnitCost
})

observeEvent(input$userAnnualCareUnit, {
    print(paste("Pre-ART Care Cost = ", input$userAnnualCareUnit))
    reactiveCost$care <<- input$userAnnualCareUnit
})

observeEvent(input$userAnnualARTUnitCost, {
    print(paste("ART Cost = ", input$userAnnualARTUnitCost))
    reactiveCost$art <<- input$userAnnualARTUnitCost
})

# Pass to parameter() functions in optimisation section


# TEST TEST TEST

# Include elements of MasterData$pop else DEFAULT of 50USD?
# Write a function to call at the beginning of optimisation?
