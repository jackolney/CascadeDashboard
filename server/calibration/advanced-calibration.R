## Advanced calibration

# Mortality over-ride
output$UI_NaturalMortalityOverRide <- renderUI({
    sliderInput(inputId = "uNaturalMortality_OverRide", label = "Natural Mortality Rate", min = 0, max = 1, value = 0.005, step = 0.001, round = FALSE, ticks = TRUE, sep = ",", post = "py^-1")
})

# HIV-related mortality over-ride
output$UI_HIVMortalityOverRide <- renderUI({
    sliderInput(inputId = "uHIVMortality_OverRide", label = "HIV-related Mortality Modifier", min = 0, max = 10, value = 1, step = 0.1, round = FALSE, ticks = TRUE, sep = ",")
})

# Reactives - set initial values
AdvCalib <- reactiveValues(NatMort = 0.005, HIVMort = 1)

# Observe changes when UI elements interacted with
observeEvent(input$uNaturalMortality_OverRide, {
    AdvCalib$NatMort <<- input$uNaturalMortality_OverRide
})

observeEvent(input$uHIVMortality_OverRide, {
    AdvCalib$HIVMort <<- input$uHIVMortality_OverRide
})

# RESET
observeEvent(input$resetOverRide, {
    # Reset 'over-ride' advanced calibration features
    updateSliderInput(session, inputId = "uNaturalMortality_OverRide", value = 0.005)
    updateSliderInput(session, inputId = "uHIVMortality_OverRide",     value = 1)
})
