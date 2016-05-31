observeEvent(input$NEXT_calib, {

    # Upon pressing the 'next' button, read in the 2015 CD4 disitribution and run the model
    # Produce the relevant plots.

    MasterCD4_2015 <<- GetCD4Distribution2015(input$selectCountry)

})

observeEvent(input$userRetArt12mths, {
    if (input$userRetArt12mths != 0 || is.na(input$userRetArt12mths)) {
        newValue <- -log(input$userRetArt12mths)
        updateSliderInput(session, "omega", value = newValue, min = 0, max = 5, step = 0.01)
    }
})

# New Plot
observeEvent(input$plotOptim_CostImpact_dblclick, {
    brush <- input$plotOptim_CostImpact_brush
    if (!is.null(brush)) {
        plotOptimCostImpact.ranges$x <- c(brush$xmin, brush$xmax)
        plotOptimCostImpact.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOptimCostImpact.ranges$x <- NULL
        plotOptimCostImpact.ranges$y <- NULL
    }
})

# Plot 1
observeEvent(input$plotOpt909090_dblclick, {
    brush <- input$plotOpt909090_brush
    if (!is.null(brush)) {
        plotOpt_909090.ranges$x <- c(brush$xmin, brush$xmax)
        plotOpt_909090.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOpt_909090.ranges$x <- NULL
        plotOpt_909090.ranges$y <- NULL
    }
})

# Plot 2
observeEvent(input$plotOptDALYs_dblclick, {
    brush <- input$plotOptDALYs_brush
    if (!is.null(brush)) {
        plotOpt_DALYs.ranges$x <- c(brush$xmin, brush$xmax)
        plotOpt_DALYs.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOpt_DALYs.ranges$x <- NULL
        plotOpt_DALYs.ranges$y <- NULL
    }
})

# Plot 3
observeEvent(input$plotOptDALYs909090_dblclick, {
    brush <- input$plotOptDALYs909090_brush
    if (!is.null(brush)) {
        plotOpt_DALYs_909090.ranges$x <- c(brush$xmin, brush$xmax)
        plotOpt_DALYs_909090.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOpt_DALYs_909090.ranges$x <- NULL
        plotOpt_DALYs_909090.ranges$y <- NULL
    }
})

# -------------- #
# Button Control #
# -------------- #

# Plot 1
observeEvent(input$showOpt909090Plot, ({updateCollapse(session, "optCollapse", open = "Plot 90-90-90")}))

# Plot 2
observeEvent(input$showOptDALYsPlot, ({updateCollapse(session, "optCollapse", open = "Plot DALYs")}))

# Plot 3
observeEvent(input$showOptDALYs909090Plot, ({updateCollapse(session, "optCollapse", open = "Plot DALYs (90-90-90)")}))

# Reactive Budget Switch
Budget <- reactiveValues(Switch = "the909090")

observeEvent(input$showBudget909090, ({Budget$Switch <- "the909090"}))

observeEvent(input$showBudgetDALYs, ({Budget$Switch <- "DALYs"}))

observeEvent(input$resetMap, {
    leafletProxy("countryMap") %>% setView(lng = 0, lat = 30, zoom = 2)
})

observeEvent(input$resetCost, {shinyjs::reset("cost-panel")})

observeEvent(input$resetInterventions, {shinyjs::reset("optimisation-panel")})


# ART Initiation Checkbox Rules #
observeEvent(input$userART_All, {
    if (input$userART_All == TRUE) {
        updateCheckboxInput(session, "userART_500", value = TRUE)
        updateCheckboxInput(session, "userART_350", value = TRUE)
        updateCheckboxInput(session, "userART_200", value = TRUE)
    }
})

observeEvent(input$userART_500, {
    if (input$userART_500 == TRUE) {
        updateCheckboxInput(session, "userART_350", value = TRUE)
        updateCheckboxInput(session, "userART_200", value = TRUE)
    } else {
        updateCheckboxInput(session, "userART_All", value = FALSE)
    }
})

observeEvent(input$userART_350, {
    if (input$userART_350 == TRUE) {
        updateCheckboxInput(session, "userART_200", value = TRUE)
    } else {
        updateCheckboxInput(session, "userART_All", value = FALSE)
        updateCheckboxInput(session, "userART_500", value = FALSE)
    }
})

observeEvent(input$userART_200, {
    if (input$userART_200 == FALSE) {
        updateCheckboxInput(session, "userART_All", value = FALSE)
        updateCheckboxInput(session, "userART_500", value = FALSE)
        updateCheckboxInput(session, "userART_350", value = FALSE)
    }
})

# Inverse sliders for parameter window #
observeEvent(input$rho,        {updateSliderInput(session,"invRho",     value = 1/input$rho,        min = 0, max = 100, step = 0.001)})

observeEvent(input$invRho,     {updateSliderInput(session,"rho",        value = 1/input$invRho,     min = 0, max = 5,   step = 0.001)})

observeEvent(input$epsilon,    {updateSliderInput(session,"invEpsilon", value = 1/input$epsilon,    min = 0, max = 100, step = 0.001)})

observeEvent(input$invEpsilon, {updateSliderInput(session,"epsilon",    value = 1/input$invEpsilon, min = 0, max = 20,  step = 0.001)})

observeEvent(input$gamma,      {updateSliderInput(session,"invGamma",   value = 1/input$gamma,      min = 0, max = 100, step = 0.001)})

observeEvent(input$invGamma,   {updateSliderInput(session,"gamma",      value = 1/input$invGamma,   min = 0, max = 5,   step = 0.001)})

observeEvent(input$omega,      {updateSliderInput(session,"invOmega",   value = 1/input$omega,      min = 0, max = 100, step = 0.001)})

observeEvent(input$invOmega,   {updateSliderInput(session,"omega",      value = 1/input$invOmega,   min = 0, max = 5,   step = 0.001)})
