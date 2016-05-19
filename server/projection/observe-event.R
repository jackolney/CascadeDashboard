observeEvent(input$NEXT_calib, {

    # Upon pressing the 'next' button, read in the 2015 CD4 disitribution and run the model
    # Produce the relevant plots.

    MasterCD4_2015 <<- GetCD4Distribution2015(input$userCountry)

})

observeEvent(input$demoInput, {
    updateSelectInput(session, "userCountry", selected = "Kenya")

    if (input$userPLHIV == 0 || is.na(input$userPLHIV)) {
        newPLHIV <- round(1.4e+6,0) # Estimate from Kenya (Marrakech)
        newDx <- round(newPLHIV * 0.79262,0) # Estimate from AMPATH
        newCare <- round(848018,0) # Estimate from Kenya (Marrakech)
        newTx <- round(748000,0) # Estimate from Kenya (Marrakech)
        newVs <- round(295000,0) # Estimate from Kenya (Marrakech)
        newLtfu <- round(0,0) # Estimate from Kenya (Marrakech)

        updateNumericInput(session, "userPLHIV", value = newPLHIV)
        updateNumericInput(session, "userDx",    value = newDx)
        updateNumericInput(session, "userCare",  value = newCare)
        updateNumericInput(session, "userTx",    value = newTx)
        updateNumericInput(session, "userVs",    value = newVs)
        updateNumericInput(session, "userLtfu",  value = newLtfu)
    } else {
        newPLHIV <- round(1.4e+6,0) # Estimate from Kenya (Marrakech)
        newDx <- round(newPLHIV * 0.79262,0) # Estimate from AMPATH
        newCare <- round(848018,0) # Estimate from Kenya (Marrakech)
        newTx <- round(748000,0) # Estimate from Kenya (Marrakech)
        newVs <- round(295000,0) # Estimate from Kenya (Marrakech)
        newLtfu <- round(0,0) # Estimate from Kenya (Marrakech)

        updateNumericInput(session, "userPLHIV", value = newPLHIV)
        updateNumericInput(session, "userDx",    value = newDx)
        updateNumericInput(session, "userCare",  value = newCare)
        updateNumericInput(session, "userTx",    value = newTx)
        updateNumericInput(session, "userVs",    value = newVs)
        updateNumericInput(session, "userLtfu",  value = newLtfu)
    }
})

observeEvent(input$userRetArt12mths, {
    if (input$userRetArt12mths != 0 || is.na(input$userRetArt12mths)) {
        newValue <- -log(input$userRetArt12mths)
        updateSliderInput(session, "omega", value = newValue, min = 0, max = 5, step = 0.01)
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
    }
)

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
    }
)

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
    }
)

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

observeEvent(input$userCountry, {
    # Find GSheet
    # theTable <- locateSheet()
    # Read new infections
    # NewInfections <<- as.double(as.double(filter(getIncidenceData(theTable), Country == input$userCountry) %>% select(NewInfections2014)))
    # if (is.na(NewInfections)) {
    #     output$warningText <- renderText({return(paste("Warning! NA value returned from", input$userCountry, "data. Using Kenya as default."))})
    #     # NewInfections <<- as.double(as.double(filter(getIncidenceData(theTable), Country == "Kenya") %>% select(NewInfections2014)))
    # } else {
    #     output$warningText <- renderText({return(paste(input$userCountry, "data loaded."))})
    # }
    # # Read CD4 distributions
    # # theCD4 <- getCD4Data(theTable)
    # if (is.na(as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.500)))) {
    #     output$warningCD4Text <- renderText({return(paste("CD4 Warning! Using Kenya as default."))})
    #     p_preArt500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.500))
    #     p_preArt350500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.350500))
    #     p_preArt250350 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.250350))
    #     p_preArt200250 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.200250))
    #     p_preArt100200 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.100200))
    #     p_preArt50100 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.50100))
    #     p_preArt50 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.50))
    #     p_onArt500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.500))
    #     p_onArt350500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.350500))
    #     p_onArt250350 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.250350))
    #     p_onArt200250 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.200250))
    #     p_onArt100200 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.100200))
    #     p_onArt50100 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.50100))
    #     p_onArt50 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.50))
    # } else {
    #     # output$warningCD4Text <- renderText({return(paste(input$userCountry,"CD4 data loaded."))})
    #     p_preArt500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.500))
    #     p_preArt350500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.350500))
    #     p_preArt250350 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.250350))
    #     p_preArt200250 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.200250))
    #     p_preArt100200 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.100200))
    #     p_preArt50100 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.50100))
    #     p_preArt50 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.50))
    #     p_onArt500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.500))
    #     p_onArt350500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.350500))
    #     p_onArt250350 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.250350))
    #     p_onArt200250 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.200250))
    #     p_onArt100200 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.100200))
    #     p_onArt50100 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.50100))
    #     p_onArt50 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.50))
    # }
    # print(paste("Country data:",NewInfections))
})

observeEvent(input$saveInput, {
    theResult <- c(input$userCountry,
        as.integer(input$userPLHIV),
        as.integer(input$userDx),
        as.integer(input$userCare),
        as.integer(input$userTx),
        as.integer(input$userVs),
        as.integer(input$userLtfu))
    print(theResult)
    saveCascadeData(theResult)
    output$saveText <- renderText({"Saved!"})
})

# Reset button stuff.
observeEvent(input$resetInput, {shinyjs::reset("setup-panel")})

observeEvent(input$resetMap, {
    leafletProxy("countryMap") %>% setView(lng = 0, lat = 30, zoom = 2)
})

observeEvent(input$resetParameters, {
    shinyjs::reset("parameter-panel-1")
    shinyjs::reset("parameter-panel-2")
    shinyjs::reset("parameter-panel-3")
    shinyjs::reset("parameter-panel-4")
    updateNumericInput(session,"userRetArt12mths",value = 0)
})

observeEvent(input$resetCost, {shinyjs::reset("cost-panel")})

observeEvent(input$resetSliders, {shinyjs::reset("optimisation-panel")})


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
