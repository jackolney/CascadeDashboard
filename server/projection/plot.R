output$plotValidation_PLHIV <- renderPlot({
    input$NEXT_calib
    GenYourCascadePlot(1)
    },
    height = 400,
    width = 'auto',
    bg = "transparent"
)

output$plotValidation_DIAG <- renderPlot({
    input$NEXT_calib
    GenYourCascadePlot(2)
    },
    height = 400,
    width = 'auto',
    bg = "transparent"
)

output$plotValidation_CARE <- renderPlot({
    input$NEXT_calib
    GenYourCascadePlot(3)
    },
    height = 400,
    width = 'auto',
    bg = "transparent"
)

output$plotValidation_ART <- renderPlot({
    input$NEXT_calib
    GenYourCascadePlot(4)
    },
    height = 400,
    width = 'auto',
    bg = "transparent"
)

output$plotValidation_SUPP <- renderPlot({
    input$NEXT_calib
    GenYourCascadePlot(5)
    },
    height = 400,
    width = 'auto',
    bg = "transparent"
)

output$plotCascade <- renderPlot({
    input$NEXT_calib
    GenCascadePlot()
    },
    height = 400,
    width = 'auto'
)

output$plotPowersCascade <- renderPlot({
    input$NEXT_calib
    GenPowersCascadePlot()
    },
    height = 400,
    width = 'auto'
)

output$plot909090 <- renderPlot({
    input$NEXT_calib
    Gen909090Plot()
    },
    height = 500,
    width = 'auto'
)

output$plotNewInf <- renderPlot({
    input$NEXT_calib
    GenNewInfPlot(wizard = FALSE)
    },
    height = 500,
    width = 'auto'
)

output$plotAidsDeaths <- renderPlot({
    input$NEXT_calib
    GenAidsDeathsPlot(wizard = FALSE)
    },
    height = 500,
    width = 'auto'
)

output$plotSingle <- renderPlot({
    input$NEXT_calib
    GenSinglePlot()
    },
    height = 400,
    width = 'auto'
)

output$plotAll <- renderPlot({
    input$NEXT_calib
    GenAllPlot()
    },
    height = 800,
    width = 'auto'
)

output$plotNewInf_wizard <- renderPlot({
    input$NEXT_calib
    GenNewInfPlot(wizard = TRUE)
    },
    height = 240,
    width = 'auto'
)

output$plotAidsDeaths_wizard <- renderPlot({
    input$NEXT_calib
    GenAidsDeathsPlot(wizard = TRUE)
    },
    height = 240,
    width = 'auto'
)

output$plotCascade_wizard <- renderPlot({
    input$NEXT_calib
    GenCascadePlot()
    },
    height = 400,
    width = 'auto'
)
