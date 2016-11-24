output$plotOptim_result <- renderPlot({
    input$NEXT_optIntro
    BuildOptimisationPlot(theOut = optResults)
}, height = 400, width = 'auto', bg = 'transparent')

# Random 10% selection of runs
output$optCalibBestFit <- renderPlot({
    input$NEXT_calib
    BuildCalibrationRandomFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = input$minResults, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)
}, height = 750, width = 'auto', bg = 'transparent')

output$plotFrontier <- renderPlot({
    input$NEXT_optIntro
    BuildFrontierPlot(CalibParamOut = CalibParamOut, optResults = optResults, target = custom$target)
}, height = 500, width = 'auto')

output$plotChanges <- renderPlot({
    input$NEXT_optIntro
    BuildChangesPlot(CalibParamOut = CalibParamOut, optResults = optResults, target = custom$target)
}, height = 400, width = 'auto', bg = 'transparent')
