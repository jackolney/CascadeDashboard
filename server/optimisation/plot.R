output$plotOptim_result <- renderPlot({
    input$NEXT_optIntro
    BuildOptimisationPlot(theOut = optResults)
}, height = 400, width = 'auto', bg = 'transparent')

# Best Fit Calibration Plot
output$optCalibBestFit <- renderPlot({
    input$NEXT_calib
    BuildCalibrationBestFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = input$minResults, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)
}, height = 750, width = 'auto', bg = 'transparent')

output$plotFrontier <- renderPlot({
    input$NEXT_optIntro
    BuildFrontierPlot(CalibParamOut = CalibParamOut, optResults = optResults)
}, height = 500, width = 'auto')
