output$plotCalibration <- renderPlot({
    # Dependency on 'REPEAT_calib' being hit
    # When 'repeat' button pressed, then we re-run calibration
    input$REPEAT_calib
    RunCalibration(data = MasterData, maxIterations = 1e4, maxError = input$maxError, limit = input$minResults)
    BuildCalibrationPlot(data = CalibOut, originalData = MasterData)
}, height = 400, width = 'auto', bg = 'transparent')

output$plotCalibrationDetail <- renderPlot({
    # Dependency on 'REPEAT_calib' being hit
    # When 'repeat' button pressed, then we re-run calibration
    input$REPEAT_calib
    input$maxError
    input$minResults
    BuildCalibrationPlotDetail(data = CalibOut, originalData = MasterData)
}, height = 500, width = 'auto', bg = 'transparent')

output$plotCalibHist <- renderPlot({
    input$REPEAT_calib
    input$minResults
    BuildCalibrationHistogram(runError = runError, maxError = input$maxError)
}, height = 200, width = 'auto', bg = 'transparent')

output$plotData <- renderPlot({
    # add a dependency if we update data, like a 'review' changes.
    input$PREV_plhiv
    input$NEXT_viral
    BuildDataReviewPlot(data = MasterData$calib)
}, height = 400, width = 'auto', bg = 'transparent')
