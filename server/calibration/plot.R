output$plotCalibration <- renderPlot({
    # Dependency on 'REPEAT_calib' being hit
    # When 'repeat' button pressed, then we re-run calibration
    input$CALIB_data
    input$REPEAT_calib
    RunCalibration(country = input$selectCountry, data = MasterData, maxIterations = 1e4, maxError = input$maxError, limit = input$minResults)
    BuildCalibrationPlot(data = CalibOut, originalData = MasterData)
}, height = 400, width = 'auto', bg = 'transparent')

output$plotCalibrationDetail <- renderPlot({
    # Dependency on 'REPEAT_calib' being hit
    # When 'repeat' button pressed, then we re-run calibration
    input$CALIB_data
    input$REPEAT_calib
    input$maxError
    input$minResults
    BuildCalibrationPlotDetail(data = CalibOut, originalData = MasterData, limit = input$minResults)
}, height = 750, width = 'auto', bg = 'transparent')

output$plotCalibHist <- renderPlot({
    input$CALIB_data
    input$REPEAT_calib
    input$minResults
    BuildCalibrationHistogram(runError = runError, maxError = input$maxError)
}, height = 200, width = 'auto', bg = 'transparent')

output$plotData <- renderPlot({
    # add a dependency if we update data, like a 'review' changes.
    input$NEXT_country
    input$PREV_plhiv
    input$NEXT_viral
    BuildDataReviewPlot(data = MasterData$calib)
}, height = 400, width = 'auto', bg = 'transparent')

output$plotCalibHist_rho <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "rho")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_q <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "q")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_epsilon <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "epsilon")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_gamma <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "gamma")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_theta <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "theta")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_kappa <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "kappa")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_omega <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "omega")
}, height = 200, width = 'auto', bg = 'transparent')

output$plotCalibHist_p <- renderPlot({
    input$ADJ_param
    input$PREV_plhiv
    input$NEXT_viral
    BuildCalibrationParamHist(pOut = CalibParamOut, param = "p")
}, height = 200, width = 'auto', bg = 'transparent')
