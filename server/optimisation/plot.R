output$plotOptim_result <- renderPlot({
    input$NEXT_optIntro
    BuildOptimisationPlot(theOut = optResults)
}, height = 400, width = 'auto', bg = 'transparent')

# Best Fit Calibration Plot
output$optCalibBestFit <- renderPlot({
    input$NEXT_calib
    BuildCalibrationBestFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = input$minResults, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)
}, height = 750, width = 'auto', bg = 'transparent')

output$plotOptim_CostImpact <- renderPlot({

    input$NEXT_optIntro

    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    optResults <- dplyr::mutate(optResults,
        'Testing'           = round(as.numeric(optResults$Rho),   digits = 4),
        'Linkage'           = round(as.numeric(optResults$Q),     digits = 4),
        'Pre-ART Retention' = round(as.numeric(optResults$Kappa), digits = 4),
        'Initiation'        = round(as.numeric(optResults$Gamma), digits = 4),
        'Adherence'         = round(as.numeric(optResults$Sigma), digits = 4),
        'ART Retention'     = round(as.numeric(optResults$Omega), digits = 4)
    )

    optResults[["Testing"]]           <- factor(optResults[["Testing"]],           levels = unique(optResults[["Testing"]]))
    optResults[["Linkage"]]           <- factor(optResults[["Linkage"]],           levels = unique(optResults[["Linkage"]]))
    optResults[["Pre-ART Retention"]] <- factor(optResults[["Pre-ART Retention"]], levels = unique(optResults[["Pre-ART Retention"]]))
    optResults[["Initiation"]]        <- factor(optResults[["Initiation"]],        levels = unique(optResults[["Initiation"]]))
    optResults[["Adherence"]]         <- factor(optResults[["Adherence"]],         levels = unique(optResults[["Adherence"]]))
    optResults[["ART Retention"]]     <- factor(optResults[["ART Retention"]],     levels = unique(optResults[["ART Retention"]]))

    theStratPoint <<- input$userStratPoint

    ggOut <- ggplot(optResults, aes(x = VS, y = Cost))
    ggOut <- ggOut + geom_point(aes(color = as.factor(get(theStratPoint))), alpha = 0.75, size = 5)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + expand_limits(y = round(max(optResults$Cost), digits = -4))
    ggOut <- ggOut + scale_color_discrete(name = input$userStratPoint)
    ggOut <- ggOut + theme(legend.title = element_text(size = 14))
    ggOut <- ggOut + theme(legend.text = element_text(size = 13))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
    ggOut <- ggOut + theme(axis.title = element_text(size = 15))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + geom_vline(xintercept = input$opt_VS_cutoff / 100)
    ggOut <- ggOut + xlab("Proportion achieving viral suppression by 2020")
    ggOut <- ggOut + ylab("Additional cost of care (2013 USD)")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + coord_cartesian(xlim = plotOptimCostImpact.ranges$x, ylim = plotOptimCostImpact.ranges$y)
    ggOut

}, height = 400, width = 'auto')
