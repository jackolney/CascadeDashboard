output$plotOptim_CostImpact <- renderPlot({

    input$NEXT_optIntro

    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    optResult <- dplyr::mutate(optResult,
        'Testing' = scales::percent(optResult$Rho / bestPar[["Rho"]]),
        'Linkage' = scales::percent(round(optResult$Q / bestPar[["q"]], digits = 2)),
        'Pre-ART Retention' = scales::percent(round(bestPar[["Kappa"]] / optResult$Kappa, digits = 0)),
        'Initiation' = scales::percent(optResult$Gamma / bestPar[["Gamma"]]),
        'Adherence' = scales::percent(round(optResult$Sigma, digits = 0)),
        'ART Retention' = scales::percent(round(bestPar[["Omega"]] / optResult$Omega, digits = 0))
    )

    optResult[["Testing"]] <- factor(optResult[["Testing"]], levels = unique(optResult[["Testing"]]))
    optResult[["Linkage"]] <- factor(optResult[["Linkage"]], levels = unique(optResult[["Linkage"]]))
    optResult[["Pre-ART Retention"]] <- factor(optResult[["Pre-ART Retention"]], levels = unique(optResult[["Pre-ART Retention"]]))
    optResult[["Initiation"]] <- factor(optResult[["Initiation"]], levels = unique(optResult[["Initiation"]]))
    optResult[["Adherence"]] <- factor(optResult[["Adherence"]], levels = unique(optResult[["Adherence"]]))
    optResult[["ART Retention"]] <- factor(optResult[["ART Retention"]], levels = unique(optResult[["ART Retention"]]))

    theStratPoint <<- input$userStratPoint

    ggOut <- ggplot(optResult, aes(x = VS, y = Cost))
    ggOut <- ggOut + geom_point(aes(color = as.factor(get(theStratPoint))), alpha = 0.5, size = 5)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + expand_limits(y = round(max(optResult$Cost), digits = -4))
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
    },
    height = 400,
    width = 'auto'
)
