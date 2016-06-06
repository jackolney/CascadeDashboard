# Optimisation Test Bed

observeEvent(input$NEXT_optIntro, {
    # Run Optimsation Function
    optResult <<- RunOptimisation()
})

output$plotOptim_result <- renderPlot({
    input$NEXT_optIntro
    BuildOptimisationPlot(theOut = optResult)
}, height = 400, width = 'auto', bg = 'transparent')

BuildOptimisationPlot <- function(theOut) {

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(theOut, theOut$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    Intervention <- c(
        "Testing",
        "Linkage",
        "Pre-ART Retention",
        "Initiation",
        "Adherence",
        "ART Retention"
    )

    # The values used in uiOutput()
    Value <- c(
        round(  (cumsum(alt$Dx)[251]           - alt$Dx[1]          ) - (cumsum(baseline$Dx)[251]   - baseline$Dx[1]    ),   digits = 0),
        round(  (cumsum(alt$Care)[251]         - alt$Care[1]        ) - (cumsum(baseline$Care)[251] - baseline$Care[1]  ),   digits = 0),
        round(  (cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]) - (cumsum(alt$PreLtfu)[251]   - alt$PreLtfu[1]    ),   digits = 0),
        round(  (cumsum(alt$Tx)[251]           - alt$Tx[1]          ) - (cumsum(baseline$Tx)[251]   - baseline$Tx[1]    ),   digits = 0),
        round(  (cumsum(alt$Vs)[251]           - alt$Vs[1]          ) - (cumsum(baseline$Vs)[251]   - baseline$Vs[1]    ),   digits = 0),
        round(  (cumsum(baseline$Ltfu)[251]    - baseline$Ltfu[1]   ) - (cumsum(alt$Ltfu)[251]      - alt$Ltfu[1]       ),   digits = 0)
    )

    resultOut <- data.frame(Intervention, Value)
    resultOut$Intervention <- factor(resultOut$Intervention, levels = Intervention)

    ggOut <- ggplot(resultOut, aes(x = Intervention, y = Value, fill = Intervention))
    ggOut <- ggOut + geom_bar(stat = "identity")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle(
        label = "Required Changes to Care",
        subtitle =
            paste0("Average changes to care required to achieve ",
                input$opt_VS_cutoff,
                "% viral suppression"
            )
        )
    ggOut <- ggOut + theme(title = element_text(size = 15))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut
}

output$optResult_testing <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # we need to test ___ more people
    # base_answer <- cumsum(baseline$Dx)[251] - baseline$Dx[1]
    alt_answer <- cumsum(alt$Dx)[251] - alt$Dx[1]
    # scales::comma(round(alt_answer - base_answer, digits = 0))
    tags$code(scales::comma(round(alt_answer, digits = 0)))
})

output$optResult_linkage <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # we need to test ___ more people
    # base_answer <- cumsum(baseline$Care)[251] - baseline$Care[1]
    alt_answer <- cumsum(alt$Care)[251] - alt$Care[1]
    # scales::comma(round(alt_answer - base_answer, digits = 0))
    tags$code(scales::comma(round(alt_answer, digits = 0)))
})

output$optResult_preRetention <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # base_answer <- cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]
    alt_answer <- cumsum(alt$PreLtfu)[251] - alt$PreLtfu[1]
    # scales::comma(round(base_answer - alt_answer, digits = 0))
    tags$code(scales::comma(round(alt_answer, digits = 0)))
})

output$optResult_initiation <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # base_answer <- cumsum(baseline$Tx)[251] - baseline$Tx[1]
    alt_answer <- cumsum(alt$Tx)[251] - alt$Tx[1]
    # scales::comma(round(alt_answer - base_answer, digits = 0))
    tags$code(scales::comma(round(alt_answer, digits = 0)))
})

output$optResult_adherence <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # base_answer <- cumsum(baseline$Vs)[251] - baseline$Vs[1]
    alt_answer <- cumsum(alt$Vs)[251] - alt$Vs[1]
    # scales::comma(round(alt_answer - base_answer, digits = 0))
    tags$code(scales::comma(round(alt_answer, digits = 0)))
})


output$optResult_retention <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # base_answer <- cumsum(baseline$Ltfu)[251] - baseline$Ltfu[1]
    alt_answer <- cumsum(alt$Ltfu)[251] - alt$Ltfu[1]
    # scales::comma(round(base_answer - alt_answer, digits = 0))
    tags$code(scales::comma(round(alt_answer, digits = 0)))
})

output$optResult_cost <- renderUI({

    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    # base_answer <- baseline$TotalCost[251]
    alt_answer <- alt$TotalCost[251]
    # scales::comma(round(alt_answer - base_answer, digits = 0))
    tags$code(scales::dollar(round(alt_answer, digits = 0)))
})

