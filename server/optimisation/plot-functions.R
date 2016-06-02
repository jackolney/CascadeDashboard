# Optimisation Test Bed

observeEvent(input$NEXT_optIntro, {
    # Run Optimsation Function
    optResult <<- RunOptimisation()
})

# Repeat trigger
observeEvent(input$REPEAT_optim, {
    optResult <<- RunOptimisation()
})

output$plotOptim_result <- renderPlot({
    input$NEXT_optIntro
    input$REPEAT_optim
    BuildOptimisationPlot(theOut = optResult)
}, height = 400, width = 'auto', bg = 'transparent')

BuildOptimisationPlot <- function(theOut) {
    # Identify the 'best fit' parameter values
    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(theOut, theOut$VS >= (input$opt_VS_cutoff / 100))

    selectedResults$testing      <- selectedResults$Rho   / bestPar[["Rho"]]
    selectedResults$linkage      <- selectedResults$Q     / bestPar[["q"]]
    selectedResults$preRetention <- bestPar[["Kappa"]]    / selectedResults$Kappa
    selectedResults$initiation   <- selectedResults$Gamma / bestPar[["Gamma"]]
    selectedResults$adherence    <- selectedResults$Sigma
    selectedResults$retention    <- bestPar[["Omega"]]    / selectedResults$Omega

    for(i in 1:dim(selectedResults)[1]) {
        selectedResults[i,"intCombinations"] <- 0
        selectedResults[i,"intCombinations"] <- if (selectedResults[i,"testing"] > 1)      {selectedResults[i,"intCombinations"] + 1} else {selectedResults[i,"intCombinations"]}
        selectedResults[i,"intCombinations"] <- if (selectedResults[i,"linkage"] > 1)      {selectedResults[i,"intCombinations"] + 1} else {selectedResults[i,"intCombinations"]}
        selectedResults[i,"intCombinations"] <- if (selectedResults[i,"preRetention"] > 1) {selectedResults[i,"intCombinations"] + 1} else {selectedResults[i,"intCombinations"]}
        selectedResults[i,"intCombinations"] <- if (selectedResults[i,"initiation"] > 1)   {selectedResults[i,"intCombinations"] + 1} else {selectedResults[i,"intCombinations"]}
        selectedResults[i,"intCombinations"] <- if (selectedResults[i,"adherence"] > 1)    {selectedResults[i,"intCombinations"] + 1} else {selectedResults[i,"intCombinations"]}
        selectedResults[i,"intCombinations"] <- if (selectedResults[i,"retention"] > 1)    {selectedResults[i,"intCombinations"] + 1} else {selectedResults[i,"intCombinations"]}
    }

    intervention <- c(
        "Testing",
        "Linkage",
        "Pre-ART Retention",
        "Initiation",
        "Adherence",
        "ART Retention")

    strength <- c(
        sum(selectedResults[selectedResults$testing      > 1, "testing"]      * selectedResults[selectedResults$testing      > 1, "VS"] * (1 / selectedResults[selectedResults$testing      > 1, "intCombinations"])) / dim(selectedResults)[1],
        sum(selectedResults[selectedResults$linkage      > 1, "linkage"]      * selectedResults[selectedResults$linkage      > 1, "VS"] * (1 / selectedResults[selectedResults$linkage      > 1, "intCombinations"])) / dim(selectedResults)[1],
        sum(selectedResults[selectedResults$preRetention > 1, "preRetention"] * selectedResults[selectedResults$preRetention > 1, "VS"] * (1 / selectedResults[selectedResults$preRetention > 1, "intCombinations"])) / dim(selectedResults)[1],
        sum(selectedResults[selectedResults$initiation   > 1, "initiation"]   * selectedResults[selectedResults$initiation   > 1, "VS"] * (1 / selectedResults[selectedResults$initiation   > 1, "intCombinations"])) / dim(selectedResults)[1],
        sum(selectedResults[selectedResults$adherence    > 1, "adherence"]    * selectedResults[selectedResults$adherence    > 1, "VS"] * (1 / selectedResults[selectedResults$adherence    > 1, "intCombinations"])) / dim(selectedResults)[1],
        sum(selectedResults[selectedResults$retention    > 1, "retention"]    * selectedResults[selectedResults$retention    > 1, "VS"] * (1 / selectedResults[selectedResults$retention    > 1, "intCombinations"])) / dim(selectedResults)[1]
    )

    # build data.frame
    resultOut <- data.frame(intervention, strength)

    # Check level order
    resultOut$intervention <- factor(resultOut$intervention, levels = intervention)

    # This show the proportion of selected runs that use a particular intervention.
    ggOut <- ggplot(resultOut, aes(x = intervention, y = strength))
    ggOut <- ggOut + geom_bar(aes(fill = intervention), stat = "identity")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle(
        label = "Weighted Impact of Individual Interventions",
        subtitle =
            paste0("Increase brought about by ",
                scales::comma(dim(selectedResults)[1]),
                " interventions, achieving ",
                input$opt_VS_cutoff,
                "% viral suppression"
            )
        )
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
    ggOut <- ggOut + theme(title =       element_text(size = 15))
    ggOut <- ggOut + theme(axis.title.y = element_blank())
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_y_continuous(expand = c(0, 0))
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

