output$vbOptim_909090_1 <- renderValueBox({
    # rely on REPEAT_optim button press
    input$REPEAT_optim

    # Identify the 'best fit' parameter values
    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    PLHIV <- sum(alt[alt$time == 5, "N"])
    DX    <- sum(alt[alt$time == 5, c("Dx", "Care", "PreLtfu", "ART", "Ltfu")])

    out <- round(DX / PLHIV, digits = 2)

    valueBox(
        value = scales::percent(out),
        subtitle = "Diagnosed",
        color = "red",
        icon = icon("medkit", lib = "font-awesome")
    )
  })

output$vbOptim_909090_2 <- renderValueBox({
    # rely on REPEAT_optim button press
    input$REPEAT_optim

    # Identify the 'best fit' parameter values
    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    DX    <- sum(alt[alt$time == 5, c("Dx", "Care", "PreLtfu", "ART", "Ltfu")])
    TX    <- sum(alt[alt$time == 5, "ART"])

    out <- round(TX / DX, digits = 2)

    valueBox(
        value = scales::percent(out),
        subtitle = "On Treatment",
        color = "orange",
        icon = icon("medkit", lib = "font-awesome")
    )
  })

output$vbOptim_909090_3 <- renderValueBox({
    # rely on REPEAT_optim button press
    input$REPEAT_optim

    # Identify the 'best fit' parameter values
    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = mean(selectedResults$Rho),
        q = mean(selectedResults$Q),
        Kappa = mean(selectedResults$Kappa),
        Gamma = mean(selectedResults$Gamma),
        Sigma = mean(selectedResults$Sigma),
        Omega = mean(selectedResults$Omega))

    TX    <- sum(alt[alt$time == 5, "ART"])
    VS    <- sum(alt[alt$time == 5, "Vs"])

    out <- round(VS / TX, digits = 2)

    valueBox(
        value = scales::percent(out),
        subtitle = "Virally Suppressed",
        color = "green",
        icon = icon("medkit", lib = "font-awesome")
    )
  })

output$vbOptim_COST <- renderValueBox({
    # rely on REPEAT_optim button press
    input$REPEAT_optim

    # Identify the 'best fit' parameter values
    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

    # Subset data using opt_VS_cutoff
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

    cost <- (cumsum(alt$TotalCost)[251] - alt$TotalCost[1]) - (cumsum(baseline$TotalCost)[251] - baseline$TotalCost[1])

    valueBox(
        value = scales::dollar(cost),
        subtitle = "Additional Cost of Care between 2015 and 2020",
        color = "green",
        icon = icon("money", lib = "font-awesome")
    )
  })
