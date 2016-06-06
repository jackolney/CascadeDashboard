output$vbOptim_909090_1 <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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
        subtitle = "Diagnosed by 2020",
        color = "red",
        icon = icon("medkit", lib = "font-awesome")
    )
  })

output$vbOptim_909090_2 <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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
        subtitle = "On Treatment by 2020",
        color = "orange",
        icon = icon("medkit", lib = "font-awesome")
    )
  })

output$vbOptim_909090_3 <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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
        subtitle = "Virally Suppressed by 2020",
        color = "green",
        icon = icon("medkit", lib = "font-awesome")
    )
  })

output$vbOptim_COST <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    cost <- alt$TotalCost[251] - baseline$TotalCost[251]

    valueBox(
        value = scales::dollar(cost),
        subtitle = "Additional Cost of Care between 2015 and 2020",
        color = "green",
        icon = icon("usd", lib = "font-awesome")
    )
  })

output$vbOptim_testing <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    out <- round((cumsum(alt$Dx)[251] - alt$Dx[1]) - (cumsum(baseline$Dx)[251] - baseline$Dx[1]), digits = 0)

    infoBox(
        title = "Testing",
        value = scales::comma(out),
        color = "orange",
        subtitle = "Additional diagnoses",
        width = NULL,
        fill = TRUE,
        icon = icon("check-square-o", lib = "font-awesome")
    )
})


output$vbOptim_linkage <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    out <- round((cumsum(alt$Care)[251] - alt$Care[1] ) - (cumsum(baseline$Care)[251] - baseline$Care[1] ), digits = 0)

    infoBox(
        title = "Linkage",
        value = scales::comma(out),
        color = "orange",
        subtitle = "Additional linkage",
        width = NULL,
        fill = TRUE,
        icon = icon("check-square-o", lib = "font-awesome")
    )
})

output$vbOptim_preRetention <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    out <- round((cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]) - (cumsum(alt$PreLtfu)[251] - alt$PreLtfu[1]), digits = 0)

    infoBox(
        title = "Pre-ART Retention",
        value = scales::comma(out),
        color = "orange",
        subtitle = "Additional pre-ART retention",
        width = NULL,
        fill = TRUE,
        icon = icon("check-square-o", lib = "font-awesome")
    )
})

output$vbOptim_initiation <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    out <- round((cumsum(alt$Tx)[251] - alt$Tx[1]) - (cumsum(baseline$Tx)[251] - baseline$Tx[1]), digits = 0)

    infoBox(
        title = "ART Initiation",
        value = scales::comma(out),
        color = "orange",
        subtitle = "Additional ART initiations",
        width = NULL,
        fill = TRUE,
        icon = icon("check-square-o", lib = "font-awesome")
    )
})

output$vbOptim_adherence <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    out <- round((cumsum(alt$Vs)[251] - alt$Vs[1]) - (cumsum(baseline$Vs)[251] - baseline$Vs[1]), digits = 0)

    infoBox(
        title = "Adherence",
        value = scales::comma(out),
        color = "orange",
        subtitle = "Additional adherence",
        width = NULL,
        fill = TRUE,
        icon = icon("check-square-o", lib = "font-awesome")
    )
})

output$vbOptim_retention <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro

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

    out <- round((cumsum(baseline$Ltfu)[251] - baseline$Ltfu[1]) - (cumsum(alt$Ltfu)[251] - alt$Ltfu[1]), digits = 0)

    infoBox(
        title = "ART Retention",
        value = scales::comma(out),
        color = "orange",
        subtitle = "Additional ART retention",
        width = NULL,
        fill = TRUE,
        icon = icon("check-square-o", lib = "font-awesome")
    )
})
