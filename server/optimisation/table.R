# Data Table Render Functions
output$optimDTout <- DT::renderDataTable({

    # rely on REPEAT_optim button press
    input$REPEAT_optim

    # Identify the 'best fit' parameter values
    bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        sampleMinErrorRun = sampleMinErrorRun)

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResult, optResult$VS >= (input$opt_VS_cutoff / 100))

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        sampleMinErrorRun = sampleMinErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        sampleMinErrorRun = sampleMinErrorRun,
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

    Description <- c(
        "The number of individuals requiring diagnosis",
        "The number of individuals that need to be linked to care",
        "The number of individuals that need to be retained in pre-ART care",
        "The number of individuals that need to be initiated onto treatment",
        "The number of individuals that need to fully adhere to treatment",
        "The number of individuals that need to be retained on ART"
    )

    # The values used in uiOutput()
    Value <- c(
        round(cumsum(alt$Dx)[251]           - alt$Dx[1]           - cumsum(baseline$Dx)[251]   - baseline$Dx[1],   digits = 0),
        round(cumsum(alt$Care)[251]         - alt$Care[1]         - cumsum(baseline$Care)[251] - baseline$Care[1], digits = 0),
        round(cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1] - cumsum(alt$PreLtfu)[251]   - alt$PreLtfu[1],   digits = 0),
        round(cumsum(alt$Tx)[251]           - alt$Tx[1]           - cumsum(baseline$Tx)[251]   - baseline$Tx[1],   digits = 0),
        round(cumsum(alt$Vs)[251]           - alt$Vs[1]           - cumsum(baseline$Vs)[251]   - baseline$Vs[1],   digits = 0),
        round(cumsum(baseline$Ltfu)[251]    - baseline$Ltfu[1]    - cumsum(alt$Ltfu)[251]      - alt$Ltfu[1],      digits = 0)
    )

    # The proportion of simulations that required that thing (then will add a bar in post processing)
    Use <- c(
        sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
        sum(unlist(lapply((selectedResults$Q     / bestPar[["q"]]),        function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
        sum(unlist(lapply((bestPar[["Kappa"]]    / selectedResults$Kappa), function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
        sum(unlist(lapply((selectedResults$Gamma / bestPar[["Gamma"]]),    function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
        sum(unlist(lapply((selectedResults$Sigma),                         function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
        sum(unlist(lapply((bestPar[["Omega"]]    / selectedResults$Omega), function(x) if (x > 1) TRUE))) / dim(selectedResults)[1]
    )
    Use <- scales::percent(Use)

    optimDT <- data.frame(Intervention, Description, Value, Use)

    DT::datatable(optimDT,
        style = 'bootstrap',
        rownames = FALSE,
        options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                "}"),
            autoWidth = FALSE
            # columnDefs = list(list(width = 'auto', targets = 2))
        )
    ) %>% formatStyle(
        columns = 'Use',
        'text-align' = 'right'
    ) %>% formatStyle(
        columns = 'Intervention',
        color = 'black',
        fontWeight = 'bold'
    ) %>% formatStyle(
        columns = 'Value',
        background = ColorFromMiddle(data = cbind(0, optimDT$Value), color1 = '#FF6347', color2 = '#90EE90'),
        backgroundSize = '100% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
    ) %>% formatCurrency(columns = 'Value', currency = '', interval = 3)
})

output$optimDTmodal <- DT::renderDataTable({
    return(datatable(Result_VS,
        style = 'bootstrap',
        options = list(
            pageLength = 25,
            autoWidth = FALSE,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                "}")
            )
        ) %>%
        formatRound("90",3) %>%
        formatRound("90-90",3) %>%
        formatRound("90-90-90",3) %>%
        formatRound("VS",3) %>%
        formatRound("Rho",3) %>%
        formatRound("Q",3) %>%
        formatRound("Kappa",3) %>%
        formatRound("Gamma",3) %>%
        formatRound("Sigma",3) %>%
        formatRound("Omega",3) %>%
        formatCurrency("Cost",'$')
        )
    }
)

output$optParTable_Rho <- renderTable({
    theData <- seq(from = input$userOptRho_Range[1], to = input$userOptRho_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Q <- renderTable({
    theData <- seq(from = input$userOptq_Range[1], to = input$userOptq_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Kappa <- renderTable({
    theData <- seq(from = input$userOptKappa_Range[2], to = input$userOptKappa_Range[1], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Gamma <- renderTable({
    theData <- seq(from = input$userOptGamma_Range[1], to = input$userOptGamma_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Sigma <- renderTable({
    theData <- seq(from = input$userOptSigma_Range[1], to = input$userOptSigma_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Omega <- renderTable({
    theData <- seq(from = input$userOptOmega_Range[2], to = input$userOptOmega_Range[1], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)
