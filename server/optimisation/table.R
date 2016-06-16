# Data Table Render Functions
output$optimDTout <- DT::renderDataTable({

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

    Intervention <- c(
        "Testing",
        "Linkage",
        "Pre-ART Retention",
        "Initiation",
        "Adherence",
        "ART Retention"
    )

    Description <- c(
        "The number of additional individuals requiring diagnosis",
        "The number of additional individuals that need to be linked to care",
        "The number of additional individuals that need to be retained in pre-ART care",
        "The number of additional individuals that need to be initiated onto treatment",
        "The number of additional individuals that need to fully adhere to treatment",
        "The number of additional individuals that need to be retained on ART"
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

    optimDT <- data.frame(Intervention, Description, Value)

    DT::datatable(optimDT,
        style = 'bootstrap',
        rownames = FALSE,
        options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                "}"),
            autoWidth = FALSE,
            columnDefs = list(list(className = 'dt-center', targets = 2))
        )
    ) %>% formatStyle(
        columns = 'Intervention',
        color = 'black',
        fontWeight = 'bold'
    ) %>% formatStyle(
        columns = 'Value',
        background = ColorFromMiddle(data = cbind(0, optimDT$Value), color1 = '#CF553D', color2 = '#46A55F'),
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


output$optimDT909090modal <- DT::renderDataTable({
    # Shows the same values as optimDTmodal
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

output$bestFitDT <- DT::renderDataTable({
    p <- parameters(
        prop_preART_500    = MasterCD4_2015[1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = MasterCD4_2015[1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = MasterCD4_2015[1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = MasterCD4_2015[1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = MasterCD4_2015[1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = MasterCD4_2015[1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = MasterCD4_2015[1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear2015(MasterData[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less200"]]),
        Rho = CalibParamOut[minErrorRun,"rho"],
        Epsilon = CalibParamOut[minErrorRun,"epsilon"],
        Kappa = CalibParamOut[minErrorRun,"kappa"],
        Gamma = CalibParamOut[minErrorRun,"gamma"],
        Theta = CalibParamOut[minErrorRun,"theta"],
        Omega = CalibParamOut[minErrorRun,"omega"],
        p = CalibParamOut[minErrorRun,"p"],
        q = CalibParamOut[minErrorRun,"q"]
    )

    parameter <- c(
        "Rho",
        "Epsilon",
        "q",
        "Kappa",
        "Gamma",
        "p",
        "Theta",
        "Omega"
    )

    description <- c(
        "Diagnosis rate",
        "Linkage rate",
        "Proportion successfully linking to care",
        "Pre-ART dropout rate",
        "ART initiation rate for eligible individuals in care",
        "Proportion of patients adhering to treatment at initiation",
        "ART initiation rate for eligible individuals not currently in care",
        "ART dropout rate"
    )

    value <- c(
        round(p[["Rho"]],     digits = 4),
        round(p[["Epsilon"]], digits = 4),
        round(p[["q"]],       digits = 4),
        round(p[["Kappa"]],   digits = 4),
        round(p[["Gamma"]],   digits = 4),
        round(p[["p"]],       digits = 4),
        round(p[["Theta"]],   digits = 4),
        round(p[["Omega"]],   digits = 4)
    )

    time <- c(
        round(1 / p[["Rho"]],     digits = 4),
        round(1 / p[["Epsilon"]], digits = 4),
        "-",
        round(1 / p[["Kappa"]],   digits = 4),
        round(1 / p[["Gamma"]],   digits = 4),
        "-",
        round(1 / p[["Theta"]],   digits = 4),
        round(1 / p[["Omega"]],   digits = 4)
    )

    df <- data.frame(parameter, description, value, time)

    colnames(df) <- c("Parameter", "Description", "Value", "Rate")

    DT::datatable(df,
        style = 'bootstrap',
        rownames = FALSE,
        options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                "}"),
            autoWidth = FALSE
        )
    )
})
