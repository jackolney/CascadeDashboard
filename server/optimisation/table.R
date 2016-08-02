# Data Table Render Functions

output$optimDTmodal <- DT::renderDataTable({
    return(datatable(optResults,
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = 25,
            autoWidth = FALSE,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                "}")
            )
        ) %>%
        formatRound("First 90",3) %>%
        formatRound("Second 90",3) %>%
        formatRound("Third 90",3) %>%
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
        extensions = 'Buttons',
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
