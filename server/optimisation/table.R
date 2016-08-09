# Data Table Render Functions

output$optimDT909090modal <- DT::renderDataTable({
    return(datatable(optResults,
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = 25,
            scrollX = TRUE,
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

    bestTenPercentCalibInitial <<- GetBestTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = 0.1)

    orderedRuns <- order(runError[selectedRuns])

    pRho     <- c()
    pEpsilon <- c()
    pQ       <- c()
    pKappa   <- c()
    pGamma   <- c()
    pP       <- c()
    pTheta   <- c()
    pOmega   <- c()

    # because seven indicators
    for (j in 1:(dim(bestTenPercentCalibInitial)[1] / 7)) {

        p <- parameters(
            prop_preART_500    = MasterData$cd4_2015[1,"prop.Off.ART.500"][[1]],
            prop_preART_350500 = MasterData$cd4_2015[1,"prop.Off.ART.350500"][[1]],
            prop_preART_250350 = MasterData$cd4_2015[1,"prop.Off.ART.250350"][[1]],
            prop_preART_200250 = MasterData$cd4_2015[1,"prop.Off.ART.200250"][[1]],
            prop_preART_100200 = MasterData$cd4_2015[1,"prop.Off.ART.100200"][[1]],
            prop_preART_50100  = MasterData$cd4_2015[1,"prop.Off.ART.50100"][[1]],
            prop_preART_50     = MasterData$cd4_2015[1,"prop.Off.ART.50"][[1]],
            t_1 = ConvertYear2015(MasterData[["treatment_guidelines"]][["more500"]]),
            t_2 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less500"]]),
            t_3 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less350"]]),
            t_4 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less250"]]),
            t_5 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less200"]]),
            Rho = CalibParamOut[orderedRuns[j],"rho"],
            Epsilon = CalibParamOut[orderedRuns[j],"epsilon"],
            Kappa = CalibParamOut[orderedRuns[j],"kappa"],
            Gamma = CalibParamOut[orderedRuns[j],"gamma"],
            Theta = CalibParamOut[orderedRuns[j],"theta"],
            Omega = CalibParamOut[orderedRuns[j],"omega"],
            p = CalibParamOut[orderedRuns[j],"p"],
            q = CalibParamOut[orderedRuns[j],"q"])

        pRho[j]     <- round(p[["Rho"]],     digits = 4)
        pEpsilon[j] <- round(p[["Epsilon"]], digits = 4)
        pQ[j]       <- round(p[["q"]],       digits = 4)
        pKappa[j]   <- round(p[["Kappa"]],   digits = 4)
        pGamma[j]   <- round(p[["Gamma"]],   digits = 4)
        pP[j]       <- round(p[["p"]],       digits = 4)
        pTheta[j]   <- round(p[["Theta"]],   digits = 4)
        pOmega[j]   <- round(p[["Omega"]],   digits = 4)

    }

    pName <- c("Rho", "Epsilon", "q", "Kappa", "Gamma", "p", "Theta", "Omega")

    pOut <- data.frame(pRho, pEpsilon, pQ, pKappa, pGamma, pP, pTheta, pOmega)

    names(pOut) <- pName

    DT::datatable(pOut,
        style = 'bootstrap',
        rownames = TRUE,
        options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                "}"),
            autoWidth = FALSE
        )
    )
})
