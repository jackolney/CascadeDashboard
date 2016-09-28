output$vb909090_COST <- renderValueBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    val <- Quantile_95(intResult[,"iCost"] / 5)

    if (val[['upper']] < 0) val <- 0
    if (val[['mean']]  < 0) val <- 0
    if (val[['lower']] < 0) val <- 0

    vLower <- paste0("+$", format(round(val['lower'] / 1e6, 2), trim = TRUE), "M")
    vMean  <- paste0("+$", format(round(val['mean']  / 1e6, 2), trim = TRUE), "M")
    vUpper <- paste0("+$", format(round(val['upper'] / 1e6, 2), trim = TRUE), "M")

    # out <- paste0(vMean, " [", vLower, " to ", vUpper, "]")
    out <- vMean

    report_909090_cost <<- paste0(vMean, " [", vLower, " to ", vUpper, "]")

    valueBox(
        value = out,
        subtitle = "Additional cost of care per year between 2015 and 2020",
        color = "light-blue",
        icon = icon("usd", lib = "font-awesome")
    )
})

output$vb909090_COST_NEW <- renderValueBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    val <- Quantile_95(intResult[,"iCost"] / 5)

    if (val[['upper']] < 0) val <- 0
    if (val[['mean']]  < 0) val <- 0
    if (val[['lower']] < 0) val <- 0

    vLower <- paste0("+$", format(round(val['lower'] / 1e6, 2), trim = TRUE), "M")
    vMean  <- paste0("+$", format(round(val['mean']  / 1e6, 2), trim = TRUE), "M")
    vUpper <- paste0("+$", format(round(val['upper'] / 1e6, 2), trim = TRUE), "M")

    # out <- paste0(vMean, " [", vLower, " to ", vUpper, "]")
    out <- vMean

    report_909090_cost <<- paste0(vMean, " [", vLower, " to ", vUpper, "]")

    valueBox(
        value = out,
        subtitle = "Additional cost of care per year between 2015 and 2020",
        color = "light-blue",
        icon = icon("usd", lib = "font-awesome")
    )
})

output$vb909090_testing <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)
    # Result Formatting
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    colValues <- NonZeroVectorCheck(colMeans(intResult[,names(intResult) != c("iCost", "iTCst")]))

    values <- round(Quantile_95(intResult[["iTest"]]) / 5, digit = 0)

    values <- scales::comma(values)

    out <- paste0("+", values[['mean']], " [+", values[['lower']], " to +", values[['upper']], "]")

    report_909090_testing <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Testing",
        value = out,
        color = "light-blue",
        subtitle = "Additional diagnoses per year",
        width = NULL,
        fill = TRUE,
        icon = icon("plus", lib = "font-awesome")
    )
})


output$vb909090_linkage <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)
    # Result Formatting
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    colValues <- NonZeroVectorCheck(colMeans(intResult[,names(intResult) != c("iCost", "iTCst")]))

    values <- round(Quantile_95(intResult[["iLink"]]) / 5, digit = 0)

    values <- scales::comma(values)

    out <- paste0("+", values[['mean']], " [+", values[['lower']], " to +", values[['upper']], "]")

    report_909090_linkage <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Linkage",
        value = out,
        color = "light-blue",
        subtitle = "Additional linkages per year",
        width = NULL,
        fill = TRUE,
        icon = icon("plus", lib = "font-awesome")
    )
})

output$vb909090_preRetention <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)
    # Result Formatting
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    colValues <- NonZeroVectorCheck(colMeans(intResult[,names(intResult) != c("iCost", "iTCst")]))

    values <- round(Quantile_95(intResult[["iPreR"]]) / 5, digit = 0)

    values <- scales::comma(values)

    out <- paste0("+", values[['mean']], " [+", values[['lower']], " to +", values[['upper']], "]")

    report_909090_preRetention <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Pre-ART Retention",
        value = out,
        color = "light-blue",
        subtitle = "Reduction in losses from pre-ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("plus", lib = "font-awesome")
    )
})

output$vb909090_initiation <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)
    # Result Formatting
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    colValues <- NonZeroVectorCheck(colMeans(intResult[,names(intResult) != c("iCost", "iTCst")]))

    values <- round(Quantile_95(intResult[["iInit"]]) / 5, digit = 0)

    values <- scales::comma(values)

    out <- paste0("+", values[['mean']], " [+", values[['lower']], " to +", values[['upper']], "]")

    report_909090_initiation <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "ART Initiation",
        value = out,
        color = "light-blue",
        subtitle = "Additional ART initiations per year",
        width = NULL,
        fill = TRUE,
        icon = icon("plus", lib = "font-awesome")
    )
})

output$vb909090_adherence <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)
    # Result Formatting
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    colValues <- NonZeroVectorCheck(colMeans(intResult[,names(intResult) != c("iCost", "iTCst")]))

    values <- round(Quantile_95(intResult[["iAdhr"]]) / 5, digit = 0)

    values <- scales::comma(values)

    out <- paste0("+", values[['mean']], " [+", values[['lower']], " to +", values[['upper']], "]")

    report_909090_adherence <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Adherence",
        value = out,
        color = "light-blue",
        subtitle = "Additional non-adherence transitions per year",
        width = NULL,
        fill = TRUE,
        icon = icon("plus", lib = "font-awesome")
    )
})

output$vb909090_retention <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)
    # Result Formatting
    intResult['iPreR'] <- abs(intResult['iPreR'])
    intResult['iRetn'] <- abs(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    colValues <- NonZeroVectorCheck(colMeans(intResult[,names(intResult) != c("iCost", "iTCst")]))

    values <- round(Quantile_95(intResult[["iRetn"]]) / 5, digit = 0)

    values <- scales::comma(values)

    out <- paste0("+", values[['mean']], " [+", values[['lower']], " to +", values[['upper']], "]")

    report_909090_retention <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "ART Retention",
        value = out,
        color = "light-blue",
        subtitle = "Reduction in losses from ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("plus", lib = "font-awesome")
    )
})
