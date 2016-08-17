output$vb909090_COST <- renderValueBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    val <- mean(intResult[,"iCost"]) / 5

    report_909090_cost <<- scales::dollar(val)

    valueBox(
        value = scales::dollar(val),
        subtitle = "Additional cost of care per year between 2015 and 2020",
        color = "gray",
        icon = icon("usd", lib = "font-awesome")
    )
  })

output$vb909090_COST_OG <- renderValueBox({
    input$NEXT_optIntro

    # The cost at 'baseline' regardless of what VS was achieved.
    val <- mean(BaselineCost) / 5

    valueBox(
        value = scales::dollar(val),
        subtitle = "Baseline cost of care per year between 2015 and 2020",
        color = "gray",
        icon = icon("usd", lib = "font-awesome")
    )
  })

output$vb909090_COST_NEW <- renderValueBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    val <- mean(intResult[,"iCost"]) / 5

    report_909090_cost <<- scales::dollar(val)

    valueBox(
        value = scales::dollar(val),
        subtitle = "Additional cost of care per year between 2015 and 2020",
        color = "gray",
        icon = icon("usd", lib = "font-awesome")
    )
  })

output$vb909090_testing <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    values <- colMeans(intResult[,names(intResult) != c("iCost", "iTCst")])

    val <- values["iTest"] / 5

    out <- scales::comma(round(val, digits = 0))

    report_909090_testing <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Testing",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "iTest")],
        subtitle = "Additional diagnoses per year",
        width = NULL,
        fill = TRUE,
        icon = icon("user-md", lib = "font-awesome")
    )
})


output$vb909090_linkage <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    values <- colMeans(intResult[,names(intResult) != c("iCost", "iTCst")])

    val <- values["iLink"] / 5

    out <- scales::comma(round(val, digits = 0))

    report_909090_linkage <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Linkage",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "iLink")],
        subtitle = "Additional linkages per year",
        width = NULL,
        fill = TRUE,
        icon = icon("ambulance", lib = "font-awesome")
    )
})

output$vb909090_preRetention <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    values <- colMeans(intResult[,names(intResult) != c("iCost", "iTCst")])

    val <- abs(values["iPreR"]) / 5

    out <- scales::comma(round(val, digits = 0))

    report_909090_preRetention <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Pre-ART Retention",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "iPreR")],
        subtitle = "Reduction in losses from pre-ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

output$vb909090_initiation <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    values <- colMeans(intResult[,names(intResult) != c("iCost", "iTCst")])

    val <- values["iInit"] / 5

    out <- scales::comma(round(val, digits = 0))

    report_909090_initiation <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "ART Initiation",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "iInit")],
        subtitle = "Additional ART initiations per year",
        width = NULL,
        fill = TRUE,
        icon = icon("medkit", lib = "font-awesome")
    )
})

output$vb909090_adherence <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    values <- colMeans(intResult[,names(intResult) != c("iCost", "iTCst")])

    val <- values["iAdhr"] / 5

    out <- scales::comma(round(val, digits = 0))

    report_909090_adherence <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Adherence",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "iAdhr")],
        subtitle = "Additional non-adherence transitions per year",
        width = NULL,
        fill = TRUE,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

output$vb909090_retention <- renderInfoBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    values <- colMeans(intResult[,names(intResult) != c("iCost", "iTCst")])

    val <- abs(values["iRetn"]) / 5

    out <- scales::comma(round(val, digits = 0))

    report_909090_retention <<- out

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "ART Retention",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "iRetn")],
        subtitle = "Reduction in losses from ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("heart-o", lib = "font-awesome")
    )
})
