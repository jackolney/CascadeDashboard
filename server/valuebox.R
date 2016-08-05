output$vbOptim_cutoff <- renderValueBox({
    valueBox(
        value = scales::percent(input$opt_VS_cutoff / 100),
        subtitle = "Viral Suppression Achieved by 2020",
        color = "maroon",
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

output$vbOptim_909090_1 <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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
        color = "blue",
        icon = icon("check-square-o", lib = "font-awesome")
    )
  })

output$vbOptim_909090_2 <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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
        color = "blue",
        icon = icon("check-square-o", lib = "font-awesome")
    )
  })

output$vbOptim_909090_3 <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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
        color = "blue",
        icon = icon("check-square-o", lib = "font-awesome")
    )
  })

output$vbOptim_COST <- renderValueBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    cost <- (alt$TotalCost[251] - baseline$TotalCost[251]) / 5

    valueBox(
        value = scales::dollar(cost),
        subtitle = "Additional Cost of Care per year between 2015 and 2020",
        color = "green",
        icon = icon("usd", lib = "font-awesome")
    )
  })

output$vbOptim_testing <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )

    out <- scales::comma(round(values[["testing"]], digits = 0))

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Testing",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "testing")],
        subtitle = "Additional diagnoses per year",
        width = NULL,
        fill = TRUE,
        icon = icon("user-md", lib = "font-awesome")
    )
})


output$vbOptim_linkage <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )

    out <- scales::comma(round(values[["linkage"]], digits = 0))

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Linkage",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "linkage")],
        subtitle = "Additional linkages per year",
        width = NULL,
        fill = TRUE,
        icon = icon("ambulance", lib = "font-awesome")
    )
})

output$vbOptim_preRetention <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )

    out <- abs(round(values[["preRetention"]], digits = 0))

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Pre-ART Retention",
        value = scales::comma(out),
        color = cols[which(names(values[rev(order(abs(values)))]) == "preRetention")],
        subtitle = "Reduction in losses from pre-ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

output$vbOptim_initiation <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )

    out <- scales::comma(round(values[["initiation"]], digits = 0))

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "ART Initiation",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "initiation")],
        subtitle = "Additional ART initiations per year",
        width = NULL,
        fill = TRUE,
        icon = icon("medkit", lib = "font-awesome")
    )
})

output$vbOptim_adherence <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )

    out <- scales::comma(round(values[["adherence"]], digits = 0))

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "Adherence",
        value = out,
        color = cols[which(names(values[rev(order(abs(values)))]) == "adherence")],
        subtitle = "Additional non-adherence transitions per year",
        width = NULL,
        fill = TRUE,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

output$vbOptim_retention <- renderInfoBox({
    # rely on a repeated press of NEX_optIntro button press
    input$NEXT_optIntro
    input$NEXT_opt909090

    # Subset data using opt_VS_cutoff
    selectedResults <- subset(optResults, optResults$VS >= (input$opt_VS_cutoff / 100))

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

    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )

    out <- abs(round(values[["retention"]], digits = 0))

    cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

    infoBox(
        title = "ART Retention",
        value = scales::comma(out),
        color = cols[which(names(values[rev(order(abs(values)))]) == "retention")],
        subtitle = "Reduction in losses from ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("heart-o", lib = "font-awesome")
    )
})

# output$vb909090_1 <- renderValueBox({
#     input$NEXT_optIntro

#     res <- Get909090(optResults)

#     out <- round(res[,"90"], digits = 2)

#     if (out >= 0.9) {
#         valueBox(
#             value = scales::percent(out),
#             subtitle = "Diagnosed",
#             color = "blue",
#             icon = icon("check", lib = "font-awesome")
#         )
#     } else {
#         valueBox(
#             value = scales::percent(out),
#             subtitle = "Diagnosed",
#             color = "blue",
#             icon = icon("times", lib = "font-awesome")
#         )
#     }
#   })

# output$vb909090_2 <- renderValueBox({
#     input$NEXT_optIntro

#     res <- Get909090(optResults)

#     out <- round(res[,"90-90"], digits = 2)

#     if (out >= 0.9) {
#         valueBox(
#             value = scales::percent(out),
#             subtitle = "On Treatment",
#             color = "blue",
#             icon = icon("check", lib = "font-awesome")
#         )
#     } else {
#         valueBox(
#             value = scales::percent(out),
#             subtitle = "On Treatment",
#             color = "blue",
#             icon = icon("times", lib = "font-awesome")
#         )
#     }
#   })

# output$vb909090_3 <- renderValueBox({
#     input$NEXT_optIntro

#     res <- Get909090(optResults)

#     out <- round(res[,"90-90-90"], digits = 2)

#     if (out >= 0.9) {
#         valueBox(
#             value = scales::percent(out),
#             subtitle = "Virally Suppressed",
#             color = "blue",
#             icon = icon("check", lib = "font-awesome")
#         )
#     } else {
#         valueBox(
#             value = scales::percent(out),
#             subtitle = "Virally Suppressed",
#             color = "blue",
#             icon = icon("times", lib = "font-awesome")
#         )
#     }
#   })

output$vb909090_COST <- renderValueBox({
    input$NEXT_optIntro

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    report_909090_cost <<- scales::dollar(mean(intResult[,"iCost"]))

    valueBox(
        value = scales::dollar(mean(intResult[,"iCost"])),
        subtitle = "Additional Cost of Care per year between 2015 and 2020",
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

    values <- colMeans(intResult[,names(intResult) != "iCost"])

    out <- scales::comma(round(values["iTest"], digits = 0))

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

    values <- colMeans(intResult[,names(intResult) != "iCost"])

    out <- scales::comma(round(values["iLink"], digits = 0))

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

    values <- colMeans(intResult[,names(intResult) != "iCost"])

    out <- scales::comma(abs(round(values["iPreR"], digits = 0)))

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

    values <- colMeans(intResult[,names(intResult) != "iCost"])

    out <- scales::comma(round(values["iInit"], digits = 0))

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

    values <- colMeans(intResult[,names(intResult) != "iCost"])

    out <- scales::comma(round(values[["iAdhr"]], digits = 0))

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

    values <- colMeans(intResult[,names(intResult) != "iCost"])

    out <- scales::comma(abs(round(values["iRetn"], digits = 0)))

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
