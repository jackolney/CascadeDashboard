output$vb909090_COST_BASE <- renderValueBox({
    input$NEXT_optIntro

    # Baseline cost
    val <- Quantile_95(BaselineCost) / 5

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    vLower <- paste0("$", format(round(val['lower'] / 1e6, 2), trim = TRUE), "M")
    vMean  <- paste0("$", format(round(val['mean']  / 1e6, 2), trim = TRUE), "M")
    vUpper <- paste0("$", format(round(val['upper'] / 1e6, 2), trim = TRUE), "M")

    # out <- paste0(vMean, " [", vLower, " to ", vUpper, "]")
    out <- vMean

    report_909090_cost_BASE <<- paste0(vMean, " [", vLower, " to ", vUpper, "]")

    valueBox(
        value = out,
        subtitle = "Baseline cost of care per year between 2015 and 2020",
        color = "light-blue",
        icon = icon("usd", lib = "font-awesome")
    )
})

output$vb909090_testing_BASE <- renderInfoBox({
    input$NEXT_optIntro

    # Baseline diagnoses
    val <- round(Quantile_95(BaselineTest) / 5, digits = 0)

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    val <- scales::comma(val)

    out <- paste0(val[['mean']], " [", val[['lower']], " to ", val[['upper']], "]")

    report_909090_testing_BASE <<- out

    infoBox(
        title = "Diagnoses",
        value = out,
        color = "light-blue",
        subtitle = "Diagnoses per year at baseline",
        width = NULL,
        fill = TRUE,
        icon = icon("line-chart", lib = "font-awesome")
    )
})


output$vb909090_linkage_BASE <- renderInfoBox({
    input$NEXT_optIntro

    # Baseline linkage
    val <- round(Quantile_95(BaselineLink) / 5, digits = 0)

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    val <- scales::comma(val)

    out <- paste0(val[['mean']], " [", val[['lower']], " to ", val[['upper']], "]")

    report_909090_linkage_BASE <<- out

    infoBox(
        title = "Linkage",
        value = out,
        color = "light-blue",
        subtitle = "Linkages per year at baseline",
        width = NULL,
        fill = TRUE,
        icon = icon("line-chart", lib = "font-awesome")
    )
})

output$vb909090_preRetention_BASE <- renderInfoBox({
    input$NEXT_optIntro

    # Baseline pre-ART retention
    val <- round(Quantile_95(BaselinePreR) / 5, digits = 0)

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    val <- scales::comma(val)

    out <- paste0(val[['mean']], " [", val[['lower']], " to ", val[['upper']], "]")

    report_909090_preRetention_BASE <<- out

    infoBox(
        title = "Pre-ART Retention",
        value = out,
        color = "light-blue",
        subtitle = "Losses from pre-ART care per year at baseline",
        width = NULL,
        fill = TRUE,
        icon = icon("line-chart", lib = "font-awesome")
    )
})

output$vb909090_initiation_BASE <- renderInfoBox({
    input$NEXT_optIntro

    # Baseline initiation
    val <- round(Quantile_95(BaselineInit) / 5, digits = 0)

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    val <- scales::comma(val)

    out <- paste0(val[['mean']], " [", val[['lower']], " to ", val[['upper']], "]")

    report_909090_initiation_BASE <<- out

    infoBox(
        title = "ART Initiation",
        value = out,
        color = "light-blue",
        subtitle = "ART initiations per year at baseline",
        width = NULL,
        fill = TRUE,
        icon = icon("line-chart", lib = "font-awesome")
    )
})

output$vb909090_adherence_BASE <- renderInfoBox({
    input$NEXT_optIntro

    # Baseline adherence
    val <- round(Quantile_95(BaselineAdhr) / 5, digits = 0)

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    val <- scales::comma(val)

    out <- paste0(val[['mean']], " [", val[['lower']], " to ", val[['upper']], "]")

    report_909090_adherence_BASE <<- out

    infoBox(
        title = "Viral Suppression",
        value = out,
        color = "light-blue",
        subtitle = "Viral suppressions per year at baseline",
        width = NULL,
        fill = TRUE,
        icon = icon("line-chart", lib = "font-awesome")
    )
})

output$vb909090_retention_BASE <- renderInfoBox({
    input$NEXT_optIntro

    # Baseline retention
    val <- round(Quantile_95(BaselineRetn) / 5, digits = 0)

    if (val[['upper']] < 0) val[['upper']] <- 0
    if (val[['mean']]  < 0) val[['mean']]  <- 0
    if (val[['lower']] < 0) val[['lower']] <- 0

    val <- scales::comma(val)

    out <- paste0(val[['mean']], " [", val[['lower']], " to ", val[['upper']], "]")

    report_909090_retention_BASE <<- out

    infoBox(
        title = "ART Retention",
        value = out,
        color = "light-blue",
        subtitle = "Losses from ART care per year at baseline",
        width = NULL,
        fill = TRUE,
        icon = icon("line-chart", lib = "font-awesome")
    )
})
