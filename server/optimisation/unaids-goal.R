# This function might need to measure the absolute difference here?
CalcDifTo90 <- function(result) {
    res_list <- matrix(0, dim(result)[1], 5)
    colnames(res_list) <- c("sim", "90", "90-90", "90-90-90", "total")
    for (i in 1:dim(result)[1]) {
        out <- rep(0, 5)
        out[1] <- i
        out[2] <- result[i,"90"]       - 0.9
        out[3] <- result[i,"90-90"]    - 0.9
        out[4] <- result[i,"90-90-90"] - 0.9
        out[5] <- sum(out[2], out[3], out[4])
        res_list[i,] <- out
    }
    return(res_list)
}

QuickRouteTo909090 <- function(result) {
    # order 90-90-90 by the sum of distance to 90%
    int <- result[order(-result[,"total"]),]

    # Identify simulation closest to the 90% mark.
    x <- 1
    while (int[[x, "90"]] > 0 & int[[x, "90-90"]] > 0 & int[[x, "90-90-90"]] > 0) {x <- x + 1}
    if (x == 1) {
        int[[x, "sim"]]
    } else {
        int[[x - 1, "sim"]]
    }
}

CanWeAchieve909090 <- function(result) {
    if (max(result[,"90"]) >= 0.9) {
        if (max(result[,"90-90"]) >= 0.9) {
            if (max(result[,"90-90-90"]) >= 0.9) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}

Get909090 <- function(result) {
    if (CanWeAchieve909090(result)) {
        message("Achieved 90-90-90 - showing cheapest route.")
        # Then subset the results that hit 90-90-90 and pass out the cheapest solution.
        first <- subset(result, result[,"90"] >= 0.9)
        second <- subset(first, first[,"90-90"] >= 0.9)
        third <- subset(second, second[,"90-90-90"] >= 0.9)

        # Return results of simulation (inc parameter values) for cheapest to achieve 90-90-90
        third[order(third[,"Cost"]),][1,]
    } else {
        message("Failed 90-90-90 - showing closest.")
        # Update ValueBoxes???
        res_list <- CalcDifTo90(result)
        result[QuickRouteTo909090(res_list),]
    }
}

output$vb909090_1 <- renderValueBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    out <- round(res[,"90"], digits = 2)

    if (out >= 0.9) {
        valueBox(
            value = scales::percent(out),
            subtitle = "Diagnosed",
            color = "green",
            icon = icon("check", lib = "font-awesome")
        )
    } else {
        valueBox(
            value = scales::percent(out),
            subtitle = "Diagnosed",
            color = "red",
            icon = icon("times", lib = "font-awesome")
        )
    }
  })

output$vb909090_2 <- renderValueBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    out <- round(res[,"90-90"], digits = 2)

    if (out >= 0.9) {
        valueBox(
            value = scales::percent(out),
            subtitle = "On Treatment",
            color = "green",
            icon = icon("check", lib = "font-awesome")
        )
    } else {
        valueBox(
            value = scales::percent(out),
            subtitle = "On Treatment",
            color = "red",
            icon = icon("times", lib = "font-awesome")
        )
    }
  })

output$vb909090_3 <- renderValueBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    out <- round(res[,"90-90-90"], digits = 2)

    if (out >= 0.9) {
        valueBox(
            value = scales::percent(out),
            subtitle = "Virally Suppressed",
            color = "green",
            icon = icon("check", lib = "font-awesome")
        )
    } else {
        valueBox(
            value = scales::percent(out),
            subtitle = "Virally Suppressed",
            color = "red",
            icon = icon("times", lib = "font-awesome")
        )
    }
  })

output$vb909090_COST <- renderValueBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    cost <- alt$TotalCost[251] - baseline$TotalCost[251]

    valueBox(
        value = scales::dollar(cost),
        subtitle = "Additional Cost of Care between 2015 and 2020",
        color = "green",
        icon = icon("usd", lib = "font-awesome")
    )
  })

output$vb909090_testing <- renderInfoBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    base_answer <- baseline$CumDiag[251] / 5
    alt_answer <- alt$CumDiag[251] / 5
    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    infoBox(
        title = "Testing",
        value = out,
        color = "orange",
        subtitle = "Additional diagnoses per year",
        width = NULL,
        fill = TRUE,
        icon = icon("user-md", lib = "font-awesome")
    )
})


output$vb909090_linkage <- renderInfoBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    base_answer <- baseline$CumLink[251] / 5
    alt_answer <- alt$CumLink[251] / 5
    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    infoBox(
        title = "Linkage",
        value = out,
        color = "orange",
        subtitle = "Additional linkages per year",
        width = NULL,
        fill = TRUE,
        icon = icon("ambulance", lib = "font-awesome")
    )
})

output$vb909090_preRetention <- renderInfoBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    base_answer <- baseline$CumPreL[251] / 5
    alt_answer <- alt$CumPreL[251] / 5
    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    infoBox(
        title = "Pre-ART Retention",
        value = out,
        color = "orange",
        subtitle = "Reduction in losses from pre-ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

output$vb909090_initiation <- renderInfoBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    base_answer <- baseline$CumInit[251] / 5
    alt_answer <- alt$CumInit[251] / 5
    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    infoBox(
        title = "ART Initiation",
        value = out,
        color = "orange",
        subtitle = "Additional ART initiations per year",
        width = NULL,
        fill = TRUE,
        icon = icon("medkit", lib = "font-awesome")
    )
})

output$vb909090_adherence <- renderInfoBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    base_answer <- baseline$CumAdhr[251] / 5
    alt_answer <- alt$CumAdhr[251] / 5
    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    infoBox(
        title = "Adherence",
        value = out,
        color = "orange",
        subtitle = "Additional non-adherence transitions per year",
        width = NULL,
        fill = TRUE,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

output$vb909090_retention <- renderInfoBox({
    input$NEXT_optIntro

    res <- Get909090(optResult)

    baseline <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun)

    alt <- CallBestModel(
        CalibOut = CalibOut,
        minErrorRun = minErrorRun,
        Rho = res[,"Rho"],
        q = res[,"Q"],
        Kappa = res[,"Kappa"],
        Gamma = res[,"Gamma"],
        Sigma = res[,"Sigma"],
        Omega = res[,"Omega"])

    base_answer <- baseline$CumLoss[251] / 5
    alt_answer <- alt$CumLoss[251] / 5
    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    infoBox(
        title = "ART Retention",
        value = out,
        color = "orange",
        subtitle = "Reduction in losses from ART care per year",
        width = NULL,
        fill = TRUE,
        icon = icon("heart-o", lib = "font-awesome")
    )
})
