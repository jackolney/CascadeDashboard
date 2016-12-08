# Intervention Detail Page
# Prediction of results from the model

# Testing Intervention
output$ib_testing_baseline <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    out <- scales::comma(round(baseline$CumDiag[251] / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Diagnoses per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("user-md", lib = "font-awesome")
    )
})

output$ib_testing_intervention <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    base_answer <- baseline$CumDiag[251] / 5

    alt <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1, Rho = OptInput$intValue_rho)
    alt_answer <- alt$CumDiag[251] / 5

    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional diagnoses per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("user-md", lib = "font-awesome")
    )
})

# Linkage Intervention
output$ib_linkage_baseline <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    out <- scales::comma(round(baseline$CumLink[251] / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Linkages per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("ambulance", lib = "font-awesome")
    )
})

output$ib_linkage_intervention <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    base_answer <- baseline$CumLink[251] / 5

    alt <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1, q = OptInput$intValue_q)
    alt_answer <- alt$CumLink[251] / 5

    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional linkages per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("ambulance", lib = "font-awesome")
    )
})

# Pre-ART Retention Intervention
output$ib_preRetention_baseline <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    out <- scales::comma(round(baseline$CumPreL[251] / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Losses from pre-ART care per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

output$ib_preRetention_intervention <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    base_answer <- baseline$CumPreL[251] / 5

    alt <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1, Kappa = OptInput$intValue_kappa)
    alt_answer <- alt$CumPreL[251] / 5

    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    valueBox(
        value = out,
        subtitle = "Reduction in losses from pre-ART care per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

# ART Initiation Intervention
output$ib_initiation_baseline <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    out <- scales::comma(round(baseline$CumInit[251] / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "ART initiations per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("medkit", lib = "font-awesome")
    )
})

output$ib_initiation_intervention <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    base_answer <- baseline$CumInit[251] / 5

    alt <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1, Gamma = OptInput$intValue_gamma)
    alt_answer <- alt$CumInit[251] / 5

    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional ART initiations per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("medkit", lib = "font-awesome")
    )
})

# Adherence Intervention
output$ib_adherence_baseline <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    out <- scales::comma(round(baseline$CumAdhr[251] / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Non-adherent transitions to adherence per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

output$ib_adherence_intervention <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    base_answer <- baseline$CumAdhr[251] / 5

    alt <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1, Sigma = OptInput$intValue_sigma)
    alt_answer <- alt$CumAdhr[251] / 5

    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional non-adherence transitions to adherence per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

# ART Retention Intervention
output$ib_retention_baseline <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    out <- scales::comma(round(baseline$CumLoss[251] / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Losses from ART care per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("heart-o", lib = "font-awesome")
    )
})

output$ib_retention_intervention <- renderValueBox({

    baseline <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1)
    base_answer <- baseline$CumLoss[251] / 5

    alt <- CallBestFitModel(CalibOut = CalibOut, propRuns = 0.1, Omega = OptInput$intValue_omega)
    alt_answer <- alt$CumLoss[251] / 5

    out <- scales::comma(round(alt_answer - base_answer, digits = 0))

    valueBox(
        value = out,
        subtitle = "Reduction in losses from ART care per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("heart-o", lib = "font-awesome")
    )
})
