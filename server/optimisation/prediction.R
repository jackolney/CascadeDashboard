# Intervention Detail Page
# Prediction of results from the model

# Testing Intervention
output$ib_testing_baseline <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Dx, baseline$Care, baseline$PreLtfu, baseline$ART, baseline$Ltfu))
    out <- scales::comma(round(sum(diff(answer)) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Diagnoses per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("user-md", lib = "font-awesome")
    )
})

output$ib_testing_intervention <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Dx, baseline$Care, baseline$PreLtfu, baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Rho = input$opt_rho_intValue)
    alt_answer <- rowSums(cbind(alt$Dx, alt$Care, alt$PreLtfu, alt$ART, alt$Ltfu))

    out <- scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0))

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

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))
    out <- scales::comma(round(sum(diff(answer)) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Linkages per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("ambulance", lib = "font-awesome")
    )
})

output$ib_linkage_intervention <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, q = input$opt_q_intValue)
    alt_answer <- rowSums(cbind(alt$Care, alt$ART, alt$Ltfu))

    out <- scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0))

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

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))
    out <- scales::comma(round(sum(diff(answer)) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Number of persons retained in pre-ART care per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

output$ib_preRetention_intervention <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Kappa = input$opt_kappa_intValue)
    alt_answer <- rowSums(cbind(alt$Care, alt$ART, alt$Ltfu))

    out <- scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional persons retained in pre-ART care per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("hospital-o", lib = "font-awesome")
    )
})

# ART Initiation Intervention
output$ib_initiation_baseline <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$ART, baseline$Ltfu))
    out <- scales::comma(round(sum(diff(answer)) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "ART initiations per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("medkit", lib = "font-awesome")
    )
})

output$ib_initiation_intervention <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Gamma = input$opt_gamma_intValue)
    alt_answer <- rowSums(cbind(alt$ART, alt$Ltfu))

    out <- scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional ART initiation per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("medkit", lib = "font-awesome")
    )
})

# Adherence Intervention
output$ib_adherence_baseline <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Vs))
    out <- scales::comma(round(sum(diff(answer)) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Number of persons adhering to ART per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

output$ib_adherence_intervention <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Vs))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Sigma = input$opt_sigma_intValue)
    alt_answer <- rowSums(cbind(alt$Vs))

    out <- scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional number of persons adhering to ART per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("heartbeat", lib = "font-awesome")
    )
})

# ART Retention Intervention
output$ib_retention_baseline <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Ltfu))
    out <- scales::comma(round(sum(diff(answer)) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Persons retained in ART care per year at baseline",
        color = "orange",
        width = NULL,
        icon = icon("heart-o", lib = "font-awesome")
    )
})

output$ib_retention_intervention <- renderValueBox({

    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Omega = input$opt_omega_intValue)
    alt_answer <- rowSums(cbind(alt$Ltfu))

    out <- scales::comma(round((sum(diff(base_answer)) - sum(diff(alt_answer))) / 5, digits = 0))

    valueBox(
        value = out,
        subtitle = "Additional persons retained in ART care per year with intervention",
        color = "green",
        width = NULL,
        icon = icon("heart-o", lib = "font-awesome")
    )
})
