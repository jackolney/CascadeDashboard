# Intervention Detail Page
# Prediction of results from the model

# Testing Intervention
output$opt_rho_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Dx, baseline$Care, baseline$PreLtfu, baseline$ART, baseline$Ltfu))
    tags$code(scales::comma(round(sum(diff(answer)) / 5, digits = 0)))
})

output$opt_rho_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Dx, baseline$Care, baseline$PreLtfu, baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Rho = input$opt_rho_intValue)
    alt_answer <- rowSums(cbind(alt$Dx, alt$Care, alt$PreLtfu, alt$ART, alt$Ltfu))
    tags$code(scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0)))
})

# Linkage Intervention
output$opt_q_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))
    tags$code(scales::comma(round(sum(diff(answer)) / 5, digits = 0)))
})

output$opt_q_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, q = input$opt_q_intValue)
    alt_answer <- rowSums(cbind(alt$Care, alt$ART, alt$Ltfu))
    tags$code(scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0)))
})

# Pre-ART Retention Intervention
output$opt_kappa_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))
    tags$code(scales::comma(round(sum(diff(answer)) / 5, digits = 0)))
})

output$opt_kappa_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Care, baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Kappa = input$opt_kappa_intValue)
    alt_answer <- rowSums(cbind(alt$Care, alt$ART, alt$Ltfu))
    tags$code(scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0)))
})

# ART Initiation Intervention
output$opt_gamma_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$ART, baseline$Ltfu))
    tags$code(scales::comma(round(sum(diff(answer)) / 5, digits = 0)))
})

output$opt_gamma_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$ART, baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Gamma = input$opt_gamma_intValue)
    alt_answer <- rowSums(cbind(alt$ART, alt$Ltfu))
    tags$code(scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0)))
})

# Adherence Intervention
output$opt_sigma_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Vs))
    tags$code(scales::comma(round(sum(diff(answer)) / 5, digits = 0)))
})

output$opt_sigma_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Vs))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Sigma = input$opt_sigma_intValue)
    alt_answer <- rowSums(cbind(alt$Vs))
    tags$code(scales::comma(round((sum(diff(alt_answer)) - sum(diff(base_answer))) / 5, digits = 0)))
})

# ART Retention Intervention
output$opt_omega_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- rowSums(cbind(baseline$Ltfu))
    tags$code(scales::comma(round(sum(diff(answer)) / 5, digits = 0)))
})

output$opt_omega_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- rowSums(cbind(baseline$Ltfu))

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Omega = input$opt_omega_intValue)
    alt_answer <- rowSums(cbind(alt$Ltfu))
    tags$code(scales::comma(round((sum(diff(base_answer)) - sum(diff(alt_answer))) / 5, digits = 0)))
})
