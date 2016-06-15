### Optimisation Input ###

output$UI_opt_rho_MAX <- renderUI({
    numericInput(inputId = "opt_rho_intValue", label = "Intervention Rate:",                       value = CalibParamMaxMin$rho_MAX,   min = 0, max = 10, step = 0.01, width = "100%")
})

output$UI_opt_q_MAX <- renderUI({
    numericInput(inputId = "opt_q_intValue", label = "Maximum Intervention Proportion:",           value = CalibParamMaxMin$q_MAX,     min = 0, max = 1, step = 0.001, width = "100%")
})

output$UI_opt_kappa_MAX <- renderUI({
    numericInput(inputId = "opt_kappa_intValue", label = "Intervention Rate (smaller is better):", value = CalibParamMaxMin$kappa_MIN, min = 0, max = 1, step = 0.001, width = "100%")
})

output$UI_opt_gamma_MAX <- renderUI({
    numericInput(inputId = "opt_gamma_intValue", label = "Intervention Rate:",                     value = CalibParamMaxMin$gamma_MAX, min = 0, max = 10, step = 0.01, width = "100%")
})

output$UI_opt_sigma_MAX <- renderUI({
    numericInput(inputId = "opt_sigma_intValue", label = "Intervention Rate (py^-1):",             value = 0.01,                       min = 0, max = 0.01, step = 0.01,  width = "100%")
})

output$UI_opt_omega_MAX <- renderUI({
    numericInput(inputId = "opt_omega_intValue", label = "Intervention Rate:",                     value = CalibParamMaxMin$omega_MAX, min = 0, max = 1, step = 0.001, width = "100%")
})
