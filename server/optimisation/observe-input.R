### Optimisation Input ###

output$UI_opt_rho_MAX <- renderUI({
    numericInput(inputId = "opt_rho_factor", label = "Intervention rate factor:", value = 10, min = 0, max = 100, step = 1, width = "100%")
})

output$UI_opt_q_MAX <- renderUI({
    numericInput(inputId = "opt_q_factor", label = "Maximum Intervention Proportion:", value = 1, min = 0, max = 1, step = 0.001, width = "100%")
})

output$UI_opt_kappa_MAX <- renderUI({
    numericInput(inputId = "opt_kappa_factor", label = "Intervention rate factor:", value = 10, min = 0, max = 100, step = 1, width = "100%")
})

output$UI_opt_gamma_MAX <- renderUI({
    numericInput(inputId = "opt_gamma_factor", label = "Intervention rate factor:", value = 10, min = 0, max = 100, step = 1, width = "100%")
})

output$UI_opt_sigma_MAX <- renderUI({
    numericInput(inputId = "opt_sigma_factor", label = "Intervention Rate (py^-1):", value = 10, min = 0, max = 10, step = 1, width = "100%")
})

output$UI_opt_omega_MAX <- renderUI({
    numericInput(inputId = "opt_omega_factor", label = "Intervention rate factor:", value = 10, min = 0, max = 100, step = 1, width = "100%"),
})
