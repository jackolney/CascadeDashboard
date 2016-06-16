# Optimisation Input Values - these are the intervention values that may or may not render in opt-parameter.R
OptInput <- reactiveValues()

# These observeEvent() update values but allow the opt-parameter.R page not to render but the same values be used by the model
observeEvent(input$opt_rho_intValue,   { OptInput$intValue_rho   <<- input$opt_rho_intValue   })
observeEvent(input$opt_q_intValue,     { OptInput$intValue_q     <<- input$opt_q_intValue     })
observeEvent(input$opt_kappa_intValue, { OptInput$intValue_kappa <<- input$opt_kappa_intValue })
observeEvent(input$opt_gamma_intValue, { OptInput$intValue_gamma <<- input$opt_gamma_intValue })
observeEvent(input$opt_sigma_intValue, { OptInput$intValue_sigma <<- input$opt_sigma_intValue })
observeEvent(input$opt_omega_intValue, { OptInput$intValue_omega <<- input$opt_omega_intValue })
