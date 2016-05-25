## Wizard Page Sliders ##
## Currently dummies ##
output$UI_optW_rho <- renderUI({
    sliderInput(inputId = 'optW_rho', label = 'Diagnosis rate (diagnoses/py) (rho):',
        min = round(ParamMaxMin["rho", "min"],        digits = 4),
        max = round(ParamMaxMin["rho", "max"],        digits = 4),
        value = round(CalibParamOut[minErrorRun, "rho"], digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_p <- renderUI({
    sliderInput(inputId = 'optW_p', label = 'Proportion of diagnosed individuals linking to care (p):',
        min = round(ParamMaxMin["p", "min"],          digits = 4),
        max = round(ParamMaxMin["p", "max"],          digits = 4),
        value = round(CalibParamOut[minErrorRun, "p"], digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_kappa <- renderUI({
    sliderInput(inputId = 'optW_kappa', label = 'Pre-ART Dropout Rate (Pre-ART dropout/py) (kappa):',
        min = round(ParamMaxMin["kappa", "min"],      digits = 4),
        max = round(ParamMaxMin["kappa", "max"],      digits = 4),
        value = round(CalibParamOut[minErrorRun, "kappa"], digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_gamma <- renderUI({
    sliderInput(inputId = 'optW_gamma', label = 'ART initiation rate (ART initiations/py) (gamma):',
        min = round(ParamMaxMin["gamma", "min"],      digits = 4),
        max = round(ParamMaxMin["gamma", "max"],      digits = 4),
        value = round(CalibParamOut[minErrorRun, "gamma"], digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_sigma <- renderUI({
    sliderInput(inputId = 'optW_sigma', label = 'Adherence Rate (Adherence/py) (sigma):',
        min = 0,
        max = 1,
        value = 0,
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_omega <- renderUI({
    sliderInput(inputId = 'optW_omega', label = 'ART dropout rate (ART dropout/py) (omega):',
        min = round(ParamMaxMin["omega", "min"],      digits = 4),
        max = round(ParamMaxMin["omega", "max"],      digits = 4),
        value = round(CalibParamOut[minErrorRun, "omega"], digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})


## Parameter Page Sliders ##

output$UI_optP_rhoRange <- renderUI({
    sliderInput(inputId = 'userOptRho_Range', label = 'Diagnoses per person-year:',
        min = 0,
        max = 50,
        value = c(
            round(CalibParamOut[minErrorRun, "rho"], digits = 4),
            round(CalibParamOut[minErrorRun, "rho"], digits = 4) * 10),
        step = 0.0001,
        sep = "")
})

output$UI_optP_qRange <- renderUI({
    sliderInput(inputId = 'userOptq_Range', label = 'Proportion diagnosed that link to care:',
        min = 0,
        max = 1,
        value = c(
            round(CalibParamOut[minErrorRun, "q"], digits = 4),
            1),
        step = 0.0001,
        sep = "")
})

output$UI_optP_kappaRange <- renderUI({
    sliderInput(inputId = 'userOptKappa_Range', label = 'Loss from pre-ART care, per person-year:',
        min = 0,
        max = 50,
        value = c(
            round(CalibParamOut[minErrorRun, "kappa"], digits = 4) / 10,
            round(CalibParamOut[minErrorRun, "kappa"], digits = 4)),
        step = 0.0001,
        sep = "")
})

output$UI_optP_gammaRange <- renderUI({
    sliderInput(inputId = 'userOptGamma_Range', label = 'ART initiations per person-year:',
        min = 0,
        max = 50,
        value = c(
            round(CalibParamOut[minErrorRun, "gamma"], digits = 4),
            round(CalibParamOut[minErrorRun, "gamma"], digits = 4) * 10),
        step = 0.0001,
        sep = "")
})

output$UI_optP_sigmaRange <- renderUI({
    sliderInput(inputId = 'userOptSigma_Range', label = 'Rate at which non-adherent persons begin adhering to treatment, per person-year:',
        min = 0,
        max = 5,
        value = c(0, 5),
        step = 0.0001,
        sep = "")
})

output$UI_optP_omegaRange <- renderUI({
    sliderInput(inputId = 'userOptOmega_Range', label = 'ART dropout per person-year:',
        min = 0,
        max = 0.1,
        value = c(
            round(CalibParamOut[minErrorRun, "omega"], digits = 4) / 10,
            round(CalibParamOut[minErrorRun, "omega"], digits = 4)),
        step = 0.0001,
        sep = "")
})


### TESTING AREA ###

output$opt_rho_baseline <- renderUI({
    baseline <- CallBestModel()
    answer <- cumsum(baseline$Dx)[251] - baseline$Dx[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_rho_max <- renderUI({
    baseline <- CallBestModel()
    base_answer <- cumsum(baseline$Dx)[251] - baseline$Dx[1]

    alt <- CallBestModel(Rho = CalibParamOut[minErrorRun, "rho"] * input$opt_rho_factor)
    alt_answer <- cumsum(alt$Dx)[251] - alt$Dx[1]
    tags$code(scales::comma(round(alt_answer - base_answer, digits = 0)))
})
