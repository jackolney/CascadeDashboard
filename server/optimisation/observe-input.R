## Wizard Page Sliders ##
## Currently dummies ##
output$UI_optW_rho <- renderUI({
    sliderInput(inputId = 'optW_rho', label = 'Diagnosis rate (diagnoses/py) (rho):',
        min = round(ParamMaxMin["rho", "min"],        digits = 4),
        max = round(ParamMaxMin["rho", "max"],        digits = 4),
        value = round(mean(CalibParamOut[["rho"]]),   digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_p <- renderUI({
    sliderInput(inputId = 'optW_p', label = 'Proportion of diagnosed individuals linking to care (p):',
        min = round(ParamMaxMin["p", "min"],          digits = 4),
        max = round(ParamMaxMin["p", "max"],          digits = 4),
        value = round(mean(CalibParamOut[["p"]]),     digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_kappa <- renderUI({
    sliderInput(inputId = 'optW_kappa', label = 'Pre-ART Dropout Rate (Pre-ART dropout/py) (kappa):',
        min = round(ParamMaxMin["kappa", "min"],      digits = 4),
        max = round(ParamMaxMin["kappa", "max"],      digits = 4),
        value = round(mean(CalibParamOut[["kappa"]]), digits = 4),
        step = 0.0001,
        sep = "",
        width = "100%")
})

output$UI_optW_gamma <- renderUI({
    sliderInput(inputId = 'optW_gamma', label = 'ART initiation rate (ART initiations/py) (gamma):',
        min = round(ParamMaxMin["gamma", "min"],      digits = 4),
        max = round(ParamMaxMin["gamma", "max"],      digits = 4),
        value = round(mean(CalibParamOut[["gamma"]]), digits = 4),
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
        value = round(mean(CalibParamOut[["omega"]]), digits = 4),
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
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 4),
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 4) * 10),
        step = 0.0001,
        sep = "")
})

output$UI_optP_qRange <- renderUI({
    sliderInput(inputId = 'userOptq_Range', label = 'Proportion diagnosed that link to care:',
        min = 0,
        max = 1,
        value = c(
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["q"]], digits = 4),
            1),
        step = 0.0001,
        sep = "")
})

output$UI_optP_kappaRange <- renderUI({
    sliderInput(inputId = 'userOptKappa_Range', label = 'Loss from pre-ART care, per person-year:',
        min = 0,
        max = 50,
        value = c(
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4) / 10,
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4)),
        step = 0.0001,
        sep = "")
})

output$UI_optP_gammaRange <- renderUI({
    sliderInput(inputId = 'userOptGamma_Range', label = 'ART initiations per person-year:',
        min = 0,
        max = 50,
        value = c(
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4),
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4) * 10),
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
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4) / 10,
            round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4)),
        step = 0.0001,
        sep = "")
})
