# Diagnosis Rate = rho
output$calib_rho_max <- renderUI({
    numericInput("test_DiagRate_U","Upper:", value = round(CalibParamMaxMin$rho_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_rho_min <- renderUI({
    numericInput("test_DiagRate_L","Lower:", value = round(CalibParamMaxMin$rho_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_rho <- renderUI({
    numericInput("uCalib_rho","Specify exact rate:", value = NULL, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Linkage Proportion = q
output$calib_q_max <- renderUI({
    numericInput("test_LinkProp_U","Upper:", value = round(CalibParamMaxMin$q_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_q_min <- renderUI({
    numericInput("test_LinkProp_L","Lower:", value = round(CalibParamMaxMin$q_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_q <- renderUI({
    numericInput("uCalib_q","Specify exact proportion:", value = NULL, min = 0, max = 1, step = 1e-3, width = '100%')
})

# Linkage Rate = epsilon
output$calib_epsilon_max <- renderUI({
    numericInput("test_LinkRate_U","Upper:", value = round(CalibParamMaxMin$epsilon_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_epsilon_min <- renderUI({
    numericInput("test_LinkRate_L","Lower:", value = round(CalibParamMaxMin$epsilon_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_epsilon <- renderUI({
    numericInput("uCalib_epsilon","Specify exact rate:", value = NULL, min = 0, max = 1, step = 1e-3, width = '100%')
})

# ART Initiation Rate = gamma
output$calib_gamma_max <- renderUI({
    numericInput("test_ARTRate_U","Upper:", value = round(CalibParamMaxMin$gamma_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_gamma_min <- renderUI({
    numericInput("test_ARTRate_L","Lower:", value = round(CalibParamMaxMin$gamma_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_gamma <- renderUI({
    numericInput("uCalib_gamma","Specify exact rate:", value = NULL, min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Initiation Rate (Side Door) = theta
output$calib_theta_max <- renderUI({
    numericInput("test_ARTsideRate_U","Upper:", value = round(CalibParamMaxMin$theta_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_theta_min <- renderUI({
    numericInput("test_ARTsideRate_L","Lower:", value = round(CalibParamMaxMin$theta_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_theta <- renderUI({
    numericInput("uCalib_theta","Specify exact rate:", value = NULL, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Pre-ART Dropout Rate = kappa
output$calib_kappa_max <- renderUI({
    numericInput("test_PreARTDropRate_U","Upper:", value = round(CalibParamMaxMin$kappa_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_kappa_min <- renderUI({
    numericInput("test_PreARTDropRate_L","Lower:", value = round(CalibParamMaxMin$kappa_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_kappa <- renderUI({
    numericInput("uCalib_kappa","Specify exact rate:", value = NULL, min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Dropout Rate = omega
output$calib_omega_max <- renderUI({
    numericInput("test_ARTDropRate_U","Upper:", value = round(CalibParamMaxMin$omega_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_omega_min <- renderUI({
    numericInput("test_ARTDropRate_L","Lower:", value = round(CalibParamMaxMin$omega_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_omega <- renderUI({
    numericInput("uCalib_omega","Specify exact rate:", value = NULL, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Adherence Proportion = p
output$calib_p_max <- renderUI({
    numericInput("test_AdhProp_U","Upper:", value = round(CalibParamMaxMin$p_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_p_min <- renderUI({
    numericInput("test_AdhProp_L","Lower:", value = round(CalibParamMaxMin$p_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$UI_calib_p <- renderUI({
    numericInput("uCalib_p","Specify exact proportion:", value = NULL, min = 0, max = 1, step = 1e-3, width = '100%')
})
