# reactiveValues to hold calibration results
CalibParamMaxMin         <- reactiveValues()

# Maximums
CalibParamMaxMin$rho_MAX     <- 0
CalibParamMaxMin$epsilon_MAX <- 0
CalibParamMaxMin$kappa_MAX   <- 0
CalibParamMaxMin$gamma_MAX   <- 0
CalibParamMaxMin$theta_MAX   <- 0
CalibParamMaxMin$omega_MAX   <- 0
CalibParamMaxMin$p_MAX       <- 0
CalibParamMaxMin$q_MAX       <- 0

# Minimums
CalibParamMaxMin$rho_MIN     <- 0
CalibParamMaxMin$epsilon_MIN <- 0
CalibParamMaxMin$kappa_MIN   <- 0
CalibParamMaxMin$gamma_MIN   <- 0
CalibParamMaxMin$theta_MIN   <- 0
CalibParamMaxMin$omega_MIN   <- 0
CalibParamMaxMin$p_MIN       <- 0
CalibParamMaxMin$q_MIN       <- 0

# userParRange
userParRange         <- list()
userParRange$rho     <- NA
userParRange$epsilon <- NA
userParRange$kappa   <- NA
userParRange$gamma   <- NA
userParRange$theta   <- NA
userParRange$omega   <- NA
userParRange$p       <- NA
userParRange$q       <- NA
