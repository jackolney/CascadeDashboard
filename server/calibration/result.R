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
userParRange$rho         <- NA
userParRange$rho_MAX     <- NA
userParRange$rho_MIN     <- NA
userParRange$epsilon     <- NA
userParRange$epsilon_MAX <- NA
userParRange$epsilon_MIN <- NA
userParRange$kappa       <- NA
userParRange$kappa_MAX   <- NA
userParRange$kappa_MIN   <- NA
userParRange$gamma       <- NA
userParRange$gamma_MAX   <- NA
userParRange$gamma_MIN   <- NA
userParRange$theta       <- NA
userParRange$theta_MAX   <- NA
userParRange$theta_MIN   <- NA
userParRange$omega       <- NA
userParRange$omega_MAX   <- NA
userParRange$omega_MIN   <- NA
userParRange$p           <- NA
userParRange$p_MAX       <- NA
userParRange$p_MIN       <- NA
userParRange$q           <- NA
userParRange$q_MAX       <- NA
userParRange$q_MIN       <- NA
