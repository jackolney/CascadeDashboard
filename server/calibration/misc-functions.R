DefineParmRangeOLD <- function() {
    parRange <- data.frame(
        min = c(
            rho     = 0.00205,
            epsilon = 0.16949,
            kappa   = 0.01079,
            gamma   = 0.02556,
            theta   = 0.01511,
            omega   = 0.00033,
            p       = 0.00950,
            q       = 0.00500
        ),
        max = c(
            rho     = 1.025,
            epsilon = 84.745,
            kappa   = 5.395,
            gamma   = 12.780,
            theta   = 7.555,
            omega   = 0.165,
            p       = 1,
            q       = 1
        )
    )
    parRange
}

DefineParmRange <- function() {
    parRange <- data.frame(
        min = c(
            rho     = 0,
            epsilon = 0,
            kappa   = 0,
            gamma   = 0,
            theta   = 0,
            omega   = 0,
            p       = 0.1,
            q       = 0.1
        ),
        max = c(
            rho     = 2,
            epsilon = 100,
            kappa   = 1,
            gamma   = 10,
            theta   = 10,
            omega   = 0.5,
            p       = 1,
            q       = 1
        )
    )
    parRange
}

# DefineParmRange <- function() {
#     parRange <- data.frame(
#         min = c(
#             rho     = 0,
#             epsilon = 0,
#             kappa   = 0,
#             gamma   = 0,
#             theta   = 0,
#             omega   = 0,
#             p       = 0.6,
#             q       = 0.1
#         ),
#         max = c(
#             rho     = 1,
#             epsilon = 50,
#             kappa   = 1,
#             gamma   = 5,
#             theta   = 5,
#             omega   = 0.5,
#             p       = 1,
#             q       = 1
#         )
#     )
#     parRange
# }

DefineInitRange <- function(data, min, max) {
    # Take 2010 subset of data.
    i2010 <- data[["calib"]][data[["calib"]]$year == 2010,]

    # List all possible indicators
    allIndicators <- c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART")

    # Check if all values are present?
    indicatorPresence <- match(x = i2010$indicator, table = allIndicators)

    # Create missingIndicators data.frame
    missingIndicators <- data.frame()

    # Walk through allIndicators, identify missing indicators and search for the next closest value
    # Function walks up and down the cascade to identify the next closest value, even if it is not adjacent.
    for (x in 1:length(allIndicators)) {
        if (!any(indicatorPresence == x)) {
            name <- allIndicators[x]

            # Go back until you find a value.
            for (z in seq(x - 1, 1)) {
                if (any(indicatorPresence == z)) {
                    theMax <- i2010[i2010$indicator == allIndicators[z], "value"]
                    break
                }
            }

            # Go forward until you find a value.
            for (z in seq(x + 1, length(allIndicators))) {
                if (any(indicatorPresence == z)) {
                    theMin <- i2010[i2010$indicator == allIndicators[z], "value"]
                    break
                }
            }
            # Append missingIndicators data.frame
            missingIndicators <- rbind(missingIndicators, data.frame(name, theMax, theMin))
        }
    }

    # Fill out initRange, taking into account of whether i2010 holds the correct data.
    # If not, then use values from missingIndicators.
    # This should hold steady for ALL countries.
    initRange <- data.frame(
        min = c(
            plhiv =
                if (isEmpty(i2010[i2010$indicator == "PLHIV", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV", "theMin"] * min
                } else {
                    i2010[i2010$indicator == "PLHIV", "value"] * min
                },
            plhiv_diag =
                if (isEmpty(i2010[i2010$indicator == "PLHIV Diagnosed", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV Diagnosed", "theMin"] * min
                } else {
                    i2010[i2010$indicator == "PLHIV Diagnosed", "value"] * min
                },
            plhiv_care =
                if (isEmpty(i2010[i2010$indicator == "PLHIV in Care", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV in Care", "theMin"] * min
                } else {
                    i2010[i2010$indicator == "PLHIV in Care", "value"] * min
                },
            plhiv_art =
                if (isEmpty(i2010[i2010$indicator == "PLHIV on ART", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV on ART", "theMin"] * min
                } else {
                    i2010[i2010$indicator == "PLHIV on ART", "value"] * min
                }
            ),
        max = c(
            plhiv =
                if (isEmpty(i2010[i2010$indicator == "PLHIV", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV", "theMax"] * max
                } else {
                    i2010[i2010$indicator == "PLHIV", "value"] * max
                },
            plhiv_diag =
                if (isEmpty(i2010[i2010$indicator == "PLHIV Diagnosed", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV Diagnosed", "theMax"] * max
                } else {
                    i2010[i2010$indicator == "PLHIV Diagnosed", "value"] * max
                },
            plhiv_care =
                if (isEmpty(i2010[i2010$indicator == "PLHIV in Care", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV in Care", "theMax"] * max
                } else {
                    i2010[i2010$indicator == "PLHIV in Care", "value"] * max
                },
            plhiv_art =
                if (isEmpty(i2010[i2010$indicator == "PLHIV on ART", "value"])) {
                    missingIndicators[missingIndicators$name == "PLHIV on ART", "theMax"] * max
                } else {
                    i2010[i2010$indicator == "PLHIV on ART", "value"] * max
                }
            )
    )
    initRange
}

DefineIncidenceRange <- function(incidenceData) {
    parRange <- data.frame(
        min = c(
            yr2010 = as.double(incidenceData[incidenceData$type == "Lower", "2010"]),
            yr2011 = as.double(incidenceData[incidenceData$type == "Lower", "2011"]),
            yr2012 = as.double(incidenceData[incidenceData$type == "Lower", "2012"]),
            yr2013 = as.double(incidenceData[incidenceData$type == "Lower", "2013"]),
            yr2014 = as.double(incidenceData[incidenceData$type == "Lower", "2014"]),
            yr2015 = as.double(incidenceData[incidenceData$type == "Lower", "2015"]),
            yr2016 = as.double(incidenceData[incidenceData$type == "Lower", "2016"])
        ),
        max = c(
            yr2010 = as.double(incidenceData[incidenceData$type == "Upper", "2010"]),
            yr2011 = as.double(incidenceData[incidenceData$type == "Upper", "2011"]),
            yr2012 = as.double(incidenceData[incidenceData$type == "Upper", "2012"]),
            yr2013 = as.double(incidenceData[incidenceData$type == "Upper", "2013"]),
            yr2014 = as.double(incidenceData[incidenceData$type == "Upper", "2014"]),
            yr2015 = as.double(incidenceData[incidenceData$type == "Upper", "2015"]),
            yr2016 = as.double(incidenceData[incidenceData$type == "Upper", "2016"])
        )
    )
    parRange
}

FindSense <- function(samples) {

    # Create output matrix
    sensicalSamples <- matrix(data = 0, nrow = 0, ncol = 4)
    colnames(sensicalSamples) <- c("plhiv", "plhiv_diag", "plhiv_care", "plhiv_art")

    # Loop through each row of samples
    # Maybe translate to apply - later.
    for (l in 1:dim(samples)[1]) {
            test <- 0

        if (samples[[l,1]] - samples[[l,2]] >= 0) {
            test <- test + 1L
        }

        if (samples[[l,2]] - samples[[l,3]] >= 0) {
            test <- test + 1L
        }

        if (samples[[l,3]] - samples[[l,4]] >= 0) {
            test <- test + 1L
        }

        if (test == 3) {
            sensicalSamples <- rbind(sensicalSamples, samples[l,])
        }
    }
    sensicalSamples
}

AppendMinMaxMean <- function(data) {
    uniqueIndicators <- unique(data$indicator)
    uniqueYear <- unique(data$year)

    for (m in 1:length(uniqueIndicators)) {
        for (l in 1:length(uniqueYear)) {
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"min"]  <-  min(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"max"]  <-  max(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"mean"] <- mean(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
        }
    }
    data
}

AppendCI <- function(data) {
    uniqueIndicators <- unique(data$indicator)
    uniqueYear <- unique(data$year)

    for (m in 1:length(uniqueIndicators)) {
        for (l in 1:length(uniqueYear)) {
            CI <- Rmisc::CI(x = data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"], ci = 0.95)
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"lower"]  <- CI[["lower"]]
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"upper"]  <- CI[["upper"]]
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"mean"]   <- CI[["mean"]]
        }
    }
    data
}


FillParValues <- function(samples, positions, limit) {
    out <- data.frame(rho = 0, epsilon = 0, kappa = 0, gamma = 0, theta = 0, omega = 0, p = 0, q = 0)

    # Loop through all iterations and fill out data.frame
    for (l in 1:limit) {
        out[l,"rho"]     <- samples[,"rho"][positions[l]]
        out[l,"epsilon"] <- samples[,"epsilon"][positions[l]]
        out[l,"kappa"]   <- samples[,"kappa"][positions[l]]
        out[l,"gamma"]   <- samples[,"gamma"][positions[l]]
        out[l,"theta"]   <- samples[,"theta"][positions[l]]
        out[l,"omega"]   <- samples[,"omega"][positions[l]]
        out[l,"p"]       <- samples[,"p"][positions[l]]
        out[l,"q"]       <- samples[,"q"][positions[l]]
    }
    out
}

FillInitValues <- function(samples, positions, limit) {
    out <- data.frame(plhiv = 0, plhiv_diag = 0, plhiv_care = 0, plhiv_art = 0)

    # Loop through all iterations and fill out data.frame
    for (l in 1:limit) {
        out[l,"plhiv"]      <- samples[,"plhiv"][positions[l]]
        out[l,"plhiv_diag"] <- samples[,"plhiv_diag"][positions[l]]
        out[l,"plhiv_care"] <- samples[,"plhiv_care"][positions[l]]
        out[l,"plhiv_art"]  <- samples[,"plhiv_art"][positions[l]]
    }
    out
}

FillIncValue <- function(samples, positions, limit) {
    out <- data.frame(yr2010 = 0, yr2011 = 0, yr2012 = 0, yr2013 = 0, yr2014 = 0, yr2015 = 0, yr2016 = 0)

    # Loop through all iterations and fill out data.frame
    for(l in 1:limit) {
        out[l,"yr2010"] <- samples[,"yr2010"][positions[l]]
        out[l,"yr2011"] <- samples[,"yr2011"][positions[l]]
        out[l,"yr2012"] <- samples[,"yr2012"][positions[l]]
        out[l,"yr2013"] <- samples[,"yr2013"][positions[l]]
        out[l,"yr2014"] <- samples[,"yr2014"][positions[l]]
        out[l,"yr2015"] <- samples[,"yr2015"][positions[l]]
        out[l,"yr2016"] <- samples[,"yr2016"][positions[l]]
    }
    out
}

# This UserOverRide() is only for a fixed parameter.
# We need functionality to accept a range of values.
# An observeEvent on the MAX and MIN which then calls this function.
UserOverRide <- function(param) {
    if (!is.na(userParRange$rho) & userParRange$rho >= 0) {
        param[which(row.names(param) == "rho"),"min"]     <- userParRange$rho
        param[which(row.names(param) == "rho"),"max"]     <- userParRange$rho
    } else {
        if (!is.na(userParRange$rho_MAX) & userParRange$rho_MAX != param[which(row.names(param) == "rho"),"max"]) {
            param[which(row.names(param) == "rho"),"max"] <- userParRange$rho_MAX
        }
        if (!is.na(userParRange$rho_MIN) & userParRange$rho_MIN != param[which(row.names(param) == "rho"),"min"]) {
            param[which(row.names(param) == "rho"),"min"] <- userParRange$rho_MIN
        }
    }
    if (!is.na(userParRange$epsilon) & userParRange$epsilon >= 0) {
        param[which(row.names(param) == "epsilon"),"min"] <- userParRange$epsilon
        param[which(row.names(param) == "epsilon"),"max"] <- userParRange$epsilon
    } else {
        if (!is.na(userParRange$epsilon_MAX) & userParRange$epsilon_MAX != param[which(row.names(param) == "epsilon"),"max"]) {
            param[which(row.names(param) == "epsilon"),"max"] <- userParRange$epsilon_MAX
        }
        if (!is.na(userParRange$epsilon_MIN) & userParRange$epsilon_MIN != param[which(row.names(param) == "epsilon"),"min"]) {
            param[which(row.names(param) == "epsilon"),"min"] <- userParRange$epsilon_MIN
        }
    }
    if (!is.na(userParRange$kappa) & userParRange$kappa >= 0) {
        param[which(row.names(param) == "kappa"),"min"]   <- userParRange$kappa
        param[which(row.names(param) == "kappa"),"max"]   <- userParRange$kappa
    } else {
        if (!is.na(userParRange$kappa_MAX) & userParRange$kappa_MAX != param[which(row.names(param) == "kappa"),"max"]) {
            param[which(row.names(param) == "kappa"),"max"] <- userParRange$kappa_MAX
        }
        if (!is.na(userParRange$kappa_MIN) & userParRange$kappa_MIN != param[which(row.names(param) == "kappa"),"min"]) {
            param[which(row.names(param) == "kappa"),"min"] <- userParRange$kappa_MIN
        }
    }
    if (!is.na(userParRange$gamma) & userParRange$gamma >= 0) {
        param[which(row.names(param) == "gamma"),"min"]   <- userParRange$gamma
        param[which(row.names(param) == "gamma"),"max"]   <- userParRange$gamma
    } else {
        if (!is.na(userParRange$gamma_MAX) & userParRange$gamma_MAX != param[which(row.names(param) == "gamma"),"max"]) {
            param[which(row.names(param) == "gamma"),"max"] <- userParRange$gamma_MAX
        }
        if (!is.na(userParRange$gamma_MIN) & userParRange$gamma_MIN != param[which(row.names(param) == "gamma"),"min"]) {
            param[which(row.names(param) == "gamma"),"min"] <- userParRange$gamma_MIN
        }
    }
    if (!is.na(userParRange$theta) & userParRange$theta >= 0) {
        param[which(row.names(param) == "theta"),"min"]   <- userParRange$theta
        param[which(row.names(param) == "theta"),"max"]   <- userParRange$theta
    } else {
        if (!is.na(userParRange$theta_MAX) & userParRange$theta_MAX != param[which(row.names(param) == "theta"),"max"]) {
            param[which(row.names(param) == "theta"),"max"] <- userParRange$theta_MAX
        }
        if (!is.na(userParRange$theta_MIN) & userParRange$theta_MIN != param[which(row.names(param) == "theta"),"min"]) {
            param[which(row.names(param) == "theta"),"min"] <- userParRange$theta_MIN
        }
    }
    if (!is.na(userParRange$omega) & userParRange$omega >= 0) {
        param[which(row.names(param) == "omega"),"min"]   <- userParRange$omega
        param[which(row.names(param) == "omega"),"max"]   <- userParRange$omega
    } else {
        if (!is.na(userParRange$omega_MAX) & userParRange$omega_MAX != param[which(row.names(param) == "omega"),"max"]) {
            param[which(row.names(param) == "omega"),"max"] <- userParRange$omega_MAX
        }
        if (!is.na(userParRange$omega_MIN) & userParRange$omega_MIN != param[which(row.names(param) == "omega"),"min"]) {
            param[which(row.names(param) == "omega"),"min"] <- userParRange$omega_MIN
        }
    }
    if (!is.na(userParRange$p) & userParRange$p >= 0) {
        param[which(row.names(param) == "p"),"min"]       <- userParRange$p
        param[which(row.names(param) == "p"),"max"]       <- userParRange$p
    } else {
        if (!is.na(userParRange$p_MAX) & userParRange$p_MAX != param[which(row.names(param) == "p"),"max"]) {
            param[which(row.names(param) == "p"),"max"] <- userParRange$p_MAX
        }
        if (!is.na(userParRange$p_MIN) & userParRange$p_MIN != param[which(row.names(param) == "p"),"min"]) {
            param[which(row.names(param) == "p"),"min"] <- userParRange$p_MIN
        }
    }
    if (!is.na(userParRange$q) & userParRange$q >= 0) {
        param[which(row.names(param) == "q"),"min"]       <- userParRange$q
        param[which(row.names(param) == "q"),"max"]       <- userParRange$q
    } else {
        if (!is.na(userParRange$q_MAX) & userParRange$q_MAX != param[which(row.names(param) == "q"),"max"]) {
            param[which(row.names(param) == "q"),"max"] <- userParRange$q_MAX
        }
        if (!is.na(userParRange$q_MIN) & userParRange$q_MIN != param[which(row.names(param) == "q"),"min"]) {
            param[which(row.names(param) == "q"),"min"] <- userParRange$q_MIN
        }
    }
    param
}

FillInBlanks <- function(data, countryName, indicatorList) {
    for(i in 1:6) {
        yearElement <- seq(2010, 2015, 1)[i]
        for(j in 1:length(indicatorList)) {
            if (dim(data[data$year == yearElement & data$indicator == indicatorList[j],])[1] == 0) {
                country <- countryName
                indicator <- indicatorList[j]
                year <- yearElement
                value <- NA
                weight <- NA
                replacement <- data.frame(country, indicator, year, value, weight)
                data <- rbind(data, replacement)
            }
        }
    }
    data
}
