# Script to source all non-shiny components of the model

require(RColorBrewer)
devtools::load_all(pkg = "~/git/cascade")

source("server/calibration/assumptions.R",          local = FALSE)
source("server/calibration/calibration-data.R",     local = FALSE)
source("server/calibration/calibration.R",          local = FALSE)
source("server/calibration/error.R",                local = FALSE)
source("server/calibration/initial.R",              local = FALSE)
source("server/calibration/marrakech-data.R",       local = FALSE)
source("server/calibration/master.R",               local = FALSE)
source("server/calibration/misc-functions.R",       local = FALSE)
source("server/calibration/model.R",                local = FALSE)
source("server/calibration/plot-functions.R",       local = FALSE)
source("server/country/misc-functions.R",           local = FALSE)
source("server/misc-functions.R",                   local = FALSE)
source("server/misc-functions.R",                   local = FALSE)
source("server/model/baseline-model.R",             local = FALSE)
source("server/model/best-fit-model.R",             local = FALSE)
source("server/model/beta.R",                       local = FALSE)
source("server/model/initial.R",                    local = FALSE)
source("server/model/parameters.R",                 local = FALSE)
source("server/model/sim-abs.R",                    local = FALSE)
source("server/model/sim-prop.R",                   local = FALSE)
source("server/non-shiny/non-shiny-calibration.R",  local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R", local = FALSE)
source("server/non-shiny/thesis/thesis-figures.R",  local = FALSE)
source("server/optimisation/frontier.R",            local = FALSE)
source("server/optimisation/input-functions.R",     local = FALSE)
source("server/optimisation/output-functions.R",    local = FALSE)
source("server/optimisation/parameters.R",          local = FALSE)
source("server/optimisation/plot-functions.R",      local = FALSE)

AdjustHIVTestCost <- function() {
    if (reactiveAdjustCost$switch == TRUE) {
        message("AdjustCost == TRUE")
        if (exists("CalibOut")) {
            if (exists("MasterData")) {
                if (!is.na(MasterData$pop$value)) {
                    # pop value is not NA

                    # From calibration (CalibOut), calculate mean # 'PLHIV' in 2015
                    meanPLHIV <- mean(CalibOut[CalibOut$indicator == "PLHIV" & CalibOut$year == 2015 & CalibOut$source == "model", "value"])
                    # From calibration (CalibOut), calculate mean # 'PLHIV in Care' in 2015
                    meanCARE <- mean(CalibOut[CalibOut$indicator == "PLHIV in Care" & CalibOut$year == 2015 & CalibOut$source == "model", "value"])
                    # Calculate those persons Not In Care
                    NotInCare <- meanPLHIV - meanCARE
                    # Calculate the HIV-negative population size
                    Negative <- MasterData$pop$value - meanPLHIV

                    # Jeff's assumption
                    # HIV-negative persons are 0.75 times as likely to test as HIV-positive in general population
                    # Lancet GH Cost-Effectiveness Paper (suppl info page 10)
                    jeff <- 0.65

                    # Using the assumption that persons are tested randomly
                    CostFactor <- ((jeff * Negative) + NotInCare) / NotInCare
                    # print(paste("CostFactor =", CostFactor))
                    # Another way of thinking about this is as the:
                    # probability of testing a positive individual
                    # given the size of the undiagnosed (not in care) population
                    # 1/(NotInCare / ((jeff * Negative) + NotInCare))

                    # CAREFUL
                    # print(paste("OLD reactiveCost$test =", reactiveCost$test))
                    reactiveCost$test <<- CostFactor * SafeReactiveCost$test
                    # print(paste("NEW reactiveCost$test =", reactiveCost$test))

                } else {
                    # pop value is NA
                    # DEFAULT of FIVE
                    CostFactor <- 5
                    # print("DEFAULT")
                    # print(paste("OLD reactiveCost$test =", reactiveCost$test))
                    reactiveCost$test <<- CostFactor * SafeReactiveCost$test
                    # print(paste("NEW reactiveCost$test =", reactiveCost$test))

                }

            } else {
                warning("MasterData does not exist")
            }
        } else {
            warning("CalibOut is does not exist")
        }
    } else {
        message("AdjustCost == FALSE")
    }
}

# editing MasterData functions
new_data <- function(country, year, indicator, value, weight, source) {
    if (!is.character(country))   stop("country is not a character.")
    if (!is.numeric(year))        stop("year is not a character.")
    if (!is.character(indicator)) stop("indicator is not a character.")
    if (!is.numeric(value))       stop("value is not a character.")
    if (!is.character(weight))    stop("weight is not a character.")
    if (!is.character(source))    stop("soruce is not a character.")
    # Check if valid indicator
    indicator_list <- c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed")
    if (!any(indicator_list == indicator)) stop("Not a valid indicator type")
    new_dat <- data.frame(country, year, indicator, value, weight, source)
    new_dat
}

replace_or_append <- function(datOne, datTwo) {
    # find out if indicator for datTwo exists in datOne
    if (any(as.character(datOne[datOne$year == datTwo$year, "indicator"]) == as.character(datTwo$indicator))) {
        # REPLACE
        datOne[datOne$year == datTwo$year & datOne$indicator == as.character(datTwo$indicator), "value"]  <- as.numeric(datTwo$value)
        datOne[datOne$year == datTwo$year & datOne$indicator == as.character(datTwo$indicator), "weight"] <- as.character(datTwo$weight)
        datOne[datOne$year == datTwo$year & datOne$indicator == as.character(datTwo$indicator), "source"] <- as.character(datTwo$source)

    } else {
        # APPEND
        datOne <- rbind(datOne, datTwo)
    }
    datOne
}

reduce_incidence <- function(inc, weight) {

    # tease out values
    lower  <- as.double(inc[1,3:9])
    median <- as.double(inc[2,3:9])
    upper  <- as.double(inc[3,3:9])

    # multiply by weight
    inc[1,3:9] <- lower  * weight
    inc[2,3:9] <- median * weight
    inc[3,3:9] <- upper  * weight

    return(inc)
}

find_error_bound <- function(runError = runError, prop = 0.05) {
    # sort errors
    srtError <- sort(runError)

    # check
    check <- scales::percent(sum(runError <= srtError[round(length(srtError) * prop)]) / sum(runError != 0))
    message(paste(srtError[round(length(srtError) * prop)], "is", check))

    # return maxError bound
    return(srtError[round(length(srtError) * prop)])
}

BuildPHIAPlot <- function(data) {
    model_2015 <- data[data$source == "model" & data$year == 2015,]

    plhiv <- model_2015[model_2015$indicator == "PLHIV", "value"]
    diag  <- model_2015[model_2015$indicator == "PLHIV Diagnosed", "value"]
    art   <- model_2015[model_2015$indicator == "PLHIV on ART", "value"]
    supp  <- model_2015[model_2015$indicator == "PLHIV Suppressed", "value"]

    UN_90 <- Quantile_95(diag / plhiv)
    UN_9090 <- Quantile_95(art / diag)
    UN_909090 <- Quantile_95(supp / art)

    res <- c(UN_90[["mean"]], UN_9090[["mean"]], UN_909090[["mean"]])
    min <- c(UN_90[["lower"]], UN_9090[["lower"]], UN_909090[["lower"]])
    max <- c(UN_90[["upper"]], UN_9090[["upper"]], UN_909090[["upper"]])
    def <- c("Diagnosed / PLHIV","On Treatment / Diagnosed","Virally Suppressed / On Treatment")
    type <- "model"
    out <- data.frame(def, res, min, max, type)
    out$def <- factor(out$def, levels = c("Diagnosed / PLHIV","On Treatment / Diagnosed","Virally Suppressed / On Treatment"))
    out

    # Data
    val <- c(0.742, 0.868, 0.865)
    def <- c("Diagnosed / PLHIV","On Treatment / Diagnosed","Virally Suppressed / On Treatment")
    type <- "data"
    out_data <- data.frame(def, val, type)

    cfill <- rev(brewer.pal(9,"Blues")[6:8])

    ggOut <- ggplot(out, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + geom_point(data = out_data, mapping = aes(x = def, y = val), size = 5.5)
    ggOut <- ggOut + geom_point(data = out_data, mapping = aes(x = def, y = val), size = 5, color = ggColorHue(10)[4])
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
    ggOut <- ggOut + scale_fill_manual(values = cfill)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ggtitle("Zimbabwe PHIA Comparison (2015)")
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 15))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut
}

OutputBeta <- function() {
    beta_out <- c()

    # FOR EACH PARAMETER SET
    for (i in 1:dim(CalibParamOut)[1]) {

        p <- GetParameters(
            masterCD4 = MasterData$cd4_2015,
            data = MasterData,
            iterationParam = CalibParamOut[i,])

        # Now we need the initials.
        y <- GetInitial(
            p = p,
            iterationResult = CalibOut[CalibOut$year == 2015 & CalibOut$source == "model",][1:7 + 7 * (i - 1),],
            masterCD4 = MasterData$cd4_2015
            )

        beta_out[i] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[i,])
    }

    mean <- round(Quantile_95(beta_out)[["mean"]], 4)
    upper <- round(Quantile_95(beta_out)[["upper"]], 4)
    lower <- round(Quantile_95(beta_out)[["lower"]], 4)

    return(paste0(mean, " [", lower, " to ", upper, "]"))
}

message("Good to go...")
