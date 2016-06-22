# Script to contain error calculation functions
# Function to calculate error in a data.frame

# AssembleComparisonDataFrame for passing to Error function
AssembleComparisonDataFrame <- function(country, model, data) {

    # Create Model data.frame
    modelOutput <- data.frame()
    for (i in 1:7) {
        year <- model$time + 2010
        source <- "model"
        if (i == 1) {
            value <- model$N
            indicator <- "PLHIV"
        } else if (i == 2) {
            value <- model$Dx + model$Care + model$PreLtfu + model$Tx + model$Ltfu
            indicator <- "PLHIV Diagnosed"
        } else if (i == 3) {
            value <- model$Care + model$Tx
            indicator <- "PLHIV in Care"
        } else if (i == 4) {
            value <- model$Tx
            indicator <- "PLHIV on ART"
        } else if (i == 5) {
            value <- model$Vs
            indicator <- "PLHIV Suppressed"
        } else if (i == 6) {
            value <- model$PreLtfu
            indicator <- "PLHIV Pre-ART LTFU"
        } else if (i == 7) {
            value <- model$Ltfu
            indicator <- "PLHIV ART LTFU"
        }

        iOutput <- data.frame(country, indicator, source, year, value, weight = NA)
        modelOutput <- rbind(modelOutput, iOutput)
    }
    modelOutput

    # Create Data data.frame
    dataOutput <- data.frame()

    for (i in 1:5) {
        if (i == 1 & !dim(data[["calib"]][data[["calib"]]$indicator == "PLHIV",])[1] == 0) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV",]
            dOutput$source <- "data"
            dataOutput <- rbind(dataOutput, dOutput)
        } else if (i == 2 & !dim(data[["calib"]][data[["calib"]]$indicator == "PLHIV Diagnosed",])[1] == 0) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV Diagnosed",]
            dOutput$source <- "data"
            dataOutput <- rbind(dataOutput, dOutput)
        } else if (i == 3 & !dim(data[["calib"]][data[["calib"]]$indicator == "PLHIV in Care",])[1] == 0) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV in Care",]
            dOutput$source <- "data"
            dataOutput <- rbind(dataOutput, dOutput)
        } else if (i == 4 & !dim(data[["calib"]][data[["calib"]]$indicator == "PLHIV on ART",])[1] == 0) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV on ART",]
            dOutput$source <- "data"
            dataOutput <- rbind(dataOutput, dOutput)
        } else if (i == 5 & !dim(data[["calib"]][data[["calib"]]$indicator == "PLHIV on ART",])[1] == 0) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV Suppressed",]
            dOutput$source <- "data"
            dataOutput <- rbind(dataOutput, dOutput)
        }
    }
    dataOutput

    output <- rbind(dataOutput, modelOutput)
    output
}

# How does it get handled in SSE()???? skipped?

# This will need to be able to handle ALL FOUR ERRORS and return a neat data.frame of errors in return.
SSE <- function(df) {
    # if (!is.data.frame(df)) stop("Not passing a data frame.")
    uniqueIndicators <- unique(df$indicator)
    for (i in 1:length(uniqueIndicators)) {
        data <- df[df$indicator == uniqueIndicators[i],]

        uniqueYears <- unique(data$year)

        # Calculate number of values in dataset for particular indicator (N)
        N <- 0
        for (j in 1:length(uniqueYears)) {
            iD <- data[data$year == uniqueYears[j] & data$source == "data","value"]
            if (isEmpty(iD)) {
                next
            } else if (!is.na(iD)) {
                N <- N + 1
            }
        }

        for (j in 1:length(uniqueYears)) {
            iYr <- data[data$year == uniqueYears[j],]

            iData  <- iYr[iYr$source == "data","value"]
            if (isEmpty(iData)) next
            if (any(is.na(iData))) next
            if (length(iData) > 1) {
                iData <- mean(iData)
                warning("iData length > 1")
            }

            iWeight <- iYr[iYr$source == "data","weight"]
            if (isEmpty(iWeight)) next
            if (iWeight == "green") {
                w <- 1
            } else if (iWeight == "amber") {
                w <- 0.5
            } else if (iWeight == "red") {
                w <- 0.1
            }

            iModel <- iYr[iYr$source == "model","value"]

            value <- ((abs(iData - iModel) / iData) * w) / N

            year <- uniqueYears[j]

            iError <- data.frame(country = data$country[1], indicator = data$indicator[1], year, value, source = "error", weight = iWeight)

            df <- rbind(df, iError)
        }
    }
    df
}

isEmpty <- function(x) {
    return(length(x) == 0)
}
