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
        } else if (i == 5 & !dim(data[["calib"]][data[["calib"]]$indicator == "PLHIV Suppressed",])[1] == 0) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV Suppressed",]
            dOutput$source <- "data"
            dataOutput <- rbind(dataOutput, dOutput)
        }
    }
    dataOutput

    output <- rbind(dataOutput, modelOutput)
    output
}

# This will need to be able to handle ALL FOUR ERRORS and return a neat data.frame of errors in return.
SSE <- function(df) {
    # Calculate total number of data values in dataset
    N <- dim(df[df$source == "data",])[1]

    # if (!is.data.frame(df)) stop("Not passing a data frame.")
    uniqueIndicators <- unique(df$indicator)
    for (i in 1:length(uniqueIndicators)) {
        data <- df[df$indicator == uniqueIndicators[i],]

        uniqueYears <- unique(data$year)

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

            # PHIA ADDITIONS
            # To remove, simply comment this if statment out and the model will return to
            # previous state
            # for collecting results, maybe just write to some data.frame somewhere
            if (unique(iYr$country) == "Zimbabwe" & unique(iYr$year) == 2015) {

                if (unique(iYr$indicator) == "PLHIV Diagnosed") {
                    # PLHIV DIAGNOSED
                    model_num <- iYr[iYr$source == "model", "value"]
                    model_den <- df[df$year == 2015 & df$indicator == "PLHIV" & df$source == "model", "value"]
                    iModel <- model_num / model_den

                    # PHIA - 74.2% of PLHIV were diagnosed
                    iData <- 0.742

                    value <- ((abs(iData - iModel) / iData) * w) / N

                } else if (unique(iYr$indicator) == "PLHIV on ART") {
                    # PLHIV on ART
                    model_num <- iYr[iYr$source == "model", "value"]
                    modle_den <- df[df$year == 2015 & df$indicator == "PLHIV Diagnosed" & df$source == "model", "value"]
                    iModel <- model_num / model_den

                    # PHIA - 86.8% of Diagnosed are on ART
                    iData <- 0.868

                    value <- ((abs(iData - iModel) / iData) * w) / N

                } else if (unique(iYr$indicator) == "PLHIV Suppressed") {
                    # PLHIV Suppressed
                    model_num <- iYr[iYr$source == "model", "value"]
                    model_den <- df[df$year == 2015 & df$indicator == "PLHIV on ART" & df$source == "model", "value"]
                    iModel <- model_num / model_den

                    # PHIA - 86.5% of on ART are virally suppressed
                    iData <- 0.865

                    value <- ((abs(iData - iModel) / iData) * w) / N

                } else {
                    value <- ((abs(iData - iModel) / iData) * w) / N
                }

            } else {
                value <- ((abs(iData - iModel) / iData) * w) / N
            }

            # value <- ((abs(iData - iModel) / iData) * w) / N

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
