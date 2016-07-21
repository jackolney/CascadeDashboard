# This file will source all the others and pull together a 'master data set' including:
#   - Previous data on the cascade (calibration-data.R)
#   - Assumptions (assumptions.R)
#   - Marrakech data (as the end target) (marrakech-data.R)

# source("server/calibration/calibration-data.R", local = FALSE)
# source("server/calibration/assumptions.R",      local = FALSE)
# source("server/calibration/marrakech-data.R",   local = FALSE)

# Then pull all this together into one 'master function'.

# THIS, is what the model will use as data and calculate error against.

# Everything will take an argument of 'uCountry' which will be 'userCountry'
# This will be called / generated once someone clicks on the map / picks from the dropdown menu

# This function will need to run some tests on the data.set to make sure that it is sensical.

# Set country
# userCountry <- "Zimbabwe"

GetMasterDataSet <- function(userCountry) {
    # Get all the data (all your base)
    countryData <- GetCountryData(userCountry)
    # countryAssumptions <- MakeAssumptions(userCountry, countryData)
    marrakechData <- GetMarrakechData(userCountry)

    # Filter out year of Marrakech data, before binding.
    # int <- rbind(countryData$calib, countryAssumptions)

    if (userCountry == "Kenya") {
        # For Kenya we only believe, the following three indicators supplied by the Marrakech Team
        mData <- marrakechData[marrakechData$indicator %in% c("PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"),]

        # This is all a bit Kenya-specific... we should really change that.
        # Now they need combining with the baseDataSet, but they should replace, not be an addition.
        # Extract everything that isn't 2015 data.
        int <- countryData$calib
        intOne <- int[int$year != 2015,]

        # Now extract the relevant bits of 2015
        intTwo <- int[int$year == 2015 & int$indicator %in% c("PLHIV", "PLHIV Diagnosed"),]

        # Combine together
        # MASTER DATA SET (for calibration)
        countryMasterDataSet <- rbind(intOne, intTwo, mData)
    } else if (userCountry == "Tanzania") {
        # This is usually just some blanket code that extract stuff from countryData$calib and prevents things from being overwritten in the background.
        # Not really necessary for Tanzania (as of now).

        # Now for some cheap hackery so that we can pick the smallest value of 'PLHIV on ART' in 2015 (from CTC) report.
        int <- countryData$calib

        intOne <- int[int$year != 2015,]

        intTemp <- int[int$year == 2015 & int$indicator == "PLHIV on ART",]
        intTwo <- intTemp[intTemp$value == min(intTemp$value),]

        intThree <- int[int$year == 2015 & int$indicator != "PLHIV on ART",]

        countryMasterDataSet <- rbind(intOne, intTwo, intThree)
    } else if (userCountry == "Zimbabwe") {
        # By hand, removing the indicators that get in the way of everything and cause trouble.
        # See issue #10 on GitHub for more details about my thinking on this.
        intOne <- marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV in Care", "PLHIV Suppressed"),]

        int <- countryData$calib
        intTwo <- int[int$indicator != "PLHIV not on ART",]

        countryMasterDataSet <- rbind(intOne, intTwo)
    } else {
        countryMasterDataSet <- countryData$calib
    }

    # Overwrite calib on countryData
    countryData$calib <- countryMasterDataSet[countryMasterDataSet$indicator != "PLHIV not on ART",]

    # Only allow certain countries to 'proceed', i.e. return 'countryData'
    # This will be removed eventually, but good for testing right now.
    if (userCountry %in% c("Kenya", "Tanzania", "Zimbabwe")) {
        countryData
    } else {
        stop("Country not approved for use by this model.")
    }
}

# GetMasterDataSet("Kenya")[["calib"]]

# Output testing
# test <- countryMasterDataSet[countryMasterDataSet$indicator != "PLHIV not on ART",]
# ggplot(test, aes(x = year, y = value)) + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")

# BLANK MASTER DATA SET
GetBlankMasterDataSet <- function(newName) {
    oldData <- GetMasterDataSet("Kenya")

    # Incidence
    oldData$incidence[,"country"] <- newName
    oldData$incidence[,as.character(seq(2010,2016,1))] <- NA

    # CD4
    oldData$cd4[,"country"] <- newName
    oldData$cd4[, c("prop.Off.ART.500",
        "prop.Off.ART.350500",
        "prop.Off.ART.250350",
        "prop.Off.ART.200250",
        "prop.Off.ART.100200",
        "prop.Off.ART.50100",
        "prop.Off.ART.50",
        "prop.On.ART.500",
        "prop.On.ART.350500",
        "prop.On.ART.250350",
        "prop.On.ART.200250",
        "prop.On.ART.100200",
        "prop.On.ART.50100",
        "prop.On.ART.50")
    ] <- NA

    # Treatment Guidelines
    oldData$treatment_guidelines[,"country"] <- newName
    oldData$treatment_guidelines[,c("less200", "less250", "less350", "less500", "more500")] <- NA

    # Calibration
    country <- newName

    indicator <- c(rep("PLHIV", 6),
        rep("PLHIV Diagnosed", 6),
        rep("PLHIV in Care", 6),
        rep("PLHIV on ART", 6),
        rep("PLHIV Suppressed", 6))

    year <- rep(seq(2010, 2015, 1), 5)

    value <- as.numeric(NA)
    weight <- NA

    oldData$calib <- data.frame(country, indicator, year, value, weight)

    # Rates (not used)
    oldData
}
