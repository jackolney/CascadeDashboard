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

        # New Data From Croatia (Global HIV Cascade Workshop 2016)
        # new_care  <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV in Care",    value = 903011,  weight = "amber", source = "Program Data (adjusted ART data)")
        # new_art   <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV on ART",     value = 879271,  weight = "amber", source = "Program Data (DHIS)")
        # new_supp  <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV Suppressed", value = 782551,  weight = "red",   source = "Surveillance")

        # countryMasterDataSet <- replace_or_append(datOne = countryMasterDataSet, datTwo = new_care)
        # countryMasterDataSet <- replace_or_append(datOne = countryMasterDataSet, datTwo = new_art)
        # countryMasterDataSet <- replace_or_append(datOne = countryMasterDataSet, datTwo = new_supp)

        # New Data from PHIA
        PHIA_dx <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV Diagnosed",    value = 1057915, weight = "green", source = "PHIA")
        PHIA_tx <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV on ART",       value = 918271,  weight = "green", source = "PHIA")
        PHIA_vs <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV Suppressed",   value = 794304,  weight = "green", source = "PHIA")

        countryMasterDataSet <- replace_or_append(datOne = countryMasterDataSet, datTwo = PHIA_dx)
        countryMasterDataSet <- replace_or_append(datOne = countryMasterDataSet, datTwo = PHIA_tx)
        countryMasterDataSet <- replace_or_append(datOne = countryMasterDataSet, datTwo = PHIA_vs)

    } else if (userCountry == "Brazil") {
        # copy in the countryData
        int <- countryData$calib
        # Remove stuff from Jaun's thesis (superceded by latest Spectrum)
        noDST <- int[int$source != "Data from Department of DST",]
        # Remove other UNAIDS data
        noOther <- noDST[noDST$source != "http://www.unaids.org/sites/default/files/en/dataanalysis/knowyourresponse/countryprogressreports/2014countries/BRA_narrative_report_2014.pdf",]
        # Ignore Spectrum stuff in 2013
        not2013 <- noOther[noOther$year != 2013,]
        # Get the 2013 Spectrum PLHIV estimate
        plhiv_2013 <- noOther[noOther$year == 2013 & noOther$indicator == "PLHIV",]
        # Pull in MarrakechData
        intOne <- marrakechData
        # Use all except the PLHIV estimate
        intTwo <- intOne[intOne$indicator != "PLHIV",]
        # Bind together
        countryMasterDataSet <- rbind(not2013, intTwo, plhiv_2013)
    } else if (userCountry == "Cambodia") {
        int <- countryData$calib
        intOne <- marrakechData
        # Override, 2013 PLHIV on ART value & pull in Marrakech data and use to overwrite
        intTwo <- int[int$year == 2013,][2:4,]
        countryMasterDataSet <- rbind(int[int$year != 2013 & int$year != 2014,], intTwo, intOne)
    } else if (userCountry == "DRC") {
        int <- countryData$calib
        # Ignore the high PLHIV diagnosed value
        value <- rbind(int[int$year == 2013 & int$indicator != "PLHIV Diagnosed",], int[int$year == 2013 & int$indicator == "PLHIV Diagnosed",][2,])
        countryMasterDataSet <- rbind(int[int$year != 2013 & int$year != 2014,], value, marrakechData)
    } else if (userCountry == "India") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(int, marrakechData)
    } else if (userCountry == "Myanmar") {
        int <- countryData$calib
        # Ignore the values that we pulled in from 'aidsdatahub' (we weighted all these as amber in any case)
        intOne <- int[int$weight != "amber",]
        countryMasterDataSet <- rbind(intOne[intOne$year != 2014,], marrakechData)
    } else if (userCountry == "Pakistan") {
        int <- countryData$calib
        # Ignore the values that we pulled in from 'aidsdatahub' (we weighted all these as amber in any case)
        intOne <- int[int$weight != "amber",]
        # Ignoring most of the data from Pakistan as very little info was supplied on it (but will use the estimate of PLHIV in Care)
        countryMasterDataSet <- rbind(intOne, marrakechData[marrakechData$indicator == "PLHIV in Care",])
    } else if (userCountry == "Thailand") {
        int <- countryData$calib
        # Ignore the values that we pulled in from 'aidsdatahub' (we weighted all these as amber in any case)
        intOne <- int[int$weight != "amber",]
        countryMasterDataSet <- rbind(marrakechData, intOne[intOne$year != 2014,])
    } else if (userCountry == "Vietnam") {
        # Pull in countryData
        int <- countryData$calib
        # Ignore some of the values that we pulled from other sources
        plhiv <- int[int$indicator == "PLHIV" & int$source == "Spectrum",]
        diag <- int[int$indicator == "PLHIV Diagnosed",]
        care <- int[int$indicator == "PLHIV In Care",]
        art <- int[int$indicator == "PLHIV on ART" & int$source == "Spectrum",]
        data <- rbind(plhiv, diag, care, art)
        # merge and ignore 2014 data as we rely on Marrakech estimates for that.
        countryMasterDataSet <- rbind(data[data$year != 2014,], marrakechData)
    } else if (userCountry == "Cameroon") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(int[int$year != 2014,], marrakechData)
    } else if (userCountry == "Cote d'Ivoire") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(int[int$year != 2014,], marrakechData)
    } else if (userCountry == "Ethiopia") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(int[int$year != 2014,], marrakechData)
    } else if (userCountry == "Haiti") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV in Care", "PLHIV Suppressed"),], int)
    } else if (userCountry == "Indonesia") {
        # Pull in countryData
        int <- countryData$calib
        # Ignore anything that came from AIDS datahub
        intOne <- int[int$source != "http://www.aidsdatahub.org/Country-Profiles/Indonesia",]
        # marrakechData is just for 2014 and covers all the indicators
        countryMasterDataSet <- rbind(rbind(intOne[intOne$year != 2014,], marrakechData))
    } else if (userCountry == "Malawi") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV in Care", "PLHIV Suppressed"),], int)
    } else if (userCountry == "Morocco") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV in Care", "PLHIV Suppressed"),], int)
    } else if (userCountry == "Mozambique") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV in Care", "PLHIV Suppressed"),], int)
    } else if (userCountry == "Nigeria") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV in Care"),], int)
    } else if (userCountry == "Philippines") {
        # Pull in countryData
        int <- countryData$calib
        # Ignore anything that came from AIDS datahub
        intOne <- int[int$source != "http://www.aidsdatahub.org/Country-Profiles/Philippines",]
        # marrakechData is just for 2015 and covers all the indicators
        countryMasterDataSet <- rbind(rbind(intOne[intOne$year != 2015,], marrakechData))
    } else if (userCountry == "Russia") {
        int <- countryData$calib
        intOne <- int[int$year == 2015 & int$indicator %in% c("PLHIV", "PLHIV Suppressed"),]
        countryMasterDataSet <- rbind(intOne, marrakechData, int[int$year != 2015,])
    } else if (userCountry == "South Africa") {

        #THEMBISA
        # Just overwrite Spectrum estimates here in:
            # $calib (yes)
            # $pop (yes)
            # $incidence (yes, need 95% CI)

        # Draft code
        thembisa.cascade    <- data.frame(readr::read_csv("server/data/calibration/thembisa-cascade.csv",    col_names = TRUE, skip = 0), stringsAsFactors = FALSE)
        thembisa.incidence  <- data.frame(readr::read_csv("server/data/calibration/thembisa-incidence.csv",  col_names = TRUE, skip = 1), stringsAsFactors = FALSE)
        thembisa.population <- data.frame(readr::read_csv("server/data/calibration/thembisa-population.csv", col_names = TRUE, skip = 0), stringsAsFactors = FALSE)

        # countryData$calib overload
        int <- countryData$calib
        t1 <- thembisa.cascade
        t2 <- int[int$source != "Spectrum",]
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV Suppressed"),], t1, t2)

        # countryData$incidence
        inc <- countryData$incidence
        inc[1,3:9] <- thembisa.incidence[1,3:9]
        inc[2,3:9] <- thembisa.incidence[2,3:9]
        inc[3,3:9] <- thembisa.incidence[3,3:9]
        countryData$incidence <- inc

        # countryData$pop
        countryData$pop[["value"]] <- thembisa.population[["value"]]

        # Then overload the Spectrum estimates here (allow me to be able to comment out this section and return to the SPECTRUM estimates)

        # SPECTRUM
        # (BLANKED OUT WHILE WE USE THEMBISA)
        # int <- countryData$calib
        # countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV Suppressed"),], int)


    } else if (userCountry == "South Sudan") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(marrakechData[marrakechData$indicator %in% c("PLHIV Diagnosed", "PLHIV in Care"),], int)
    } else if (userCountry == "Uganda") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(int[int$year != 2014,], marrakechData)
    } else if (userCountry == "Ukraine") {
        int <- countryData$calib
        countryMasterDataSet <- rbind(int[int$year != 2015,], marrakechData)
    } else {
        countryMasterDataSet <- countryData$calib
    }

    # Overwrite calib on countryData
    countryData$calib <- countryMasterDataSet[countryMasterDataSet$indicator != "PLHIV not on ART",]

    # Only allow certain countries to 'proceed', i.e. return 'countryData'
    # This will be removed eventually, but good for testing right now.
    # if (userCountry %in% c("Kenya", "Tanzania", "Zimbabwe")) {
    #     countryData
    # } else {
    #     stop("Country not approved for use by this model.")
    # }
    countryData
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
    oldData$incidence[,as.character(seq(2010,2016,1))] <- as.numeric(NA)

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

    # CD4 2015
    oldData$cd4_2015[,"country"] <- newName
    oldData$cd4_2015[, c("prop.Off.ART.500",
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
    oldData$treatment_guidelines[,c("less200", "less250", "less350", "less500", "more500")] <- as.numeric(NA)

    # Population
    oldData$pop[,"country"] <- newName
    oldData$pop[,"value"] <- as.numeric(NA)

    # Calibration
    country <- newName

    indicator <- c(rep("PLHIV", 6),
        rep("PLHIV Diagnosed", 6),
        rep("PLHIV in Care", 6),
        rep("PLHIV on ART", 6),
        rep("PLHIV Suppressed", 6))

    year <- rep(seq(2010, 2015, 1), 5)

    value <- as.numeric(NA)
    weight <- factor(x = NA, levels = c("red", "amber", "green"))
    source <- as.character(NA)

    oldData$calib <- data.frame(country, indicator, year, value, weight, source, stringsAsFactors = FALSE)

    # Rates (not used)
    oldData
}
