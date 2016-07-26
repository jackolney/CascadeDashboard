# require(readr)
# rm(list=ls())
# setwd("~/git/WhoCascade/CascadeApp/CascadeDashboard")
# dir()

# Setup Page -> Multiple Tabs
# Location Tab -> Please Select Country (displays a map of available countries (interactive [plotGoogleMap package])).
# 1) Pick Country.
# on next page the model will pull in all data from Marrakech, colour code boxes according to the weights assigned to them ('traffic lights').
# User can adjust 'marrakech' values and relative weights.
# clicking 'next' will run the calibration (visualise this?, or at least the final plot -> fun shit load of curves on the screen).
# Then 'parameter' page will be entered, showing the "ghost values" that the model has decided upon (in grey or something).
# The user can then leave them, as is, OR alter them (re-run calibration if so?).
# Then proceed to view results -> presented as an average perhaps, or a range (tricky).


# This 'calibration-data.R' script will include functions that are called during calibration.
# When 'country' is picked, pull out relevant data from all the data/calibration and data/marrakech.csv files.
# Assemble dynamically into a calibration-data DATA.FRAME using a standard format; i.e.
# data.frame(<year>,<country>,<category>,<value>,<source>,<details>)
# Need to specify the initial values in the model?????
#       - Values for state variables in 2010?
#       - CD4 distribution (SPECTRUM -- Just update script for it) [could even update it every year]
# Then run the model, check SSE between model and data, optim to minimise (weighting how?)
# Algorithm for doing this multiple times?
# (Store results for each simulation?)
# Plot results?
# Evaluate transition rates that the model decied upon.

# Model runs between 2010 and 2015.
# uses marrakech data to specify state of care (targets) in either 2015/2014/2013.
# fixed parameter values between those times.
# Spectrum tells us incidence
# Need to account for changing treatment guidelines in this period (have csv of dates, just need to configure it).

# Country Selection
# userCountry = "Kenya"

# What folders do I need to search through?

# calib.files.path <- c()
# for (i in 1:length(calib.files)) {
#     print(i)
#     calib.files.path[i] <- paste0("server/data/calibration/", calib.files[i])
# }

# For all files in calib.files.path, do readr and identify all those with Kenya
# readr::read_csv
# a <- readr::read_csv(calib.files.path[1])
# dplyr::filter(a, Country == "Kenya")

# Lets try that again.
# Read all csv data in at applicataion start, then we can just jump between countries as and when we select them.
# c.file.path <- "server/data/calibration"
# calib.incidence            <- readr::read_csv(paste0(c.file.path, "/incident-infections.csv"),   col_names = TRUE, skip = 1)
# calib.cd4                  <- readr::read_csv(paste0(c.file.path, "/cd4-distribution-2010.csv"), col_names = TRUE, skip = 0)
# calib.art                  <- readr::read_csv(paste0(c.file.path, "/art.csv"),                   col_names = TRUE, skip = 1)
# calib.hiv_awareness_unaids <- readr::read_csv(paste0(c.file.path, "/hiv-awareness-unaids.csv"),  col_names = TRUE, skip = 0)
# calib.not_on_art           <- readr::read_csv(paste0(c.file.path, "/not-on-art.csv"),            col_names = TRUE, skip = 1)
# calib.plhiv                <- readr::read_csv(paste0(c.file.path, "/plhiv.csv"),                 col_names = TRUE, skip = 1)
# calib.previous_data        <- readr::read_csv(paste0(c.file.path, "/previous-data.csv"),         col_names = TRUE, skip = 0)
# calib.rates                <- readr::read_csv(paste0(c.file.path, "/rates.csv"),                 col_names = TRUE, skip = 0)
# calib.treatment_guidelines <- readr::read_csv(paste0(c.file.path, "/treatment-guidelines.csv"),  col_names = TRUE, skip = 0)

# For each find userCountry
# userCountry
# calib.incidence[[userCountry]]

# something along the lines of:

# GetCountryData <- function(uCountry) {
    # This will then return a data.frame containing ALL available data.

    # But how do we handle counties with values missing?
    # Return a list of three things?
    #   [1] - Incidence and CD4 (the ESSENTIALS)
    #   [2] - Additional data points (varied width)
    #   [3] - Treatment Guideline changes for the specific country
    #   [4] - Rates (if available) [only will be possible for like 3 countries who supplied rates in Marrakech]
    # Think we need a standard format here??
# }

# this will replace which.exist()
# build.master.df <- function(temp.names, calib.df) {
#     out <- c()
#     for (i in 1:length(temp.names)) {
#         if (exists(temp.names[i])) {
#             out <- rbind(out, get(temp.names[i]))
#         }
#     }

#     out.list <- list(
#         calib.df$incidence,
#         calib.df$cd4,
#         calib.df$treatment_guidelines,
#         out,
#         calib.df$rates
#         )

#     names(out.list) <- c(
#         "incidence",
#         "cd4",
#         "treatment_guidelines",
#         "calib",
#         "rates"
#         )

#     out.list
# }

is.not.empty <- function(ListElement) {
    if (!is.list(ListElement)) stop("not a list")
    if (dim(ListElement)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}


# Wrap in a function then question the data.
# uCountry = "China"

GetCountryData <- function(uCountry) {
    # Read in all relevant csv files
    c.file.path <- "server/data/calibration"
    calib.incidence            <- readr::read_csv(paste0(c.file.path, "/incidence-uncertainty.csv"),    col_names = TRUE, skip = 1)
    calib.cd4                  <- readr::read_csv(paste0(c.file.path, "/cd4-distribution-2010.csv"),    col_names = TRUE, skip = 0)
    calib.art                  <- readr::read_csv(paste0(c.file.path, "/art.csv"),                      col_names = TRUE, skip = 1)
    calib.hiv_awareness_unaids <- readr::read_csv(paste0(c.file.path, "/hiv-awareness-unaids.csv"),     col_names = TRUE, skip = 0)
    calib.not_on_art           <- readr::read_csv(paste0(c.file.path, "/not-on-art.csv"),               col_names = TRUE, skip = 1)
    calib.plhiv                <- readr::read_csv(paste0(c.file.path, "/plhiv.csv"),                    col_names = TRUE, skip = 1)
    calib.previous_data        <- readr::read_csv(paste0(c.file.path, "/previous-data.csv"),            col_names = TRUE, skip = 0)
    calib.rates                <- readr::read_csv(paste0(c.file.path, "/rates.csv"),                    col_names = TRUE, skip = 0)
    calib.treatment_guidelines <- readr::read_csv(paste0(c.file.path, "/treatment-guidelines-cd4.csv"), col_names = TRUE, skip = 0)

    # Compile data.frame
    calib.df <- list()
    calib.df[[1]] <- dplyr::filter(calib.incidence,             country == uCountry)
    calib.df[[2]] <- dplyr::filter(calib.cd4,                   country == uCountry)
    calib.df[[3]] <- dplyr::filter(calib.treatment_guidelines,  country == uCountry)
    calib.df[[4]] <- dplyr::filter(calib.art,                   country == uCountry)
    calib.df[[5]] <- dplyr::filter(calib.hiv_awareness_unaids,  country == uCountry)
    calib.df[[6]] <- dplyr::filter(calib.not_on_art,            country == uCountry)
    calib.df[[7]] <- dplyr::filter(calib.plhiv,                 country == uCountry)
    calib.df[[8]] <- dplyr::filter(calib.previous_data,         country == uCountry)
    calib.df[[9]] <- dplyr::filter(calib.rates,                 country == uCountry)
    calib.df[[10]] <- dplyr::filter(calib.treatment_guidelines, country == uCountry)

    # Create a vector of data names
    names(calib.df) <- c(
        "incidence",
        "cd4",
        "treatment_guidelines",
        "art",
        "hiv_awareness_unaids",
        "not_on_art",
        "plhiv",
        "previous_data",
        "rates"
        )

    ## Incidence
    if (isReallyEmpty(calib.df$incidence)) {
        country <- uCountry
        type <- c("Lower", "Median", "Upper")
        "2010" <- as.numeric(NA)
        "2011" <- as.numeric(NA)
        "2012" <- as.numeric(NA)
        "2013" <- as.numeric(NA)
        "2014" <- as.numeric(NA)
        "2015" <- as.numeric(NA)
        "2016" <- as.numeric(NA)
        blankIncidence <- data.frame(country, type, get("2010"), get("2011"), get("2012"), get("2013"), get("2014"), get("2015"), get("2016"))
        names(blankIncidence) <- c("country", "type", as.character(seq(2010, 2016, 1)))
        calib.df$incidence <- dplyr::tbl_df(blankIncidence)
    }

    ## CD4
    if (isReallyEmpty(calib.df$cd4)) {
        country <- uCountry
        "prop.Off.ART.500" <- as.numeric(NA)
        "prop.Off.ART.350500" <- as.numeric(NA)
        "prop.Off.ART.250350" <- as.numeric(NA)
        "prop.Off.ART.200250" <- as.numeric(NA)
        "prop.Off.ART.100200" <- as.numeric(NA)
        "prop.Off.ART.50100" <- as.numeric(NA)
        "prop.Off.ART.50" <- as.numeric(NA)
        "prop.On.ART.500" <- as.numeric(NA)
        "prop.On.ART.350500" <- as.numeric(NA)
        "prop.On.ART.250350" <- as.numeric(NA)
        "prop.On.ART.200250" <- as.numeric(NA)
        "prop.On.ART.100200" <- as.numeric(NA)
        "prop.On.ART.50100" <- as.numeric(NA)
        "prop.On.ART.50" <- as.numeric(NA)

        blankCD4 <- data.frame(country,
        get("prop.Off.ART.500"),
        get("prop.Off.ART.350500"),
        get("prop.Off.ART.250350"),
        get("prop.Off.ART.200250"),
        get("prop.Off.ART.100200"),
        get("prop.Off.ART.50100"),
        get("prop.Off.ART.50"),
        get("prop.On.ART.500"),
        get("prop.On.ART.350500"),
        get("prop.On.ART.250350"),
        get("prop.On.ART.200250"),
        get("prop.On.ART.100200"),
        get("prop.On.ART.50100"),
        get("prop.On.ART.50"))
        names(blankCD4) <- c("country", "prop.Off.ART.500", "prop.Off.ART.350500", "prop.Off.ART.250350", "prop.Off.ART.200250", "prop.Off.ART.100200", "prop.Off.ART.50100", "prop.Off.ART.50", "prop.On.ART.500", "prop.On.ART.350500", "prop.On.ART.250350", "prop.On.ART.200250", "prop.On.ART.100200", "prop.On.ART.50100", "prop.On.ART.50")
        calib.df$cd4 <- dplyr::tbl_df(blankCD4)
    }

    ## Treatment Guidelines
    if (isReallyEmpty(calib.df$treatment_guidelines)) {
        country <- uCountry
        less200 <- as.integer(NA)
        less250 <- as.integer(NA)
        less350 <- as.integer(NA)
        less500 <- as.integer(NA)
        more500 <- as.integer(NA)
        blankGuidelines <- data.frame(country, less200, less250, less350, less500, more500)
        calib.df$treatment_guidelines <- dplyr::tbl_df(blankGuidelines)
    }

    ## All of the below goes into data[["calib"]]
    # list temp.names
    temp.names <- c(
        "temp.art",
        "temp.hiv_awareness_unaids",
        "temp.not_on_art",
        "temp.plhiv",
        "temp.previous_data"
        )

    for (i in 1:length(temp.names)) {
        if (exists(temp.names[i])) {
            rm(list = temp.names[i])
        }
    }

    # Reshape data
    # calib.art
    if (is.not.empty(calib.df$art)) {
        temp.art <- reshape2::melt(calib.df$art[c("country","indicator","2010","2011","2012","2013","2014","2015")],
            id.vars = c("country", "indicator"),
            variable.name = "year",
            value.name = "value")
        temp.art$weight <- "green"
    }

    # calib.not_on_art
    if (is.not.empty(calib.df$not_on_art)) {
        temp.not_on_art <- reshape2::melt(calib.df$not_on_art[c("country","indicator","2010","2011","2012","2013","2014","2015")],
            id.vars = c("country", "indicator"),
            variable.name = "year",
            value.name = "value")
        temp.not_on_art$weight <- "green"
    }

    # calib.plhiv
    if (is.not.empty(calib.df$plhiv)) {
        temp.plhiv <- reshape2::melt(calib.df$plhiv[c("country","indicator","2010","2011","2012","2013","2014","2015")],
            id.vars = c("country","indicator"),
            variable.name = "year",
            value.name = "value")
        temp.plhiv$weight <- "green"
    }

    # calib.hiv_awareness_unaids (remember this is a proportion of PLHIV)
    # Needs to be matched to PLHIV from the SAME YEAR.
    if (is.not.empty(calib.df$hiv_awareness_unaids)) {
        temp.hiv_awareness_unaids <- calib.df$hiv_awareness_unaids[c("country","indicator","year","value")]

        # Need to match PLHIV, if it doesn't exist for a particular year then we delete the value
        if (exists("temp.plhiv") & exists("temp.hiv_awareness_unaids")) {
            for (i in 1:dim(temp.hiv_awareness_unaids)[1]) {
                t.plhiv <- dplyr::filter(temp.plhiv, year == temp.hiv_awareness_unaids$year[i])$value
                temp.hiv_awareness_unaids[[i,"value"]] <- temp.hiv_awareness_unaids[[i,"value"]] * t.plhiv
                temp.hiv_awareness_unaids$weight <- "green"
            }
        } else {
            rm(temp.hiv_awareness_unaids)
        }
    }

    # calib.previous_data
    if (is.not.empty(calib.df$previous_data)) {
        temp.previous_data <- dplyr::filter(calib.df$previous_data[c("country","indicator","year","value","weight")], year >= 2010)
    }

    # Assemble master.data.frame
    # master.df <- build.master.df(temp.names, calib.df)
    out <- c()
    for (i in 1:length(temp.names)) {
        if (exists(temp.names[i])) {
            out <- rbind(out, get(temp.names[i]))
        }
    }

    if (length(out) == 0) {
        country <- uCountry

        indicator <- c(rep("PLHIV", 6),
            rep("PLHIV Diagnosed", 6),
            rep("PLHIV in Care", 6),
            rep("PLHIV on ART", 6),
            rep("PLHIV Suppressed", 6))

        year <- rep(seq(2010, 2015, 1), 5)

        value <- as.numeric(NA)
        weight <- factor(x = NA, levels = c("red", "amber", "green"))

        out <- data.frame(country, indicator, year, value, weight)
    }

    out.list <- list(
        calib.df$incidence,
        calib.df$cd4,
        calib.df$treatment_guidelines,
        out,
        calib.df$rates
        )

    names(out.list) <- c(
        "incidence",
        "cd4",
        "treatment_guidelines",
        "calib",
        "rates"
        )

    out.list
}

# test <- GetCountryData("Brazil")

# master.df needs to include
# incidence
# cd4
# rates
# treatment guidelines

# Below function called when resetting treatment guidelines

GetTreatmentGuidelines <- function(uCountry) {
    # Read in all relevant csv files
    c.file.path <- "server/data/calibration"
    calib.treatment_guidelines <- readr::read_csv(paste0(c.file.path, "/treatment-guidelines-cd4.csv"), col_names = TRUE, skip = 0)

    # Compile data.frame
    treatment_guidelines <- dplyr::filter(calib.treatment_guidelines, country == uCountry)

    # Return vector
    treatment_guidelines
}

