# In here, write some functions that pull out the Marrakech data.
# We will then use these to MakeAssumptions() based on data.

# For instance, we know that 57% of PLHIV were in CARE in 2015 (From Marrakech Data)
# So we can calculate that value over time to get an estimate of those in care prior.

# For # diagnosed (missing from Marrakech data), so use data from neighbouring countries.
# Ethiopia = 0.6436 of PLHIV
# Tanzania = 0.5859 of PLHIV
# Uganda = 0.6274 of PLHIV
# mean = 0.6436

# We need to import Marrakech data
# Tease it apart so that we know DISCRETE CATEGORIES
# Run some tests to make sure it is all solid?

# By end of day Friday, have an imported dataset containing Marrakech data, previous data etc.
# Ready for model to be updated and fit to.
# Weighting is just the sum of the errors between model and data.
# We then adjust the contribution of individual errors to total error to make them more or less important.

# uCountry <- "Kenya"

GetMarrakechData <- function(uCountry) {
    # Pull out marrakech csv and fill it in (standard form),
    # Return to master file.

    m.data <- readr::read_csv("server/data/marrakech.csv", col_names = TRUE, skip = 0)
    colnames(m.data) <- c(
        "country",
        "year",
        "PLHIV",
        "PLHIV Diagnosed",
        "PLHIV in Care",
        "PLHIV on ART",
        "PLHIV Retained",
        "PLHIV Suppressed",
        "plhiv_score",
        "diagnosed_score",
        "care_score",
        "art_score",
        "retention_score",
        "suppression_score")

    # Check if country exists and
    if (sum(m.data$country == uCountry) > 0) {
        country.data <- dplyr::filter(m.data, country == uCountry)
    } else {
        stop("Country not found in dataset.")
    }

    # Melt
    out <- reshape2::melt(dplyr::filter(country.data, country == uCountry),
        id.vars = c("country", "year"),
        variable.name = "indicator",
        value.name = "value")

    # Remove *_score (for now)
    final <- out[grep("*_score", out$indicator, invert = TRUE),]

    # Add weights to the melted data.frame
    weights <- c()
    for (i in 1:length(unique(final$indicator))) {
        weights[i] <- out[grep("*_score", out$indicator, invert = FALSE),][i,"value"]
    }
    final$weight <- weights
    final$value <- as.double(final$value)

    final
}

# What is standard form here?

# Generate easily readable dataframe and return at end of function

# Master dataframe should have the functionality to sort out conflicting data

# GetMarrakechData("Kenya")
