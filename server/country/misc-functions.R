isReallyEmpty <- function(x) {
    if (length(x) == 0) {
        return(TRUE)
    } else if (is(x)[1] == "tbl_df") {
        if (nrow(x) == 0) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}

AddNAToMasterData <- function(theBlank, theData) {
    theBlank$country <- theData$country[1]
    if (dim(na.omit(theData))[1] != 0) {
        uInd <- as.character(unique(theBlank$indicator))
        uYr <- as.character(unique(theBlank$year))
        for(i in 1:length(uInd)) {
            for(j in 1:length(uYr)) {
                if (isReallyEmpty(theData[theData$indicator    == uInd[i] & theData$year  == uYr[j], "value"])) next else {
                    if (!is.na(theData[theData$indicator == uInd[i] & theData$year  == uYr[j], "value"]) & theData[theData$indicator == uInd[i] & theData$year == uYr[j], "value"] != 0) {
                        theBlank[theBlank$indicator      == uInd[i] & theBlank$year == uYr[j], "value"] <- theData[theData$indicator == uInd[i] & theData$year == uYr[j], "value"]
                    }
                }
                if (isReallyEmpty(theData[theData$indicator    == uInd[i] & theData$year  == uYr[j], "weight"])) next else {
                    if (!is.na(theData[theData$indicator == uInd[i] & theData$year  == uYr[j], "weight"]) & theData[theData$indicator == uInd[i] & theData$year == uYr[j], "weight"] != 0) {
                        theBlank[theBlank$indicator      == uInd[i] & theBlank$year == uYr[j], "weight"] <- theData[theData$indicator == uInd[i] & theData$year == uYr[j], "weight"]
                    }
                }
                if (isReallyEmpty(theData[theData$indicator    == uInd[i] & theData$year  == uYr[j], "source"])) next else {
                    if (!is.na(theData[theData$indicator == uInd[i] & theData$year  == uYr[j], "source"]) & theData[theData$indicator == uInd[i] & theData$year == uYr[j], "source"] != 0) {
                        theBlank[theBlank$indicator      == uInd[i] & theBlank$year == uYr[j], "source"] <- theData[theData$indicator == uInd[i] & theData$year == uYr[j], "source"]
                    }
                }
            }
        }
    }
    theBlank
}

checkForClashes <- function(theData) {
    test <- theData$calib
    uYr <- unique(test$year)
    uInd <- unique(test$indicator)
    for(i in 1:length(uInd)) {
        for(j in 1:length(uYr)) {
            if (dim(test[test$indicator == uInd[i] & test$year == uYr[j],])[1] > 1) {
                warning(paste("Data clash on", uInd[i], "in", uYr[j]))
            }
        }
    }
}

GetCD4Distribution2015 <- function(uCountry) {
    proj.CD4 <- readr::read_csv("server/data/projection/cd4-distribution-2015.csv", col_names = TRUE, skip = 0)
    out.CD4 <- dplyr::filter(proj.CD4, country == uCountry)
    if (isReallyEmpty(out.CD4)) {
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
        return(dplyr::tbl_df(blankCD4))
    } else {
        return(out.CD4)
    }
}
