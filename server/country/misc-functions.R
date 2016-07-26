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
