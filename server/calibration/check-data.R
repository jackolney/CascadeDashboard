CheckCSV_Incidence <- function(uCountry) {
    data <- readr::read_csv("server/data/calibration/incidence-uncertainty.csv", col_names = TRUE, skip = 1)
    out <- data[data$country == uCountry,]
    if (dim(out)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

CheckCSV_CD4 <- function(uCountry) {
    data <- readr::read_csv("server/data/calibration/cd4-distribution-2010.csv", col_names = TRUE, skip = 0)
    out <- data[data$country == uCountry,]
    if (dim(out)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

CheckCSV_Treatment <- function(uCountry) {
    data <- readr::read_csv("server/data/calibration/treatment-guidelines-cd4.csv", col_names = TRUE, skip = 0)
    out <- data[data$country == uCountry,]
    if (dim(out)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

CheckCSV_PLHIV <- function(uCountry) {
    data <- readr::read_csv("server/data/calibration/plhiv.csv", col_names = TRUE, skip = 1)
    out <- data[data$country == uCountry,]
    if (dim(out)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

CheckCSV_ART <- function(uCountry) {
    data <- readr::read_csv("server/data/calibration/art.csv", col_names = TRUE, skip = 1)
    out <- data[data$country == uCountry,]
    if (dim(out)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

### Custom MasterData Checks ###
Check_NewCascade <- function(theData) {
    test <- theData$calib
    d2010 <- test[test$year == 2010,]
    subTest <- subset(test, test$value != 0 & !is.na(test$value))

    PLHIV_test <- any(d2010$indicator == "PLHIV") & !is.na(d2010[d2010$indicator == "PLHIV","value"]) & d2010[d2010$indicator == "PLHIV","value"] != 0 & !is.na(d2010[d2010$indicator == "PLHIV","weight"])
    PLHIV_ART_test <- any(d2010$indicator == "PLHIV on ART") & !is.na(d2010[d2010$indicator == "PLHIV on ART","value"]) & d2010[d2010$indicator == "PLHIV on ART","value"] != 0 & !is.na(d2010[d2010$indicator == "PLHIV on ART","weight"])

    if (PLHIV_test) {
        if (PLHIV_ART_test) {
            if (dim(subTest)[1] != 0) {
                if (!any(is.na(subTest$weight)) & !any(subTest$weight == "NA")) {
                    return(TRUE)
                } else return(FALSE)
            } else return(FALSE)
        } else return(FALSE)
    } else return(FALSE)
}

Check_NewCD4 <- function(theData) {
    if (!any(is.na(theData$cd4))) {
        if (round(sum(theData$cd4[2:8]), digits = 2) == 1) {
            if (round(sum(theData$cd4[9:15]), digits = 2) == 1) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(FALSE)
        }
    } else return(FALSE)
}

CheckOrder <- function(x) if (x[3] <= x[2] & x[2] <= x[1]) return(TRUE) else return(FALSE)

Check_NewIncidence <- function(theData) {
    test <- theData$incidence
    if (sum(test[,as.character(seq(2010,2016,1))] <= 0) | sum(is.na(test[,as.character(seq(2010,2016,1))]))) {
        return(FALSE)
    } else {
        ordered <- test[order(test$type, decreasing = TRUE),]
        if (CheckOrder(t(ordered[,"2010"])) & CheckOrder(t(ordered[,"2011"])) & CheckOrder(t(ordered[,"2012"])) & CheckOrder(t(ordered[,"2013"])) & CheckOrder(t(ordered[,"2014"])) & CheckOrder(t(ordered[,"2015"])) & CheckOrder(t(ordered[,"2016"]))) {
            return(TRUE)
        } else return(FALSE)
    }
}

Check_NewGuidelines <- function(theData) {
    test <- theData$treatment_guidelines
    if (!any(is.na(test[,2:6]))) {
        if (GuidelineCheck(tx_l200 = test$less200, tx_l250 = test$less250, tx_l350 = test$less350, tx_l500 = test$less500, tx_m500 = test$more500)) {
            return(TRUE)
        } else return(FALSE)
    } else return(FALSE)
}

