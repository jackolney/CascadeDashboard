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
