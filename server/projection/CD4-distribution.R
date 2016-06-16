GetCD4Distribution2015 <- function(uCountry) {
    proj.CD4 <- readr::read_csv("server/data/projection/cd4-distribution-2015.csv", col_names = TRUE, skip = 0)
    out.CD4 <- dplyr::filter(proj.CD4, Country == uCountry)
    out.CD4
}
