# Write data to spreadsheet
write_spreadsheet <- function(country_name, global_out, path) {
    # Create WorkBook
    wb <- createWorkbook()

    #####
    ## Sheet 1 (90-90-90)
    sheet1 <- createSheet(wb, sheetName = "90-90-90")

    cb <- CellBlock(sheet = sheet1, startRow = 1, startColumn = 1, noRows = 28, noColumns = 11)

    cs <- CellStyle(wb) + Font(wb, isBold = TRUE, heightInPoints = 12) + Alignment(h = "ALIGN_LEFT")

    # Set headers
    CB.setRowData(cellBlock = cb,
        x = c("country",
            "1st 90 in 2020",
            "2nd 90 in 2020",
            "3rd 90 in 2020",
            "1st 90 lower 95% CI",
            "2nd 90 lower 95% CI",
            "3rd 90 lower 95% CI",
            "1st 90 upper 95% CI",
            "2nd 90 upper 95% CI",
            "3rd 90 upper 95% CI",
            "Notes"),
        rowIndex = 1, colOffset = 0, rowStyle = cs)

    # Set colWidths
    setColumnWidth(sheet = sheet1, colIndex = 1,    colWidth = 12)
    setColumnWidth(sheet = sheet1, colIndex = 2:10, colWidth = 20)
    setColumnWidth(sheet = sheet1, colIndex = 11,   colWidth = 30)

    # ADD DATA
    CB.setRowData(cellBlock = cb, x = global_out[[1]], rowIndex = 2, colOffset = 0)

    # Borders
    TitleBorder <- Border(color = "black", position = c("TOP","BOTTOM","LEFT","RIGHT"),
        pen = c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN"))
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 6)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 7)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 8)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 9)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 10)
    CB.setBorder(cb, border = TitleBorder, rowIndex = 1:28, colIndex = 11)

    #####
    ## Sheet 2 (Cascade in 2015)
    sheet2 <- createSheet(wb , sheetName = "2015")

    cb2 <- CellBlock(sheet = sheet2, startRow = 1, startColumn = 1, noRows = 28, noColumns = 16)

    # Set headers
    CB.setRowData(cellBlock = cb2, x = c(
        "country",
        "PLHIV",
        "PLHIV Diagnosed",
        "PLHIV In Care",
        "PLHIV On ART",
        "PLHIV Virally Suppressed",
        "PLHIV lower 95% CI",
        "PLHIV Diagnosed lower 95% CI",
        "PLHIV In Care lower 95% CI",
        "PLHIV On ART lower 95% CI",
        "PLHIV Virally Suppressed lower 95% CI",
        "PLHIV upper 95% CI",
        "PLHIV Diagnosed upper 95% CI",
        "PLHIV In Care upper 95% CI",
        "PLHIV On ART upper 95% CI",
        "PLHIV Virally Suppressed upper 95% CI"),
        rowIndex = 1, colOffset = 0, rowStyle = cs)

    setColumnWidth(sheet = sheet2, colIndex = 1,     colWidth = 12)
    setColumnWidth(sheet = sheet2, colIndex = 2:16,  colWidth = 20)

    # ADD DATA
    CB.setRowData(cellBlock = cb2, x = global_out[[2]], rowIndex = 2, colOffset = 0)

    # Borders
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 6)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 7)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 8)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 9)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 10)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 11)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 12)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 13)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 14)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 15)
    CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 16)

    #####
    ## Sheet 3 (Cascade in 2020)
    sheet3 <- createSheet(wb , sheetName = "2020")

    cb3 <- CellBlock(sheet = sheet3, startRow = 1, startColumn = 1, noRows = 28, noColumns = 16)

    # Set headers
    CB.setRowData(cellBlock = cb3, x = c(
        "country",
        "PLHIV",
        "PLHIV Diagnosed",
        "PLHIV In Care",
        "PLHIV On ART",
        "PLHIV Virally Suppressed",
        "PLHIV lower 95% CI",
        "PLHIV Diagnosed lower 95% CI",
        "PLHIV In Care lower 95% CI",
        "PLHIV On ART lower 95% CI",
        "PLHIV Virally Suppressed lower 95% CI",
        "PLHIV upper 95% CI",
        "PLHIV Diagnosed upper 95% CI",
        "PLHIV In Care upper 95% CI",
        "PLHIV On ART upper 95% CI",
        "PLHIV Virally Suppressed upper 95% CI"),
        rowIndex = 1, colOffset = 0, rowStyle = cs)

    setColumnWidth(sheet = sheet3, colIndex = 1,     colWidth = 12)
    setColumnWidth(sheet = sheet3, colIndex = 2:16,  colWidth = 20)

    # ADD DATA
    CB.setRowData(cellBlock = cb3, x = global_out[[3]], rowIndex = 2, colOffset = 0)

    # Borders
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 6)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 7)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 8)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 9)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 10)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 11)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 12)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 13)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 14)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 15)
    CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 16)

    #####
    ## Sheet 4 (AIDS Deaths)
    sheet4 <- createSheet(wb , sheetName = "AIDS Deaths")

    cb4 <- CellBlock(sheet = sheet4, startRow = 1, startColumn = 1, noRows = 28, noColumns = 16)

    # Set headers
    CB.setRowData(cellBlock = cb4, x = c(
        "country",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2015 lower 95% CI",
        "2016 lower 95% CI",
        "2017 lower 95% CI",
        "2018 lower 95% CI",
        "2019 lower 95% CI",
        "2015 upper 95% CI",
        "2016 upper 95% CI",
        "2017 upper 95% CI",
        "2018 upper 95% CI",
        "2019 upper 95% CI"),
        rowIndex = 1, colOffset = 0, rowStyle = cs)

    setColumnWidth(sheet = sheet4, colIndex = 1,     colWidth = 12)
    setColumnWidth(sheet = sheet4, colIndex = 2:16,  colWidth = 20)

    # ADD DATA
    CB.setRowData(cellBlock = cb4, x = global_out[[4]], rowIndex = 2, colOffset = 0)

    # Borders
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 6)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 7)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 8)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 9)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 10)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 11)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 12)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 13)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 14)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 15)
    CB.setBorder(cb4, border = TitleBorder, rowIndex = 1:28, colIndex = 16)

    #####
    ## Sheet 5 (New Infections)
    sheet5 <- createSheet(wb , sheetName = "New Infections")

    cb5 <- CellBlock(sheet = sheet5, startRow = 1, startColumn = 1, noRows = 28, noColumns = 16)

    # Set headers
    CB.setRowData(cellBlock = cb5, x = c(
        "country",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2015 lower 95% CI",
        "2016 lower 95% CI",
        "2017 lower 95% CI",
        "2018 lower 95% CI",
        "2019 lower 95% CI",
        "2015 upper 95% CI",
        "2016 upper 95% CI",
        "2017 upper 95% CI",
        "2018 upper 95% CI",
        "2019 upper 95% CI"),
        rowIndex = 1, colOffset = 0, rowStyle = cs)

    setColumnWidth(sheet = sheet5, colIndex = 1,     colWidth = 12)
    setColumnWidth(sheet = sheet5, colIndex = 2:16,   colWidth = 20)

    # ADD DATA
    CB.setRowData(cellBlock = cb5, x = global_out[[5]], rowIndex = 2, colOffset = 0)

    # Borders
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 6)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 7)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 8)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 9)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 10)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 11)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 12)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 13)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 14)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 15)
    CB.setBorder(cb5, border = TitleBorder, rowIndex = 1:28, colIndex = 16)


    # Save that shit.
    # SAVE BY COUNTRY_NAME
    saveWorkbook(wb, file = paste0(path, country_name, ".xlsx"))
}

# Build spreadsheet data structure
get_spreadsheet_data <- function(country_name, notes) {
    # Global Output that will constitute one line of each workbook
    global_out <- list()

    ####################################################################################################
    # Sheet one (90-90-90)
    # "country", "1st 90 in 2020", "lower 95% CI", "upper 95% CI", "2nd 90 in 2020", "lower 95% CI", "upper 95% CI", "3rd 90 in 2020", "lower 95% CI", "upper 95% CI",
    # "Notes"

    # just declare it
    AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

    # Can we extract the confidence intervals too? - YES.

    out <- Get909090Data()

    # First 90
    x_1 = round(out[out$def == "Diagnosed / PLHIV", "res"], digits = 2)
    min_1 = round(out[out$def == "Diagnosed / PLHIV", "min"], digits = 2)
    max_1 = round(out[out$def == "Diagnosed / PLHIV", "max"], digits = 2)
    # u1st_90 <- paste0(x_1, " (", min_1, " to ", max_1, ")")

    # Second 90
    x_2 = round(out[out$def == "On Treatment / Diagnosed", "res"], digits = 2)
    min_2 = round(out[out$def == "On Treatment / Diagnosed", "min"], digits = 2)
    max_2 = round(out[out$def == "On Treatment / Diagnosed", "max"], digits = 2)
    # u2nd_90 <- paste0(x_2, " (", min_2, " to ", max_2, ")")

    # Third 90
    x_3 = round(out[out$def == "Virally Suppressed / On Treatment", "res"], digits = 2)
    min_3 = round(out[out$def == "Virally Suppressed / On Treatment", "min"], digits = 2)
    max_3 = round(out[out$def == "Virally Suppressed / On Treatment", "max"], digits = 2)
    # u3rd_90 <- paste0(x_3, " (", min_3, " to ", max_3, ")")

    # NOTES
    # comes in as arg now
    # notes <- "Good fit."

    global_out[[1]] <- c(country_name, x_1, x_2, x_3, min_1, min_2, min_3, max_1, max_2, max_3, notes)

    ####################################################################################################
    # Sheet two (Cascade in 2015)
    # "country", "PLHIV", "Diagnosed", "In Care", "On ART", "Virally Suppressed"

    out_t0 <- GetCascadeData(1)   # t0 = 1

    # PLHIV
    plhiv_x <- round(out_t0[out_t0$def == "PLHIV", "res"], digits = 0)
    plhiv_min <- round(out_t0[out_t0$def == "PLHIV", "min"], digits = 0)
    plhiv_max <- round(out_t0[out_t0$def == "PLHIV", "max"], digits = 0)
    # plhiv <- paste0(plhiv_x, " (", plhiv_min, " to ", plhiv_max, ")")

    # Diagnosed
    diag_x <- round(out_t0[out_t0$def == "Diagnosed", "res"], digits = 0)
    diag_min <- round(out_t0[out_t0$def == "Diagnosed", "min"], digits = 0)
    diag_max <- round(out_t0[out_t0$def == "Diagnosed", "max"], digits = 0)
    # diagnosed <- paste0(diag_x, " (", diag_min, " to ", diag_max, ")")

    # In Care
    care_x <- round(out_t0[out_t0$def == "In Care", "res"], digits = 0)
    care_min <- round(out_t0[out_t0$def == "In Care", "min"], digits = 0)
    care_max <- round(out_t0[out_t0$def == "In Care", "max"], digits = 0)
    # care <- paste0(care_x, " (", care_min, " to ", care_max, ")")

    # On ART
    art_x <- round(out_t0[out_t0$def == "Treatment", "res"], digits = 0)
    art_min <- round(out_t0[out_t0$def == "Treatment", "min"], digits = 0)
    art_max <- round(out_t0[out_t0$def == "Treatment", "max"], digits = 0)
    # art <- paste0(art_x, " (", art_min, " to ", art_max, ")")

    # Virally Suppressed
    supp_x <- round(out_t0[out_t0$def == "Suppressed", "res"], digits = 0)
    supp_min <- round(out_t0[out_t0$def == "Suppressed", "min"], digits = 0)
    supp_max <- round(out_t0[out_t0$def == "Suppressed", "max"], digits = 0)
    # supp <- paste0(supp_x, " (", supp_min, " to ", supp_max, ")")

    global_out[[2]] <- c(country_name, plhiv_x, diag_x, care_x, art_x, supp_x, plhiv_min, diag_min,
       care_min, art_min, supp_min, plhiv_max, diag_max, care_max, art_max, supp_max)

    ####################################################################################################
    # Sheet three (Cascade in 2020)
    # "country", "PLHIV", "Diagnosed", "In Care", "On ART", "Virally Suppressed"

    out_t5 <- GetCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    # PLHIV
    plhiv_x <- round(out_t5[out_t5$def == "PLHIV", "res"], digits = 0)
    plhiv_min <- round(out_t5[out_t5$def == "PLHIV", "min"], digits = 0)
    plhiv_max <- round(out_t5[out_t5$def == "PLHIV", "max"], digits = 0)
    # plhiv <- paste0(plhiv_x, " (", plhiv_min, " to ", plhiv_max, ")")

    # Diagnosed
    diag_x <- round(out_t5[out_t5$def == "Diagnosed", "res"], digits = 0)
    diag_min <- round(out_t5[out_t5$def == "Diagnosed", "min"], digits = 0)
    diag_max <- round(out_t5[out_t5$def == "Diagnosed", "max"], digits = 0)
    # diagnosed <- paste0(diag_x, " (", diag_min, " to ", diag_max, ")")

    # In Care
    care_x <- round(out_t5[out_t5$def == "In Care", "res"], digits = 0)
    care_min <- round(out_t5[out_t5$def == "In Care", "min"], digits = 0)
    care_max <- round(out_t5[out_t5$def == "In Care", "max"], digits = 0)
    # care <- paste0(care_x, " (", care_min, " to ", care_max, ")")

    # On ART
    art_x <- round(out_t5[out_t5$def == "Treatment", "res"], digits = 0)
    art_min <- round(out_t5[out_t5$def == "Treatment", "min"], digits = 0)
    art_max <- round(out_t5[out_t5$def == "Treatment", "max"], digits = 0)
    # art <- paste0(art_x, " (", art_min, " to ", art_max, ")")

    # Virally Suppressed
    supp_x <- round(out_t5[out_t5$def == "Suppressed", "res"], digits = 0)
    supp_min <- round(out_t5[out_t5$def == "Suppressed", "min"], digits = 0)
    supp_max <- round(out_t5[out_t5$def == "Suppressed", "max"], digits = 0)
    # supp <- paste0(supp_x, " (", supp_min, " to ", supp_max, ")")

    global_out[[3]] <- c(country_name, plhiv_x, diag_x, care_x, art_x, supp_x, plhiv_min, diag_min,
       care_min, art_min, supp_min, plhiv_max, diag_max, care_max, art_max, supp_max)

    ####################################################################################################
    # Sheet four (AIDS Deaths)
    # "country", "2015", "2016", "2017", "2018", "2019"

    ad <- round(get_aids_deaths_data(), digits = 0)

    ad_2015_x   <- ad[ad$timeOut == 2015, "HivMortality"]
    ad_2015_min <- ad[ad$timeOut == 2015, "min"]
    ad_2015_max <- ad[ad$timeOut == 2015, "max"]

    ad_2016_x   <- ad[ad$timeOut == 2016, "HivMortality"]
    ad_2016_min <- ad[ad$timeOut == 2016, "min"]
    ad_2016_max <- ad[ad$timeOut == 2016, "max"]

    ad_2017_x   <- ad[ad$timeOut == 2017, "HivMortality"]
    ad_2017_min <- ad[ad$timeOut == 2017, "min"]
    ad_2017_max <- ad[ad$timeOut == 2017, "max"]

    ad_2018_x   <- ad[ad$timeOut == 2018, "HivMortality"]
    ad_2018_min <- ad[ad$timeOut == 2018, "min"]
    ad_2018_max <- ad[ad$timeOut == 2018, "max"]

    ad_2019_x   <- ad[ad$timeOut == 2019, "HivMortality"]
    ad_2019_min <- ad[ad$timeOut == 2019, "min"]
    ad_2019_max <- ad[ad$timeOut == 2019, "max"]

    global_out[[4]] <- c(country_name, ad_2015_x, ad_2016_x, ad_2017_x, ad_2018_x, ad_2019_x,
        ad_2015_min, ad_2016_min, ad_2017_min, ad_2018_min, ad_2019_min, ad_2015_max, ad_2016_max,
        ad_2017_max, ad_2018_max, ad_2019_max)

    ####################################################################################################
    # Sheet five (New Infections)
    # "country", "2015", "2016", "2017", "2018", "2019"

    ni <- round(get_new_infections_data(), digits = 0)

    ni_2015_x   <- ni[ni$timeOut == 2015, "NewInf"]
    ni_2015_min <- ni[ni$timeOut == 2015, "min"]
    ni_2015_max <- ni[ni$timeOut == 2015, "max"]

    ni_2016_x   <- ni[ni$timeOut == 2016, "NewInf"]
    ni_2016_min <- ni[ni$timeOut == 2016, "min"]
    ni_2016_max <- ni[ni$timeOut == 2016, "max"]

    ni_2017_x   <- ni[ni$timeOut == 2017, "NewInf"]
    ni_2017_min <- ni[ni$timeOut == 2017, "min"]
    ni_2017_max <- ni[ni$timeOut == 2017, "max"]

    ni_2018_x   <- ni[ni$timeOut == 2018, "NewInf"]
    ni_2018_min <- ni[ni$timeOut == 2018, "min"]
    ni_2018_max <- ni[ni$timeOut == 2018, "max"]

    ni_2019_x   <- ni[ni$timeOut == 2019, "NewInf"]
    ni_2019_min <- ni[ni$timeOut == 2019, "min"]
    ni_2019_max <- ni[ni$timeOut == 2019, "max"]

    global_out[[5]] <- c(country_name, ni_2015_x, ni_2016_x, ni_2017_x, ni_2018_x, ni_2019_x,
        ni_2015_min, ni_2016_min, ni_2017_min, ni_2018_min, ni_2019_min, ni_2015_max, ni_2016_max,
        ni_2017_max, ni_2018_max, ni_2019_max)

    # return the global_out list
    global_out
}

# Extract AIDS Deaths Data from model results
get_aids_deaths_data <- function() {
    result <- GetModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Quantile_95(unlist(lapply(result, function(x) sum(x$HivMortality[j]))))
        out[j] <- dat[["mean"]]
        min[j] <- dat[["lower"]]
        max[j] <- dat[["upper"]]
    }

    HM_out <- c(0, diff(out))
    HM_min <- c(0, diff(min))
    HM_max <- c(0, diff(max))

    times <- seq(0, 5, 0.02)
    combo <- cbind(times, HM_out, HM_min, HM_max)

    # Calculate time intervals
    yr2015 <- times[times >= 0 & times <= 1]
    yr2016 <- times[times > 1  & times <= 2]
    yr2017 <- times[times > 2  & times <= 3]
    yr2018 <- times[times > 3  & times <= 4]
    yr2019 <- times[times > 4  & times <= 5]

    # count between years to calculate bars
    bar1 <- combo[times %in% yr2015,]
    bar2 <- combo[times %in% yr2016,]
    bar3 <- combo[times %in% yr2017,]
    bar4 <- combo[times %in% yr2018,]
    bar5 <- combo[times %in% yr2019,]

    HivMortality <- c(
        sum(bar1[,"HM_out"]),
        sum(bar2[,"HM_out"]),
        sum(bar3[,"HM_out"]),
        sum(bar4[,"HM_out"]),
        sum(bar5[,"HM_out"])
    )

    min <- c(
        sum(bar1[,"HM_min"]),
        sum(bar2[,"HM_min"]),
        sum(bar3[,"HM_min"]),
        sum(bar4[,"HM_min"]),
        sum(bar5[,"HM_min"])
    )

    max <- c(
        sum(bar1[,"HM_max"]),
        sum(bar2[,"HM_max"]),
        sum(bar3[,"HM_max"]),
        sum(bar4[,"HM_max"]),
        sum(bar5[,"HM_max"])
    )

    timeOut <- seq(2015, 2019, 1)
    df <- data.frame(timeOut, HivMortality, min, max)
    df
}

# Extract New Infections Data from model results
get_new_infections_data <- function() {
    result <- GetModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Quantile_95(unlist(lapply(result, function(x) sum(x$NewInf[j]))))
        out[j] <- dat[["mean"]]
        min[j] <- dat[["lower"]]
        max[j] <- dat[["upper"]]
    }

    NI_out <- c(0, diff(out))
    NI_min <- c(0, diff(min))
    NI_max <- c(0, diff(max))

    times <- seq(0, 5, 0.02)
    combo <- cbind(times, NI_out, NI_min, NI_max)

    # Calculate time intervals
    yr2015 <- times[times >= 0 & times <= 1]
    yr2016 <- times[times > 1  & times <= 2]
    yr2017 <- times[times > 2  & times <= 3]
    yr2018 <- times[times > 3  & times <= 4]
    yr2019 <- times[times > 4  & times <= 5]

    # count between years to calculate bars
    bar1 <- combo[times %in% yr2015,]
    bar2 <- combo[times %in% yr2016,]
    bar3 <- combo[times %in% yr2017,]
    bar4 <- combo[times %in% yr2018,]
    bar5 <- combo[times %in% yr2019,]

    NewInf <- c(
        sum(bar1[,"NI_out"]),
        sum(bar2[,"NI_out"]),
        sum(bar3[,"NI_out"]),
        sum(bar4[,"NI_out"]),
        sum(bar5[,"NI_out"])
    )

    min <- c(
        sum(bar1[,"NI_min"]),
        sum(bar2[,"NI_min"]),
        sum(bar3[,"NI_min"]),
        sum(bar4[,"NI_min"]),
        sum(bar5[,"NI_min"])
    )

    max <- c(
        sum(bar1[,"NI_max"]),
        sum(bar2[,"NI_max"]),
        sum(bar3[,"NI_max"]),
        sum(bar4[,"NI_max"]),
        sum(bar5[,"NI_max"])
    )

    timeOut <- seq(2015, 2019, 1)
    df <- data.frame(timeOut, NewInf, min, max)
    df
}
