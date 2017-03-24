############################################
# UNAIDS 90-90-90 Trends Estimation Script #
# Jack Olney                               #
# 24/03/17                                 #
############################################

####################################################################################################
# Setup

rm(list=ls())
setwd("~/git/CascadeDashboard/inst/app")

# Graphics (is this neccessary?)
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")


####################################################################################################
# Countries

# list all countries with sufficient data to 'roll' through and produce results
countries <- c("Brazil", "Cambodia", "Cameroon", "China", "Cote d'Ivoire", "DRC", "Ethiopia",
    "Haiti", "Indonesia", "Jamaica", "Kenya", "Malawi", "Morocco", "Mozambique", "Myanmar",
    "Nigeria", "Pakistan", "Philippines", "Russia", "South Africa", "Tanzania", "Thailand",
    "Uganda", "Ukraine", "Vietnam", "Zambia", "Zimbabwe")

# total of 27 for the moment

# GLOBAL
MasterName <- "Zimbabwe"
MasterData <- GetMasterDataSet(MasterName)

# ---- #
set.seed(100)
# ---- #

# These will be adjusted in due course:
# MaxError <- 0.06
# MinNumber <- 1000
MaxError <- 2
MinNumber <- 50

# After first simulation, run this function (default = 5%)
# MaxError <- find_error_bound(runError, prop = 0.05)

# Define Parameter Range
parRange <- DefineParmRange(
    rho     = c(0, 0.005),
    q       = c(0.9, 1),
    epsilon = c(100, 100),
    kappa   = c(0, 3),
    gamma   = c(2, 4),
    theta   = c(0, 0.5),
    p       = c(0.7, 1),
    omega   = c(0, 0.01)
)

# Run Calibration
start.time <- proc.time()
RunNSCalibration(
    country = MasterName,
    data = MasterData,
    maxIterations = 1e4,
    maxError = MaxError,
    limit = MinNumber,
    parRange = parRange,
    targetIterations = 1e5)
finish.time <- proc.time() - start.time
finish.time[3] / 60
# 95 min runtime


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
notes <- "Good fit."

global_out[[1]] <- c(country, x_1, min_1, max_1, x_2, min_2, max_2, x_3, min_3, max_3, notes)


####################################################################################################
# Sheet two (Cascade in 2015)
# "country", "PLHIV", "Diagnosed", "In Care", "On ART", "Virally Suppressed"

out_t0 <- GetCascadeData(1)   # t0 = 1

# looks like this:
#          def       res      min     max
# 1      PLHIV 1436197.6 995563.7 1846634
# 2  Diagnosed 1047412.1 646318.3 1427130
# 3    In Care  883811.5 478092.9 1258294
# 4  Treatment  883193.9 477950.4 1258288
# 5 Suppressed  787868.5 430002.4 1139547

# just need to package is nicely.
# I think just shove it all in a character string that gets stuck in a cell.
# Lets not go too fancy for these values.


####################################################################################################
# Sheet three (Cascade in 2020)
# "country", "PLHIV", "Diagnosed", "In Care", "On ART", "Virally Suppressed"

out_t5 <- GetCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

# Mortality trends?

# New Infections trends?


####################################################################################################
# xlsx dev area

# GOAL:
# > xlsx sheet that contains all the info needed by UNAIDS

# INPUT:
# Each country should be a list with elements three
# Each element will contain a vector of values that will be splayed across the excel sheet
# [[1]] = 90-90-90 in 2020
# [[2]] = Cascade in 2015
# [[3]] = Cascade in 2020

# cols:
    # country
    # 1st 90
    # 2nd 90
    # 3rd 90
    # PLHIV 2015
    # DIAGNOSED 2015
    # IN CARE 2015
    # ON ART 2015
    # SUPPRESSED 2015
    # PLHIV 2020
    # DIAGNOSED 2020
    # IN CARE 2020
    # ON ART 2020
    # SUPPRESSED 2020


# Should wrap this in a function that takes a result object and stretches around it
require(xlsx)

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
        "1st 90 in 2020", "1st 90 lower 95% CI", "1st 90 upper 95% CI",
        "2nd 90 in 2020", "2nd 90 lower 95% CI", "2nd 90 upper 95% CI",
        "3rd 90 in 2020", "3rd 90 lower 95% CI", "3rd 90 upper 95% CI",
        "Notes"),
    rowIndex = 1, colOffset = 0, rowStyle = cs)

# Set colWidths
setColumnWidth(sheet = sheet1, colIndex = 1,    colWidth = 12)
setColumnWidth(sheet = sheet1, colIndex = 2:10, colWidth = 20)
setColumnWidth(sheet = sheet1, colIndex = 11,   colWidth = 30)

# ADD DATA
CB.setMatrixData(cb, x = as.matrix(countries), startRow = 2, startColumn = 1)

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

cb2 <- CellBlock(sheet = sheet2, startRow = 1, startColumn = 1, noRows = 28, noColumns = 6)

# Set headers
CB.setRowData(cellBlock = cb2, x = c("country", "PLHIV", "PLHIV Diagnosed", "PLHIV In Care",
    "PLHIV On ART", "PLHIV Virally Suppressed"),
    rowIndex = 1, colOffset = 0, rowStyle = cs)

setColumnWidth(sheet = sheet2, colIndex = 1,     colWidth = 12)
setColumnWidth(sheet = sheet2, colIndex = 2:5,   colWidth = 20)
setColumnWidth(sheet = sheet2, colIndex = 6,     colWidth = 30)

# ADD DATA
CB.setMatrixData(cb2, x = as.matrix(countries), startRow = 2, startColumn = 1)

# Borders
CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
CB.setBorder(cb2, border = TitleBorder, rowIndex = 1:28, colIndex = 6)

#####
## Sheet 3 (Cascade in 2020)
sheet3 <- createSheet(wb , sheetName = "2020")

cb3 <- CellBlock(sheet = sheet3, startRow = 1, startColumn = 1, noRows = 28, noColumns = 6)

# Set headers
CB.setRowData(cellBlock = cb3, x = c("country", "PLHIV", "PLHIV Diagnosed", "PLHIV In Care",
    "PLHIV On ART", "PLHIV Virally Suppressed"),
    rowIndex = 1, colOffset = 0, rowStyle = cs)

setColumnWidth(sheet = sheet3, colIndex = 1,     colWidth = 12)
setColumnWidth(sheet = sheet3, colIndex = 2:5,   colWidth = 20)
setColumnWidth(sheet = sheet3, colIndex = 6,     colWidth = 30)

# ADD DATA
CB.setMatrixData(cb3, x = as.matrix(countries), startRow = 2, startColumn = 1)

# Borders
CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 1)
CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 2)
CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 3)
CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 4)
CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 5)
CB.setBorder(cb3, border = TitleBorder, rowIndex = 1:28, colIndex = 6)

# Save that shit.
saveWorkbook(wb, file = "results.xlsx")
