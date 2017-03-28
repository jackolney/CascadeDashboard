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
require(xlsx)
source("../../formal/UNAIDS/functions.R")
source("../../formal/initial.R")


####################################################################################################
# Countries

# list all countries with sufficient data to 'roll' through and produce results
countries <- c("Brazil", "Cambodia", "Cameroon", "China", "Cote d'Ivoire", "DRC", "Ethiopia",
    "Haiti", "Indonesia", "Jamaica", "Kenya", "Malawi", "Morocco", "Mozambique", "Myanmar",
    "Nigeria", "Pakistan", "Philippines", "Russia", "South Africa", "Tanzania", "Thailand",
    "Uganda", "Ukraine", "Vietnam", "Zambia", "Zimbabwe")

# total of 27 for the moment
length(countries)

# GLOBAL
MasterName <- countries[27]
MasterData <- GetMasterDataSet(MasterName)

# Do we have an R test MasterData function?
# its usually just overloading the guidelines
MasterData$treatment_guidelines
# MasterData$treatment_guidelines[[4]] <- 2015
# MasterData$treatment_guidelines[[5]] <- 2015
# MasterData$treatment_guidelines[[6]] <- 2016

# ---- #
set.seed(100)
# ---- #

# These will be adjusted in due course:
# MaxError <- 0.06
# MinNumber <- 1000
MaxError <- 0.2
MinNumber <- 500

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

# I think that we should be pushing jobs to the cluster here to maintain local stability
# i.e. clustr::login
# try locally first

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

####################################################################################################
# We will want to do some calibration checking...

# What about a country directory, with calibration figures, data and a workspace?
# Define output_path
output_path <- paste0("../../formal/UNAIDS/results/", MasterName, "/")
# Create result directory (safely, -p)
system(paste0("mkdir -p ", output_path))

# Cascade in 2015
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = paste0(output_path, "cascade-2015.pdf"), type = "pdf")

# Error Histogram
graphics.off(); quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(
    runError = runError,
    maxError = MaxError)
quartz.save(file = paste0(output_path, "sim-error.pdf"), type = "pdf")

# Calibration Detail
graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = paste0(output_path, "cascade-detail.pdf"), type = "pdf")

# Parameter Histograms
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()
quartz.save(file = paste0(output_path, "parameter-hist.pdf"), type = "pdf")

####################################################################################################
# UNAIDS workbook creation and completion functions

# Just declare this (unused)
AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

out_data <- get_spreadsheet_data(
    country_name = MasterName,
    notes = "max_error = 0.06, min_number = 1000")

write_spreadsheet(
    country_name = MasterName,
    global_out = out_data,
    path = output_path)

# Save image
image.path <- paste0(output_path, MasterName, ".RData")
save.image(file = image.path)
