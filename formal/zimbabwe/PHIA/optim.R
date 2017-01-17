# optim calibration method

# optim(par, fn) #

# par = initial values for parameters to be optimised over
# fn = a function to be minimised, should take single argument of vector of parameters





GetError <- function(par) {

    # par needs to define
    # y
    # p
    # i

    # time can be defined here
    # country also here
    # data must be global

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

################################################################################
# Zimbabwe optim script
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard/inst/app")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")


MasterName <- "Zimbabwe"
MaxError <- 0.1
MinNumber <- 100

MasterData <- GetMasterDataSet(MasterName)

parRange <- DefineParmRange(
    p = c(0.7, 1),
    omega = c(0, 0.01),
    epsilon = c(100, 100),
    q = c(0.9, 1),
    kappa = c(0, 0.05)
)

# function setup
country          = MasterName
data             = MasterData
maxIterations    = 1e4
maxError         = MaxError
limit            = MinNumber
parRange         = parRange
targetIterations = 1e5


# define time
time <- seq(0, 5, 1)

# define p
p <- parameters(
    prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
    prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
    prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
    prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
    prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
    prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
    prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
    t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
    t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
    t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
    t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
    t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
)



# define y
optim_initial <- matrix(0,1,4)
colnames(optim_initial) <- c("plhiv", "plhiv_diag", "plhiv_care", "plhiv_art")
optim_initial[1,] <- c(1220058, 728013, 400000, 292573)
y <- GetCalibInitial(p, data, init2010 = optim_initial[1,])

# define i
# using median values from Spectrum
i <- incidence(as.double(data$incidence[2,3:9]))
# scope to add this as a paramter later?

# p
# y
# i
# SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

# define par (the parameters that we want optim to vary)
par <- c(
    Rho     = 0.3148,
    Epsilon = 100,
    Kappa   = 0.0261,
    Gamma   = 0.8037,
    Theta   = 0.3992,
    Omega   = 0.0052,
    p       = 0.8375,
    q       = 0.9479
)

GetError <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

GetError(par)

optim(par, GetError)

result <- c(
    Rho     = 0.031277257,
    Epsilon = 102.567917414,
    Kappa   = 2.064367960,
    Gamma   = 1.619482994,
    Theta   = 0.421657503,
    Omega   = 0.001290356,
    p       = 0.785773456,
    q       = 1.269157212
)
# error = 0.01278704
GetError(result)
# 400000 in care.


GetErrorResult <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

calibOut <- GetErrorResult(result)

BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)

BuildPHIAPlot(data = calibOut)

# OKAY we should only be passing parameters that we VARY

optim parameter bounds


# optim(par = par, fn = GetError, method = "L-BFGS-B", lower = parRange$min, upper = parRange$max)

################################################################################
# Constrained Optimisation TAKE TWO #

GetErrorConstrained <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 90)   par[["Epsilon"]] <- 90
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.7)  par[["p"]]       <- 0.7
    if (par[["q"]]       < 0.9)  par[["q"]]       <- 0.9


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 0.05) par[["Kappa"]]   <- 0.05
    if (par[["Gamma"]]   > 2)    par[["Gamma"]]   <- 2
    if (par[["Theta"]]   > 2)    par[["Theta"]]   <- 2
    if (par[["Omega"]]   > 0.01) par[["Omega"]]   <- 0.05
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

GetErrorConstrained(par)

GetRealPar(par)

optim(par, GetErrorConstrained)

testResult <- c(
    Rho     = 0.01644884,
    Epsilon = 101.10953770,
    Kappa   = 4.39250933,
    Gamma   = 0.84941599,
    Theta   = 0.49302572,
    Omega   = 6.27621235,
    p       = 0.17461624,
    q       = 1.24494045
)

GetErrorConstrained(testResult)

GetRealPar <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 90)   par[["Epsilon"]] <- 90
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.7)  par[["p"]]       <- 0.7
    if (par[["q"]]       < 0.9)  par[["q"]]       <- 0.9


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 0.05) par[["Kappa"]]   <- 0.05
    if (par[["Gamma"]]   > 2)    par[["Gamma"]]   <- 2
    if (par[["Theta"]]   > 2)    par[["Theta"]]   <- 2
    if (par[["Omega"]]   > 0.01) par[["Omega"]]   <- 0.05
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

    par
}

realResult <- GetRealPar(testResult)

calibOut <- GetErrorResult(realResult)

BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)

BuildPHIAPlot(data = calibOut)


################################################################################
# Constrained Optimisation

parRange

lower <- c(
    Rho     = 1e-3,
    Epsilon = 90,
    Kappa   = 1e-3,
    Gamma   = 1e-3,
    Theta   = 1e-3,
    Omega   = 1e-3,
    p       = 0.7,
    q       = 0.9
)

upper <- c(
    Rho     = 1,
    Epsilon = 100,
    Kappa   = 0.05,
    Gamma   = 2,
    Theta   = 2,
    Omega   = 0.01,
    p       = 1,
    q       = 1
)

par >= lower
par <= upper

optim(par = par, fn = GetError, method = "L-BFGS-B", lower = lower, upper = upper)
