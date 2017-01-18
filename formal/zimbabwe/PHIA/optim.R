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

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

# OKAY we should only be passing parameters that we VARY

# optim parameter bounds


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
    if (par[["Omega"]]   > 0.05) par[["Omega"]]   <- 0.05
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
GetError(par)

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
    if (par[["Omega"]]   > 0.05) par[["Omega"]]   <- 0.05
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

    par
}

realResult <- GetRealPar(testResult)
as.matrix(realResult)

GetErrorConstrained(testResult)
GetErrorConstrained(realResult)

calibOut <- GetErrorResult(realResult)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

################################################################################
# Wednesday

GetErrorConstrained <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 90)   par[["Epsilon"]] <- 90
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.5)  par[["p"]]       <- 0.5
    if (par[["q"]]       < 0.9)  par[["q"]]       <- 0.9


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 0.5)  par[["Kappa"]]   <- 0.5
    if (par[["Gamma"]]   > 2)    par[["Gamma"]]   <- 2
    if (par[["Theta"]]   > 2)    par[["Theta"]]   <- 2
    if (par[["Omega"]]   > 0.5)  par[["Omega"]]   <- 0.5
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

res <- optim(par, GetErrorConstrained)
as.matrix(res$par)
res$value

testResult <- c(
    Rho     = -1.290357779,
    Epsilon = 101.583061566,
    Kappa   = 9.674792436,
    Gamma   = 1.319025464,
    Theta   = 0.390643260,
    Omega   = 0.004153525,
    p       = 0.785520885,
    q       = 0.897956715
)

GetErrorConstrained(testResult)
GetErrorConstrained(res$par)

GetRealPar <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 90)   par[["Epsilon"]] <- 90
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.5)  par[["p"]]       <- 0.5
    if (par[["q"]]       < 0.9)  par[["q"]]       <- 0.9


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 0.5)  par[["Kappa"]]   <- 0.5
    if (par[["Gamma"]]   > 2)    par[["Gamma"]]   <- 2
    if (par[["Theta"]]   > 2)    par[["Theta"]]   <- 2
    if (par[["Omega"]]   > 0.5)  par[["Omega"]]   <- 0.5
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

    par
}


realResult <- GetRealPar(testResult)
as.matrix(realResult)

GetErrorConstrained(testResult)
GetErrorConstrained(realResult)

calibOut <- GetErrorResult(realResult)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

################################################################################
# GetErrorCD4

GetErrorCD4 <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1
    p[["s_2"]] <- 1
    p[["s_3"]] <- 1
    p[["s_4"]] <- 1
    p[["s_5"]] <- 1
    p[["s_6"]] <- 1
    p[["s_7"]] <- 1

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

GetErrorCD4(par)

res <- optim(par, GetErrorCD4)
as.matrix(res$par)
res$value

result <- c(
    Rho     = 0.0002390255,
    Epsilon = 102.2647,
    Kappa   = 1.749754,
    Gamma   = 3.581516,
    Theta   = 0.3114793,
    Omega   = 0.006870914,
    p       = 0.8291169,
    q       = 0.9473207
)
# error = 0.01418761
GetErrorCD4(result)
# 400000 in care.

GetErrorResultCD4 <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1
    p[["s_2"]] <- 1
    p[["s_3"]] <- 1
    p[["s_4"]] <- 1
    p[["s_5"]] <- 1
    p[["s_6"]] <- 1
    p[["s_7"]] <- 1

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

calibOut <- GetErrorResultCD4(result)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

GenSinglePowersPlot_Thesis()
GetPowersCascadeData
GetModel

BuildOptimPowersPlot <- function(dat) {

    d <- dat[dat$source == "model" & dat$year == 2015,]

    UNDX <- d[d$indicator == "PLHIV","value"] - d[d$indicator == "PLHIV Diagnosed","value"]
    DX   <- d[d$indicator == "PLHIV Diagnosed","value"] - d[d$indicator == "PLHIV in Care","value"] - d[d$indicator == "PLHIV Pre-ART LTFU","value"] - d[d$indicator == "PLHIV ART LTFU","value"]
    CX   <- d[d$indicator == "PLHIV in Care","value"] - d[d$indicator == "PLHIV on ART","value"]
    PLX  <- d[d$indicator == "PLHIV Pre-ART LTFU","value"]
    TXN  <- d[d$indicator == "PLHIV on ART","value"] - d[d$indicator == "PLHIV Suppressed","value"]
    VS   <- d[d$indicator == "PLHIV Suppressed","value"]
    LX   <- d[d$indicator == "PLHIV ART LTFU","value"]

    res <- c(VS, TXN, CX, DX, UNDX, PLX, LX,
             VS, TXN, CX, DX, PLX, LX,
             VS, TXN, CX,
             VS, TXN,
             VS)

    state <- c("On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Undiagnosed", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)",
               "On ART\n virally suppressed")

    order <- c(rep("All"       ,7),
               rep("Diagnosed" ,6),
               rep("Care"      ,3),
               rep("Treatment" ,2),
               rep("Suppressed",1))

    df <- data.frame(state, res, order)
    df$order <- factor(df$order, levels = c("All", "Diagnosed", "Care", "Treatment", "Suppressed"))
    df$state <- factor(df$state, levels = c("On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Undiagnosed", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART"))
    df

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggOne <- ggplot(df, aes(x = order, y = res, fill = state))
    ggOne <- ggOne + geom_bar(stat = 'identity')
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = p.col, guide = guide_legend(title = ""))
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 13))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 12))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 12))
    ggOne <- ggOne + theme(legend.text = element_text(size = 7))
    ggOne <- ggOne + theme(legend.title = element_text(size = 12))
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + theme(axis.line.y = element_line())
    ggOne <- ggOne + expand_limits(y = round(sum(df[df$order == "All","res"]), -5))
    ggOne
}

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")


################################################################################
#


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
stop("error...")
