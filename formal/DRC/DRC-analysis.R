####################################################################################################
# PEPFAR PHIA FILE
# DRC Cascade Tool Formal Analysis Script
# Model version include CD4 dependency on theta for only those with CD4 <200
####################################################################################################
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard/inst/app")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")

# GLOBAL
MasterName <- "DRC"
MasterData <- GetMasterDataSet(MasterName)

# Making the assumption of treat all from 2016
MasterData$treatment_guidelines[[6]] <- 2016

# I need to do an optim first to understand the parameter bounds

# ---- #
set.seed(100)
# ---- #

# These will be adjusted in due course:
MaxError <- 0.15
MinNumber <- 1000

# After first simulation, run this function (default = 5%)
# MaxError <- find_error_bound(runError, prop = 0.05)

# Define Parameter Range
parRange <- DefineParmRange(
    rho     = c(0.01, 0.05),
    q       = c(0.4, 0.6),
    epsilon = c(100, 100),
    kappa   = c(2, 6),
    gamma   = c(1, 3),
    theta   = c(0, 0.2),
    p       = c(0.01, 0.6),
    omega   = c(0, 0.03)
)

# Rho     = 0.02287549,
# Epsilon = 105.7396,
# Kappa   = 3.883224,
# Gamma   = 1.15771,
# Theta   = 0.08828346,
# Omega   = 0.02574068,
# p       = 2.045676e-05,
# q       = 0.4493568

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
# 70 min runtime

graphics.off(); quartz.options(w = 8, h = 6)
BuildCD4CalibData_Thesis(year = 1, modelOut = modelOut)
quartz.save(file = "../../formal/DRC/fig/cal/CD4-2010.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 6)
BuildCD4CalibData_Thesis(year = 6, modelOut = modelOut)
quartz.save(file = "../../formal/DRC/fig/cal/CD4-2015.pdf", type = "pdf")

# No PHIA for DRC
# graphics.off(); quartz.options(w = 9, h = 4)
# BuildPHIAPlot(data = CalibOut)
# quartz.save(file = "../../formal/DRC/fig/cal/PHIA.pdf", type = "pdf")

####################################################################################################
#### PLOT ####

# Cascade in 2015
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = "../../formal/DRC/fig/cal/cascade-2015.pdf", type = "pdf")

# Error Histogram
graphics.off(); quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(
    runError = runError,
    maxError = MaxError)
quartz.save(file = "../../formal/DRC/fig/cal/calib-hist.pdf", type = "pdf")

# Calibration Detail
graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = "../../formal/DRC/fig/cal/calib-detail.pdf", type = "pdf")

# Parameter Histograms
BuildCalibrationParameterHistGroup_Thesis <- function() {
    ggA <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "rho")
    ggB <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "q")
    ggD <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "kappa")
    ggE <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "gamma")
    ggF <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "theta")
    ggG <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "omega")
    ggH <- BuildCalibrationParamHist_Thesis(pOut = CalibParamOut, param = "p")

    gridExtra::grid.arrange(ggA, ggB, ggD, ggE, ggF, ggG, ggH, ncol = 4, nrow = 2)
}

graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()
quartz.save(file = "../../formal/DRC/fig/cal/par-hist.pdf", type = "pdf")


################################################################################

# DataReviewPlot

BuildDataReviewPlot_Thesis <- function(data) {
    data <- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = data)
    data$indicator <- factor(data$indicator, levels = c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"))
    data$year <- as.numeric(as.character(data$year))
    ggOut <- ggplot(data, aes(x = year, y = value))
    ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
    ggOut <- ggOut + scale_fill_manual(values = brewer.pal(9,"Blues")[3:8])
    # ggOut <- ggOut + scale_fill_brewer(palette = "Accent")
    ggOut <- ggOut + expand_limits(y = 0.5e6)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(
        labels = scales::comma,
        breaks = seq(0, 0.5e6, 1e5),
        expand = c(0, 0))
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2010, 2015, 1), labels = seq(2010, 2015, 1))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(legend.text = element_text(size = 10))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + ylab("Number of persons")
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut
}

graphics.off(); quartz.options(w = 10, h = 4)
BuildDataReviewPlot_Thesis(data = MasterData$calib)
quartz.save(file = "../../formal/DRC/fig/cal/calib-data.pdf", type = "pdf")

# save.image("../../formal/DRC/data.RData")
# Parameter means
round(colMeans(CalibParamOut), 4)

# Parameter values
a <- paste0(round(Quantile_95(CalibParamOut[["rho"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["rho"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["rho"]])[["upper"]], 4), "]")
b <- paste0(round(Quantile_95(CalibParamOut[["q"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["q"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["q"]])[["upper"]], 4), "]")
c <- paste0(round(Quantile_95(CalibParamOut[["epsilon"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["epsilon"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["epsilon"]])[["upper"]], 4), "]")
d <- paste0(round(Quantile_95(CalibParamOut[["kappa"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["kappa"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["kappa"]])[["upper"]], 4), "]")
e <- paste0(round(Quantile_95(CalibParamOut[["gamma"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["gamma"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["gamma"]])[["upper"]], 4), "]")
f <- paste0(round(Quantile_95(CalibParamOut[["theta"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["theta"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["theta"]])[["upper"]], 4), "]")
g <- paste0(round(Quantile_95(CalibParamOut[["p"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["p"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["p"]])[["upper"]], 4), "]")
h <- paste0(round(Quantile_95(CalibParamOut[["omega"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["omega"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["omega"]])[["upper"]], 4), "]")

list(a,b,c,d,e,f,g,h)

# Results from 29-03-17.RData file
# rho "0.0276 [0.0117 to 0.0474]"
# q "0.5023 [0.4113 to 0.588]"
# epsilon "100 [100 to 100]"
# kappa "3.9759 [2.1951 to 5.8066]"
# gamma "2.0258 [1.1084 to 2.8964]"
# theta "0.0801 [0.0539 to 0.0983]"
# p "0.2765 [0.0424 to 0.567]"
# omega "0.0142 [0.0014 to 0.0285]"

Quantile_95(CalibParamOut[["rho"]])
Quantile_95(CalibParamOut[["q"]])
Quantile_95(CalibParamOut[["epsilon"]])
Quantile_95(CalibParamOut[["kappa"]])
Quantile_95(CalibParamOut[["gamma"]])
Quantile_95(CalibParamOut[["theta"]])
Quantile_95(CalibParamOut[["p"]])
Quantile_95(CalibParamOut[["omega"]])

# Mean of over-riding parameter bounds from non-shiny interface

################################################################################
# Projection

AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

# Single Powers Plot
# graphics.off(); quartz.options(w = 8, h = 4)
# GenSinglePowersPlot_Thesis()
# quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")
# quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

# CareCascade Plot
GenCascadePlot_Thesis <- function() {
    t0 <- GetCascadeData(1)   # t0 = 1
    t5 <- GetCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    t0$year <- 2015
    t5$year <- 2020
    out <- rbind(t0, t5)

    ggOne <- ggplot(out, aes(x = def, y = res))
    ggOne <- ggOne + geom_bar(aes(fill = as.factor(year)), position = 'dodge', stat = 'identity')
    ggOne <- ggOne + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max, fill = as.factor(year)), position = position_dodge(width = 0.9), stat = "identity", width = 0.2, size = 0.5)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 9))
    ggOne <- ggOne + scale_fill_manual(values = c(c.fill[2],c.fill[5]), guide = guide_legend(title = ""))
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(title = element_text(size = 13))
    ggOne <- ggOne + theme(axis.title = element_text(size = 10))
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 12))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 12))
    ggOne <- ggOne + theme(axis.ticks.x = element_blank())
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(legend.title = element_text(size = 12))
    ggOne <- ggOne + theme(legend.text = element_text(size = 12))
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(axis.line.y = element_line())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + ylab("Number of persons")
    ggOne <- ggOne + expand_limits(y = round(max(out$max), digits = -5) + 1e5)
    ggOne
}

graphics.off(); quartz.options(w = 10, h = 4)
GenCascadePlot_Thesis()
# quartz.save(file = "~/Desktop/fig/cascade-projection.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/cascade-projection.pdf", type = "pdf")

# What is the average value of beta?
# OutputBeta()

# 90-90-90 Plot
graphics.off(); quartz.options(w = 9, h = 4)
Gen909090Plot_Thesis()
# quartz.save(file = "~/Desktop/fig/90-90-90.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/90-90-90.pdf", type = "pdf")

# Get 90-90-90 numbers
out <- Get909090Data()
DRC_out <- out
save(DRC_out, file = "../../formal/DRC/UNAIDS-90-90-90.RData")

# Powers Plot
graphics.off(); quartz.options(w = 15, h = 4)
GenPowersCascadePlot_Thesis()
# quartz.save(file = "~/Desktop/fig/cascade-powers.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/cascade-powers.pdf", type = "pdf")

# New Infections
GenNewInfPlot_Thesis <- function() {
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

    c.fill <- rev(brewer.pal(9,"Blues")[4:8])

    ggOut <- ggplot(df, aes(x = timeOut, NewInf))
    ggOut <- ggOut + geom_bar(stat = "identity", size = 2, fill = c.fill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + expand_limits(y = round(max(df$max), digits = -4))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2019, 1), labels = seq(2015, 2019, 1))
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + ylab("New Infections Per Year")
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.title.x =  element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 11))
    ggOut <- ggOut + expand_limits(y = 25e3)
    ggOut
}
graphics.off(); quartz.options(w = 6, h = 4)
GenNewInfPlot_Thesis()
# quartz.save(file = "~/Desktop/fig/new-infections.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/new-infections.pdf", type = "pdf")

# AIDS Deaths
GenAidsDeathsPlot_Thesis <- function() {
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

    c.fill <- rev(brewer.pal(9,"Blues")[4:8])

    ggOut <- ggplot(df, aes(x = timeOut, HivMortality))
    ggOut <- ggOut + geom_bar(stat = "identity", size = 2, fill = c.fill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + expand_limits(y = round(max(df$max), digits = -4))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2019, 1), labels = seq(2015, 2019, 1))
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + ylab("AIDS Deaths Per Year")
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(axis.title.x =  element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 11))
    ggOut <- ggOut + expand_limits(y = 3e4)
    ggOut
}
graphics.off(); quartz.options(w = 6, h = 4)
GenAidsDeathsPlot_Thesis()
# quartz.save(file = "~/Desktop/fig/AIDS-deaths.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/AIDS-deaths.pdf", type = "pdf")

# Discrete Cascade
graphics.off(); quartz.options(w = 10, h = 4)
GenDiscreteCascade_Thesis()
# quartz.save(file = "~/Desktop/fig/cascade-discrete.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/cascade-discrete.pdf", type = "pdf")

# CD4 distribution at ART initiation
graphics.off(); quartz.options(w = 8, h = 6)
BuildCD4Data_Thesis(year = 251)
# quartz.save(file = "~/Desktop/fig/CD4-2020.pdf", type = "pdf")
quartz.save(file = "../../formal/DRC/fig/pro/CD4-2020.pdf", type = "pdf")

# NUMBERS
t0 <- GetCascadeData(1)   # t0 = 1
t5 <- GetCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

# PLHIV Estimate in 2015
scales::comma(round(t0$res[1], -3))
1.394e+6
scales::comma(round(t5$res[1], -3))
1.516e+6

1.516e+6 / 1.394e+6

# DIAGNOSED
scales::comma(round(t0$res[2], -3))
1.044e+6
scales::comma(round(t5$res[2], -3))
1.296e+6

1.296e+6 / 1.044e+6

# On ART
scales::comma(round(t0$res[4], -3))
scales::comma(round(t5$res[4], -3))

round(t5$res[4], -3) / round(t0$res[4], -3)
37\% (0.899M to 1.233M)

# Viral suppression
scales::comma(round(t0$res[5], -3))
scales::comma(round(t5$res[5], -3))

round(t5$res[5], -3) / round(t0$res[5], -3)
40\% (0.791M to 1.104M)
23 - 14
t0

t0$res[5]/t0$res[1]
t5$res[5]/t5$res[1]

# edit on the 24/01/17 for new model that only has a CD4 dependency on theta for persons with CD4
# <200

# Current as of 29/03/17
# save.image("../../formal/DRC/data-29-03-17.RData")
# load("../../formal/DRC/data-29-03-17.RData")

####################################################################################################
# if we were to adjust 'beta' such that it is reduced by 50%, then what functions would need to be
# adjusted? Remember, that we only wanted to adjust transmission between 2015 to 2020, not during
# calibration.

# Projection:
# GetModel()

# Optimisation:
# RunNSOptimisation()

# What about the baselineModel function that RunNSOptimisation() calls?


####################################################################################################
# Optimisation

intSwitch <- data.frame(
    testing      = TRUE,
    linkage      = TRUE,
    preRetention = TRUE,
    initiation   = TRUE,
    adherence    = TRUE,
    retention    = TRUE
)

# These need updating
# and re-write this definition, it looks vile
# OptInput <- c()
# OptInput$intValue_rho   <- parRange["rho", "max"]
# OptInput$intValue_q     <- parRange["q", "max"]
# OptInput$intValue_kappa <- parRange["kappa", "min"]
# OptInput$intValue_gamma <- parRange["gamma", "max"]
# OptInput$intValue_sigma <- 0.5
# OptInput$intValue_omega <- parRange["omega", "min"]

OptInput <- c()
OptInput$intValue_rho   <- 2
OptInput$intValue_q     <- 1
OptInput$intValue_kappa <- 0
OptInput$intValue_gamma <- 20
OptInput$intValue_sigma <- 0.5
OptInput$intValue_omega <- 0

reactiveCost <- data.frame(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

# This is used by the function 'AdjustHIVTetsCost'
SafeReactiveCost <- data.frame(
    test = 10,
    link = 40,
    care = 40,
    art = 367
)

custom <- data.frame(target = 0.9^3)

AdvCalib <- data.frame(NatMort = 0.005, HIVMort = 1)

reactiveAdjustCost <- data.frame(switch = TRUE)

intLength = 2

AdjustHIVTestCost()

theOut <- RunNSOptimisation(propRuns = 0.1, intLength = intLength)

# Frontier Plot (optResults comes from RunNSOptimisation
graphics.off(); quartz.options(w = 8, h = 4)
BuildFrontierPlot_Thesis(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3)
quartz.save(file = "../../formal/DRC/fig/opt/frontier.pdf", type = "pdf")

# FIGURE GENERATION
graphics.off(); quartz.options(w = 8, h = 4)
BuildChangesPlot_Thesis(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3)
quartz.save(file = "../../formal/DRC/fig/opt/changes.pdf", type = "pdf")

################################################################################
# TABULATE RESULTS #

simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength, target = custom$target)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

intRes <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = custom$target)
colMeans(intRes)

results <- intRes[,c("iTest","iLink","iPreR","iInit","iAdhr","iRetn")]
results$iPreR <- abs(results$iPreR)
results$iRetn <- abs(results$iRetn)
results[results$iTest < 0, "iTest"] <- 0
results[results$iLink < 0, "iLink"] <- 0
results[results$iInit < 0, "iInit"] <- 0
results[results$iAdhr < 0, "iAdhr"] <- 0
results$run <- 1:dim(results)[1]

resTable <- results

resTable[resTable$iTest < 0, "iTest"] <- 0
resTable[resTable$iLink < 0, "iLink"] <- 0
resTable[resTable$iInit < 0, "iInit"] <- 0
resTable[resTable$iAdhr < 0, "iAdhr"] <- 0

colMeans(resTable)

# BASELINE
b1 <- paste0("iCost = ", scales::dollar(Quantile_95(BaselineCost)["mean"] / 5), " [", scales::dollar(Quantile_95(BaselineCost)["lower"] / 5), " to ", scales::dollar(Quantile_95(BaselineCost)["upper"] / 5), "]")
b2 <- paste0("iTest = ", scales::comma(round(Quantile_95(BaselineTest)["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(BaselineTest)["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(BaselineTest)["upper"] / 5, 0)), "]")
b3 <- paste0("iLink = ", scales::comma(round(Quantile_95(BaselineLink)["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(BaselineLink)["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(BaselineLink)["upper"] / 5, 0)), "]")
b4 <- paste0("iPreR = ", scales::comma(round(Quantile_95(BaselinePreR)["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(BaselinePreR)["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(BaselinePreR)["upper"] / 5, 0)), "]")
b5 <- paste0("iInit = ", scales::comma(round(Quantile_95(BaselineInit)["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(BaselineInit)["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(BaselineInit)["upper"] / 5, 0)), "]")
b6 <- paste0("iAdhr = ", scales::comma(round(Quantile_95(BaselineAdhr)["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(BaselineAdhr)["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(BaselineAdhr)["upper"] / 5, 0)), "]")
b7 <- paste0("iRetn = ", scales::comma(round(Quantile_95(BaselineRetn)["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(BaselineRetn)["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(BaselineRetn)["upper"] / 5, 0)), "]")

b1
b2
b3
b4
b5
b6
b7

# Results from 29-03-17.RData calibration
# "iCost = $52,028,104 [$40,278,497 to $61,593,505]"
# "iTest = 23,755 [19,390 to 29,014]"
# "iLink = 21,320 [16,740 to 26,142]"
# "iPreR = 4,100 [1,788 to 6,799]"
# "iInit = 25,298 [18,346 to 30,274]"
# "iAdhr = 7,000 [1,223 to 14,347]"
# "iRetn = 1,924 [284 to 3,846]"

# INTERVENTIONS
round(Quantile_95(intRes[,"iCost"])["mean"] / 1e6, 2)
i1 <- paste0("iCost = ", scales::dollar(Quantile_95(intRes[,"iCost"])["mean"] / 5), " [", scales::dollar(Quantile_95(intRes[,"iCost"])["lower"] / 5), " to ", scales::dollar(Quantile_95(intRes[,"iCost"])["upper"] / 5), "]")
i2 <- paste0("iTest = ", scales::comma(round(Quantile_95(resTable[,"iTest"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iTest"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iTest"])["upper"] / 5, 0)), "]")
i3 <- paste0("iLink = ", scales::comma(round(Quantile_95(resTable[,"iLink"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iLink"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iLink"])["upper"] / 5, 0)), "]")
i4 <- paste0("iPreR = ", scales::comma(round(Quantile_95(resTable[,"iPreR"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iPreR"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iPreR"])["upper"] / 5, 0)), "]")
i5 <- paste0("iInit = ", scales::comma(round(Quantile_95(resTable[,"iInit"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iInit"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iInit"])["upper"] / 5, 0)), "]")
i6 <- paste0("iAdhr = ", scales::comma(round(Quantile_95(resTable[,"iAdhr"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iAdhr"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iAdhr"])["upper"] / 5, 0)), "]")
i7 <- paste0("iRetn = ", scales::comma(round(Quantile_95(resTable[,"iRetn"])["mean"] / 5, 0)), " [", scales::comma(round(Quantile_95(resTable[,"iRetn"])["lower"] / 5, 0)), " to ", scales::comma(round(Quantile_95(resTable[,"iRetn"])["upper"] / 5, 0)), "]")

i1
i2
i3
i4
i5
i6
i7

# Results from 29-03-17.RData calibration
# "iCost = $71,868,939 [$54,886,611 to $92,014,301]"
# "iTest = 22,128 [15,202 to 31,883]"
# "iLink = 24,547 [17,625 to 34,275]"
# "iPreR = 4,089 [1,782 to 6,805]"
# "iInit = 24,432 [17,559 to 33,982]"
# "iAdhr = 42,399 [29,841 to 53,913]"
# "iRetn = 1,921 [284 to 3,846]"

Quantile_95(resTable[,"iTest"]) / 5
Quantile_95(resTable[,"iLink"]) / 5
Quantile_95(resTable[,"iPreR"]) / 5
Quantile_95(resTable[,"iInit"]) / 5
Quantile_95(resTable[,"iAdhr"]) / 5
Quantile_95(resTable[,"iRetn"]) / 5
Quantile_95(intRes[,"iTCst"]) / 5

################################################################################
# SAVE IMAGE #

# save.image("../../formal/DRC/data.RData")
# save.image("../../formal/DRC/big-data.RData")

################################################################################
# PEPFAR PHIA 2016 Analysis


Get909090Data_2016 <- function(yr) {
    result <- GetModel()

    # Always aiming for 2020 here (5.02 / 0.02)
    # year <- 251
    year <- ((yr - 2015) / 0.02)

    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    UN_90 <- Quantile_95(DX_data / NX_data)
    UN_9090 <- Quantile_95(TX_data / DX_data)
    UN_909090 <- Quantile_95(VS_data / TX_data)

    res <- c(UN_90[["mean"]], UN_9090[["mean"]], UN_909090[["mean"]])
    min <- c(UN_90[["lower"]], UN_9090[["lower"]], UN_909090[["lower"]])
    max <- c(UN_90[["upper"]], UN_9090[["upper"]], UN_909090[["upper"]])
    def <- c("Diagnosed / PLHIV", "On Treatment / Diagnosed", "Virally Suppressed / On Treatment")
    out <- data.frame(def, res, min, max)
    out$def <- factor(out$def, levels = c("Diagnosed / PLHIV", "On Treatment / Diagnosed", "Virally Suppressed / On Treatment"))
    out
}

test <- Get909090Data_2016(yr = 2016)

Gen909090Plot_2016 <- function(yr) {
    out    <- Get909090Data_2016(yr = yr)

    cfill <- rev(brewer.pal(9,"Blues")[6:8])

    vbOut1 <- round(out[out$def == "Diagnosed / PLHIV",    "res"] * 100, digits = 0)
    vbOut2 <- round(out[out$def == "On Treatment / Diagnosed", "res"] * 100, digits = 0)
    vbOut3 <- round(out[out$def == "Virally Suppressed / On Treatment",   "res"] * 100, digits = 0)

    ggOut <- ggplot(out, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 0.5)
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
    ggOut <- ggOut + scale_fill_manual(values = cfill)
    ggOut <- ggOut + geom_abline(intercept = 0.9, slope = 0)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 20))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + geom_label(aes(x = def, label = scales::percent(round(out$res, digits = 2))), size = 4)
    ggOut
}

Gen909090Plot_2016(yr = 2016)


###

head(CalibOut)

test <- CalibOut

plhiv_2010 <- mean(test[test$source == "model" & test$indicator == "PLHIV" & test$year == 2010, "value"])
plhiv_2011 <- mean(test[test$source == "model" & test$indicator == "PLHIV" & test$year == 2011, "value"])
plhiv_2012 <- mean(test[test$source == "model" & test$indicator == "PLHIV" & test$year == 2012, "value"])
plhiv_2013 <- mean(test[test$source == "model" & test$indicator == "PLHIV" & test$year == 2013, "value"])
plhiv_2014 <- mean(test[test$source == "model" & test$indicator == "PLHIV" & test$year == 2014, "value"])
plhiv_2015 <- mean(test[test$source == "model" & test$indicator == "PLHIV" & test$year == 2015, "value"])

plhiv <- c(plhiv_2010, plhiv_2011, plhiv_2012, plhiv_2013, plhiv_2014, plhiv_2015)

inc <- as.double(MasterData$incidence[2,3:8])

inc

rate <- inc / plhiv

year <- seq(2010, 2015, 1)

df <- data.frame(year, rate)

graphics.off(); quartz.options(w = 5, h = 3)
ggplot(df, aes(x = year, y = rate)) +
geom_line() +
ggtitle("Transmission Rate", subtitle = "New Infections / Total PLHIV")
quartz.save(file = "~/Desktop/fig/transmission-rate.pdf", type = "pdf")


####################################################################################################
####################################################################################################
# CROI 2017 FIGURE GENERATION

# Baseline Changes Figure
build_changes_baseline_CROI <- function() {

    theBase <-  rbind(
        data.frame(variable = "Testing",            mean = mean(BaselineTest) / 5, strategy = "Baseline"),
        data.frame(variable = "Linkage",            mean = mean(BaselineLink) / 5, strategy = "Baseline"),
        data.frame(variable = "Pre-ART\nRetention", mean = mean(BaselinePreR) / 5, strategy = "Baseline"),
        data.frame(variable = "ART\nInitiation",    mean = mean(BaselineInit) / 5, strategy = "Baseline"),
        data.frame(variable = "ART\nRetention",     mean = mean(BaselineRetn) / 5, strategy = "Baseline"),
        data.frame(variable = "Viral\nSuppression", mean = mean(BaselineAdhr) / 5, strategy = "Baseline")
        )

    theBase$upper <- NA
    theBase$lower <- NA

    theBase$variable <- factor(theBase$variable, levels = c("Testing", "Linkage",
       "Pre-ART\nRetention", "ART\nInitiation", "ART\nRetention", "Viral\nSuppression"))

    # make retention values NEGATIVE
    theBase[theBase$variable == "Pre-ART\nRetention", "mean"] <- - theBase[theBase$variable ==
    "Pre-ART\nRetention", "mean"]

    theBase[theBase$variable == "ART\nRetention", "mean"] <- - theBase[theBase$variable ==
    "ART\nRetention", "mean"]


    ggOut <- ggplot(theBase, aes(x = variable, y = mean, fill = strategy))
    ggOut <- ggOut + geom_bar(stat = "identity", alpha = 1)
    ggOut <- ggOut + scale_fill_manual(values = "#E41A1C")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ylab("Annual Movement Through Care (2015 to 2020)")
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + scale_y_continuous(limits = c(-1e4, 8e4), labels = scales::comma, breaks =
       scales::pretty_breaks(n = 8), expand = c(0, 0))
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut <- ggOut + theme(legend.position = 'right')
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
    ggOut <- ggOut + geom_hline(yintercept = 0)
    ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut
}

graphics.off(); quartz.options(w = 8, h = 4)
build_changes_baseline_CROI()
quartz.save(file = "../../formal/DRC/fig/opt/changes-baseline.pdf", type = "pdf")

# handy invert function
invert <- function(x) {
    if (x <= 0) {
        x <- abs(x)
    } else {
        x <- - x
    }
    x
}

# Complete Changes Figures
build_changes_CROI <- function(CalibParamOut, optResults, target) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength, target = target)
    frontierList <- GetFrontiers(simData = optResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = optResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = target)

    # Result Formatting
    intResult <- intResult[,c("iTest","iLink","iPreR","iInit","iAdhr","iRetn")]
    intResult['iPreR'] <- invert(intResult['iPreR'])
    intResult['iRetn'] <- invert(intResult['iRetn'])
    intResult[intResult$iTest < 0, 'iTest'] <- 0
    intResult[intResult$iLink < 0, 'iLink'] <- 0
    intResult[intResult$iInit < 0, 'iInit'] <- 0
    intResult[intResult$iAhdr < 0, 'iAdhr'] <- 0

    # Assign a 'run' number to simulations
    intResult$run <- 1:dim(intResult)[1]

    # Melt them
    mRes <- reshape2::melt(intResult, id = "run")

    ## DIVIDE ALL VALUES BY FIVE
    # Conversion from 5 year values to single years
    mRes$value <- mRes$value / 5

    # RENAME VARIABLES
    mRes$variable <- as.character(mRes$variable)
    mRes[mRes$variable == "iTest", "variable"] <- "Number of\nPLHIV to\nDiagnose"
    mRes[mRes$variable == "iLink", "variable"] <- "Number of\nPLHIV to\nLink"
    mRes[mRes$variable == "iPreR", "variable"] <- "Number of\nPLHIV to\nRetain in\nPre-ART Care"
    mRes[mRes$variable == "iInit", "variable"] <- "Number of\nPLHIV to\nInitiate ART"
    mRes[mRes$variable == "iAdhr", "variable"] <- "Number of\nPLHIV to\nMaintain Viral\nSuppression"
    mRes[mRes$variable == "iRetn", "variable"] <- "Number of\nPLHIV to\nRetain on\nART"

    mRes$variable <- factor(mRes$variable, levels = c("Number of\nPLHIV to\nDiagnose", "Number of\nPLHIV to\nLink", "Number of\nPLHIV to\nRetain in\nPre-ART Care", "Number of\nPLHIV to\nInitiate ART",
        "Number of\nPLHIV to\nRetain on\nART", "Number of\nPLHIV to\nMaintain Viral\nSuppression"))

    # EDITS FROM HERE
    variable <- c("Number of\nPLHIV to\nDiagnose", "Number of\nPLHIV to\nLink",
        "Number of\nPLHIV to\nRetain in\nPre-ART Care", "Number of\nPLHIV to\nInitiate ART", "Number of\nPLHIV to\nRetain on\nART", "Number of\nPLHIV to\nMaintain Viral\nSuppression")

    mean <- c(
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nDiagnose", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nLink", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nRetain in\nPre-ART Care", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nInitiate ART", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nRetain on\nART", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nMaintain Viral\nSuppression", "value"])[["mean"]]
    )

    upper <- c(
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nDiagnose", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nLink", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nRetain in\nPre-ART Care", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nInitiate ART", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nRetain on\nART", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nMaintain Viral\nSuppression", "value"])[["upper"]]
    )

    lower <- c(
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nDiagnose", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nLink", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nRetain in\nPre-ART Care", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nInitiate ART", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nRetain on\nART", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nMaintain Viral\nSuppression", "value"])[["lower"]]
    )

    strategy <- "Intervention"

    outData <- data.frame(variable, mean, lower, upper, strategy)

    # baseline data.frame
    theBase <-  rbind(
        data.frame(variable = "Number of\nPLHIV to\nDiagnose",
                   mean = mean(BaselineTest) / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nLink",
                   mean = mean(BaselineLink) / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nRetain in\nPre-ART Care",
                   mean = mean(BaselinePreR) / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nInitiate ART",
                   mean = mean(BaselineInit) / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nRetain on\nART",
                   mean = mean(BaselineRetn) / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nMaintain Viral\nSuppression",
                   mean = mean(BaselineAdhr) / 5,
                   strategy = "Baseline")
        )

    theBase$upper <- NA
    theBase$lower <- NA

    final <- rbind(theBase, outData)
    final$strategy <- factor(final$strategy, levels = c("Intervention", "Baseline"))

    # Now we need the error_bar data.frame
    value <- mean
    mean <- mean + theBase$mean
    upper <- upper + theBase$mean
    lower <- lower + theBase$mean
    theLabel <- data.frame(variable, value, mean, upper, lower)

    ggOut <- ggplot(final, aes(x = variable, y = mean, fill = strategy))
    ggOut <- ggOut + geom_bar(stat = "identity", alpha = 1)
    ggOut <- ggOut + geom_hline(yintercept = 0)
    ggOut <- ggOut + geom_errorbar(data = theLabel, aes(x = variable, y = mean, ymax = upper, ymin = lower), alpha = 1, width = 0.25, size = 0.5)
    ggOut <- ggOut + geom_label(data = theLabel, aes(x = variable, y = mean, label = paste0("+",
       scales::comma(round(value, 0)))), vjust = c(0.5, 0.5, 0, 0.5, 0.5, 0.5), family = "Avenir Next", colour = "black", size = 3, alpha = 1, show.legend = FALSE)
    ggOut <- ggOut + scale_fill_manual(values = c("#4F8ABA","#E41A1C"))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ylab("Annual Targets")
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 11e4), labels = scales::comma, breaks =
       scales::pretty_breaks(n = 11), expand = c(0, 0))
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut <- ggOut + theme(legend.position = 'right')
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
    ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut
}

graphics.off(); quartz.options(w = 8, h = 4)
build_changes_CROI(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3)
quartz.save(file = "../../formal/DRC/fig/opt/changes.pdf", type = "pdf")

build_custom_frontier_plot <- function(CalibParamOut, optResults, target = target, ylim) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]

    optRuns <- WhichAchieved73(simData = optResults, simLength = simLength, target = target)

    optResults$sim <- rep(x = 1:(dim(optResults)[1] / simLength), each = simLength)

    allRuns <- GetFrontiers(simData = optResults, optRuns = 1:(dim(optResults)[1] / simLength), simLength = simLength)

    interpol <- list()
    for(n in 1:(dim(optResults)[1] / simLength)) {
        lower <- (1 + simLength * (n - 1))
        upper <- (simLength + simLength * (n - 1))
        vals <- optResults[lower:upper,]

        interpolation <- approx(x = vals[,"VS"][allRuns[[n]]], y = vals[,"Cost"][allRuns[[n]]])
        interpol[[n]] <- interpolation
    }

    ggPlot <- ggplot(optResults, aes(x = VS, y = Cost))
    ggPlot <- ggPlot + geom_point(col = '#4F8ABA', alpha = 0.2)
    for(n in 1:(dim(optResults)[1] / simLength)) {
        ggPlot <- ggPlot + geom_line(data = as.data.frame(interpol[[n]]), mapping = aes(x = x, y = y), col = 'black', alpha = 0.2, size = 0.5)
    }
    for(n in 1:length(optRuns)) {
        ggPlot <- ggPlot + geom_line(data = as.data.frame(interpol[[optRuns[n]]]), mapping = aes(x = x, y = y), col = "red", alpha = 0.5, size = 0.75)
    }
    ggPlot <- ggPlot + geom_vline(xintercept = 0.9^3, alpha = 1)
    ggPlot <- ggPlot + theme_classic()
    ggPlot <- ggPlot + expand_limits(y = ylim)
    ggPlot <- ggPlot + scale_y_continuous(labels = scales::scientific, breaks = scales::pretty_breaks(n = 5))
    ggPlot <- ggPlot + scale_x_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 5))
    ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 10))
    ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 10))
    ggPlot <- ggPlot + theme(axis.title = element_text(size = 11))
    ggPlot <- ggPlot + theme(axis.line.x = element_line())
    ggPlot <- ggPlot + theme(axis.line.y = element_line())
    ggPlot <- ggPlot + xlab("Viral Suppression")
    ggPlot <- ggPlot + ylab("Additional Cost of Care")
    ggPlot <- ggPlot + theme(text = element_text(family = figFont))
    ggPlot
}

graphics.off(); quartz.options(w = 8, h = 4)
build_custom_frontier_plot(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3,
    ylim = 2.5e8)
quartz.save(file = "../../formal/DRC/fig/opt/frontier.pdf", type = "pdf")

####################################################################################################
### PAPER CHANGE FIGURE (Adapted from version presented at CROI 2017)

# new variable: absResults
# this maps the ABSOLUTE changes in care (not those relative to baseline) in the optResults df

build_changes_paper_figure <- function(target = 0.9^3) {

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]
    optRuns <- WhichAchieved73(simData = absResults, simLength = simLength, target = target)
    frontierList <- GetFrontiers(simData = absResults, optRuns = optRuns, simLength = simLength)
    intResult <- RunInterpolation(simData = absResults, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = target)

    # Result Formatting
    intResult <- intResult[,c("iTest","iLink","iPreR","iInit","iAdhr","iRetn")]

    # Assign a 'run' number to simulations
    intResult$run <- 1:dim(intResult)[1]

    # Melt them
    mRes <- reshape2::melt(intResult, id = "run")

    ## DIVIDE ALL VALUES BY FIVE
    # Conversion from 5 year values to single years
    mRes$value <- mRes$value / 5

    # RENAME VARIABLES
    mRes$variable <- as.character(mRes$variable)
    mRes[mRes$variable == "iTest", "variable"] <- "Number of\nPLHIV to\nDiagnose"
    mRes[mRes$variable == "iLink", "variable"] <- "Number of\nPLHIV to\nLink"
    mRes[mRes$variable == "iPreR", "variable"] <- "Number of\nPLHIV\nLost From\nPre-ART Care"
    mRes[mRes$variable == "iInit", "variable"] <- "Number of\nPLHIV to\nInitiate ART"
    mRes[mRes$variable == "iAdhr", "variable"] <- "Number of\nPLHIV to\nMaintain Viral\nSuppression"
    mRes[mRes$variable == "iRetn", "variable"] <- "Number of\nPLHIV\nLost From\nART Care"

    mRes$variable <- factor(mRes$variable, levels = c("Number of\nPLHIV to\nDiagnose", "Number of\nPLHIV to\nLink", "Number of\nPLHIV\nLost From\nPre-ART Care", "Number of\nPLHIV to\nInitiate ART",
        "Number of\nPLHIV\nLost From\nART Care", "Number of\nPLHIV to\nMaintain Viral\nSuppression"))

    # EDITS FROM HERE
    variable <- c("Number of\nPLHIV to\nDiagnose", "Number of\nPLHIV to\nLink",
        "Number of\nPLHIV\nLost From\nPre-ART Care", "Number of\nPLHIV to\nInitiate ART", "Number of\nPLHIV\nLost From\nART Care", "Number of\nPLHIV to\nMaintain Viral\nSuppression")

    mean <- c(
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nDiagnose", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nLink", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV\nLost From\nPre-ART Care", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nInitiate ART", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV\nLost From\nART Care", "value"])[["mean"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nMaintain Viral\nSuppression", "value"])[["mean"]]
    )

    upper <- c(
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nDiagnose", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nLink", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV\nLost From\nPre-ART Care", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nInitiate ART", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV\nLost From\nART Care", "value"])[["upper"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nMaintain Viral\nSuppression", "value"])[["upper"]]
    )

    lower <- c(
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nDiagnose", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nLink", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV\nLost From\nPre-ART Care", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nInitiate ART", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV\nLost From\nART Care", "value"])[["lower"]],
        Quantile_95(mRes[mRes$variable == "Number of\nPLHIV to\nMaintain Viral\nSuppression", "value"])[["lower"]]
    )

    strategy <- "Intervention"

    outData <- data.frame(variable, mean, lower, upper, strategy)

    # baseline data.frame
    theBase <- rbind(
        data.frame(variable = "Number of\nPLHIV to\nDiagnose",
                   mean  = Quantile_95(BaselineTest)[["mean"]]  / 5,
                   lower = Quantile_95(BaselineTest)[["lower"]] / 5,
                   upper = Quantile_95(BaselineTest)[["upper"]] / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nLink",
                   mean  = Quantile_95(BaselineLink)[["mean"]]  / 5,
                   lower = Quantile_95(BaselineLink)[["lower"]] / 5,
                   upper = Quantile_95(BaselineLink)[["upper"]] / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV\nLost From\nPre-ART Care",
                   mean  = Quantile_95(BaselinePreR)[["mean"]]  / 5,
                   lower = Quantile_95(BaselinePreR)[["lower"]] / 5,
                   upper = Quantile_95(BaselinePreR)[["upper"]] / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nInitiate ART",
                   mean  = Quantile_95(BaselineInit)[["mean"]]  / 5,
                   lower = Quantile_95(BaselineInit)[["lower"]] / 5,
                   upper = Quantile_95(BaselineInit)[["upper"]] / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV\nLost From\nART Care",
                   mean  = Quantile_95(BaselineRetn)[["mean"]]  / 5,
                   lower = Quantile_95(BaselineRetn)[["lower"]] / 5,
                   upper = Quantile_95(BaselineRetn)[["upper"]] / 5,
                   strategy = "Baseline"),
        data.frame(variable = "Number of\nPLHIV to\nMaintain Viral\nSuppression",
                   mean  = Quantile_95(BaselineAdhr)[["mean"]]  / 5,
                   lower = Quantile_95(BaselineAdhr)[["lower"]] / 5,
                   upper = Quantile_95(BaselineAdhr)[["upper"]] / 5,
                   strategy = "Baseline")
        )

    final <- rbind(theBase, outData)
    final$strategy <- factor(final$strategy, levels = c("Baseline", "Intervention"))

    # WHAT I NEED
    # -> Single data.frame containing 'baseline' 'intervention'
    # -> cols = Intervention, mean, upper, lower
    # -> IGNORE geom_label for now (I think it is too complicated with a geom_label call)
    # need to re-run the simulations and then spit out ANOTHER data.frame that just shows things like
    # the baseline changes but for interventions

    # Leave option for labels
    # ggOut <- ggOut + geom_label(data = theLabel, aes(x = variable, y = mean, label = paste0("+",
    # scales::comma(round(value, 0)))), vjust = c(0.5, 0.5, 0, 0.5, 0.5, 0.5), family = "Avenir
    # Next", colour = "black", size = 3, alpha = 1, show.legend = FALSE)

    ggOut <- ggplot(final, aes(x = variable, y = mean, fill = strategy))
    ggOut <- ggOut + geom_bar(position = "dodge", stat = "identity", alpha = 1)
    ggOut <- ggOut + geom_errorbar(aes(x = variable, ymax = upper, ymin = lower), position =
        position_dodge(0.9), alpha = 1, width = 0.25, size = 0.5)
    ggOut <- ggOut + scale_fill_manual(values = c("#E41A1C","#4F8ABA"))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + ylab("Annual Targets")
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 8))
    ggOut <- ggOut + theme(axis.title.x = element_blank())
    ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 12e4), labels = scales::comma, breaks =
       scales::pretty_breaks(n = 12), expand = c(0, 0))
    ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
    ggOut <- ggOut + theme(legend.position = 'right')
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
    ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut
}

graphics.off(); quartz.options(w = 8, h = 4)
build_changes_paper_figure()
quartz.save(file = "../../formal/DRC/fig/opt/changes-dodge.pdf", type = "pdf")
