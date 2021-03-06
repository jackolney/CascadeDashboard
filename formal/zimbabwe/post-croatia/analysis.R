# POST CROATIA FILE
# Zimbabwe Cascade Tool Formal Analysis Script
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard/inst/app")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")

# GLOBAL
MasterName <- "Zimbabwe"
MasterData <- GetMasterDataSet(MasterName)

# EDIT TO INCLUDE DATA DEPOSITED DURING CROATIA MEETING

# New Data
new_plhiv <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV",            value = 1425762, weight = "green", source = "Spectrum")
# new_diag  <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV Diagnosed",  value = 908210,  weight = "red",   source = "Adjusted 2010 DHS")
new_care  <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV in Care",    value = 903011,  weight = "amber", source = "Program Data (adjusted ART data)")
new_art   <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV on ART",     value = 879271,  weight = "amber", source = "Program Data (DHIS)")
new_supp  <- new_data(country = "Zimbabwe", year = 2015, indicator = "PLHIV Suppressed", value = 782551,  weight = "red",   source = "Surveillance")

MasterData$calib <- replace_or_append(datOne = MasterData$calib, datTwo = new_plhiv)
# MasterData$calib <- replace_or_append(datOne = MasterData$calib, datTwo = new_diag)
MasterData$calib <- replace_or_append(datOne = MasterData$calib, datTwo = new_care)
MasterData$calib <- replace_or_append(datOne = MasterData$calib, datTwo = new_art)
MasterData$calib <- replace_or_append(datOne = MasterData$calib, datTwo = new_supp)

# Ignoring 'PLHIV Diagnosed' value as it was an outdated value from DHS in 2010 (we have a slightly different adjusted version from Mary Mahy)

MasterData$calib

# ---- #
set.seed(100)
# ---- #

MaxError <- 0.06
MinNumber <- 1000

# Define Parameter Range
# function can now be edited
# e.g. DefineParmRange(p = c(0, 1))
# parRange <- DefineParmRange()

# parRange <- DefineParmRange(p = c(0.86, 1), omega = c(0, 0.01))
parRange <- DefineParmRange(p = c(0.7, 1), omega = c(0, 0.01))

# Run Calibration
RunNSCalibration(
    country = MasterName,
    data = MasterData,
    maxIterations = 1e4,
    maxError = MaxError,
    limit = MinNumber,
    parRange = parRange,
    targetIterations = 1e5)

# Cascade in 2015
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/cal/cascade-2015.pdf", type = "pdf")

# Error Histogram
graphics.off(); quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(
    runError = runError,
    maxError = MaxError)
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/cal/calib-hist.pdf", type = "pdf")

# Calibration Detail
graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = CalibOut,
    originalData = MasterData,
    limit = MinNumber)
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/cal/calib-detail.pdf", type = "pdf")

# Parameter Histograms
graphics.off(); quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/cal/par-hist.pdf", type = "pdf")

# DataReviewPlot
graphics.off(); quartz.options(w = 10, h = 4)
BuildDataReviewPlot_Thesis(data = MasterData$calib)
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/cal/calib-data.pdf", type = "pdf")

# save.image("../../formal/zimbabwe/post-croatia/data.RData")
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

# CareCascade Plot
graphics.off(); quartz.options(w = 10, h = 4)
GenCascadePlot_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/pro/cascade-projection.pdf", type = "pdf")

# 90-90-90 Plot
graphics.off(); quartz.options(w = 9, h = 4)
Gen909090Plot_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/pro/90-90-90.pdf", type = "pdf")

# Powers Plot
graphics.off(); quartz.options(w = 15, h = 4)
GenPowersCascadePlot_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/pro/cascade-powers.pdf", type = "pdf")

# New Infections
graphics.off(); quartz.options(w = 6, h = 4)
GenNewInfPlot_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/pro/new-infections.pdf", type = "pdf")

# AIDS Deaths
graphics.off(); quartz.options(w = 6, h = 4)
GenAidsDeathsPlot_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/pro/AIDS-deaths.pdf", type = "pdf")

# Discrete Cascade
graphics.off(); quartz.options(w = 10, h = 4)
GenDiscreteCascade_Thesis()
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/pro/cascade-discrete.pdf", type = "pdf")

# NUMBERS
t0 <- GetCascadeData(1)   # t0 = 1
t5 <- GetCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

# PLHIV Estimate in 2015
scales::comma(round(t0$res[1], -3))
1.385e+6
scales::comma(round(t5$res[1], -3))
1.534e+6

1.534e+6 / 1.385e+6

# DIAGNOSED
scales::comma(round(t0$res[2], -3))
1.242e+6
scales::comma(round(t5$res[2], -3))
1.406e+6

1.406e+6 / 1.242e+6

# On ART
scales::comma(round(t0$res[4], -3))
scales::comma(round(t5$res[4], -3))

round(t5$res[4], -3) / round(t0$res[4], -3)
30\% (0.876M to 1.140M)

# Viral suppression
scales::comma(round(t0$res[5], -3))
scales::comma(round(t5$res[5], -3))

round(t5$res[5], -3) / round(t0$res[5], -3)
33\% (0.773M to 1.027M)
23 - 14
t0

t0$res[5]/t0$res[1]
t5$res[5]/t5$res[1]

################################################################################
# Optimisation

intSwitch <- data.frame(
    testing      = TRUE,
    linkage      = TRUE,
    preRetention = TRUE,
    initiation   = TRUE,
    adherence    = TRUE,
    retention    = TRUE
)

OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.5
OptInput$intValue_omega <- parRange["omega", "min"]

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
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/opt/frontier.pdf", type = "pdf")

# FIGURE GENERATION
graphics.off(); quartz.options(w = 8, h = 4)
BuildChangesPlot_Thesis(CalibParamOut = CalibParamOut, optResults = optResults, target = 0.9^3)
quartz.save(file = "../../formal/zimbabwe/post-croatia/fig/opt/changes.pdf", type = "pdf")

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


Quantile_95(resTable[,"iTest"]) / 5
Quantile_95(resTable[,"iLink"]) / 5
Quantile_95(resTable[,"iPreR"]) / 5
Quantile_95(resTable[,"iInit"]) / 5
Quantile_95(resTable[,"iAdhr"]) / 5
Quantile_95(resTable[,"iRetn"]) / 5
Quantile_95(intRes[,"iTCst"]) / 5

################################################################################
# SAVE IMAGE #

# save.image("../../formal/zimbabwe/post-croatia/data.RData")
# save.image("../../formal/zimbabwe/post-croatia/big-data.RData")

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

################################################################################
# Loading RData and running some comparisons to PLHIV spectrum estimates
load("../../formal/zimbabwe/post-croatia/data.RData")

# get the model output right
head(CalibOut)

data <- CalibOut
head(out2)

# Calculate CI's
out2 <- AppendCI(data[data$source == "model" & data$indicator == "PLHIV",])
year  <- seq(2010, 2015, 1)
lower <- unique(out2[,"lower"])
mean  <- unique(out2[,"mean"])
upper <- unique(out2[,"upper"])
type <- "model"

df_model <- data.frame(year, type, lower, mean, upper)


spectrum <- readr::read_csv(file = "~/Google Drive/DIDE/HIVMC/WhoCascade/SpectrumCountryFiles/2016-update/output/refined/plhiv-refined.csv", col_names = TRUE, skip = 1)[1:6,]

year <- spectrum$year
lower <- spectrum$lower
mean <- spectrum$median
upper <- spectrum$upper
type <- "spectrum"

df_data <- data.frame(year, type, lower, mean, upper)

df <- rbind(df, df_data)

graphics.off()
quartz.options(w = 10, h = 4)

ggOut <- ggplot(df, aes(x = year, y = mean))
ggOut <- ggOut + geom_bar(aes(fill = type), position = 'dodge', stat = 'identity')
ggOut <- ggOut + geom_errorbar(mapping = aes(x = year, ymin = lower, ymax = upper, fill = type), position = position_dodge(width = 0.9), stat = "identity", width = 0.2, size = 0.5)
ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 6))
ggOut <- ggOut + scale_x_continuous(breaks = seq(2010, 2015, 1), labels = seq(2010, 2015, 1))
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.ticks.x = element_blank())
ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
ggOut <- ggOut + theme(axis.title.x =  element_blank())
ggOut <- ggOut + theme(axis.title.y =  element_text(size = 10))
ggOut <- ggOut + ggtitle("PLHIV Comparison", subtitle = "Model vs. Spectrum")
ggOut <- ggOut + ylab("Number of PLHIV")
ggOut

# DONE
