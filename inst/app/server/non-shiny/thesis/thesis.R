# Thesis Script.R

# Create all functions external to this script.
rm(list=ls())
setwd("~/git/CascadeDashboard")
# dir()
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# -------- #
# WORKFLOW #
# -------- #

# --------- #
# STAGE ONE #
# --------- #

# source all the relevant files
require(RColorBrewer)
source("server/calibration/master.R",  local = FALSE)
source("server/calibration/initial.R", local = FALSE)
source("server/calibration/model.R",   local = FALSE)
source("server/calibration/error.R",   local = FALSE)

# This contains simple function calls for the models in various permutations
source("server/calibration/calibration.R",           local = FALSE)
source("server/calibration/assumptions.R",           local = FALSE)
source("server/calibration/calibration-data.R",      local = FALSE)
source("server/calibration/marrakech-data.R",        local = FALSE)
source("server/calibration/misc-functions.R",        local = FALSE)
source("server/misc-functions.R",                    local = FALSE)
source("server/calibration/plot-functions.R",        local = FALSE)
source("server/non-shiny/non-shiny-calibration.R",   local = FALSE)
source("server/country/misc-functions.R",            local = FALSE)
source("server/non-shiny/thesis/thesis-figures.R",   local = FALSE)
source("server/misc-functions.R",                    local = FALSE)
source("server/model/baseline-model.R",              local = FALSE)
source("server/model/best-fit-model.R",              local = FALSE)
source("server/model/beta.R",                        local = FALSE)
source("server/model/initial.R",                     local = FALSE)
source("server/model/parameters.R",                  local = FALSE)
source("server/model/sim-abs.R",                     local = FALSE)
source("server/model/sim-prop.R",                    local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",  local = FALSE)
source("server/optimisation/frontier.R",             local = FALSE)
source("server/optimisation/input-functions.R",      local = FALSE)
source("server/optimisation/output-functions.R",     local = FALSE)
source("server/optimisation/parameters.R",           local = FALSE)
source("server/optimisation/plot-functions.R",       local = FALSE)


# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/cascade")

# Unit tests
devtools::test(pkg = "~/git/cascade")
testthat::test_dir("tests")

# SIMULATIONS #
set.seed(100)
ZimbabweData <- GetMasterDataSet("Zimbabwe")
RunNSCalibration(country = "Zimbabwe", data = ZimbabweData, maxIterations = 1e4, maxError = 0.4, limit = 1000)

graphics.off()
quartz.options(w = 10, h = 4)
BuildCalibPlot_Thesis(data = CalibOut, originalData = ZimbabweData, limit = 1000)

graphics.off()
quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(data = CalibOut, originalData = ZimbabweData, limit = 1000)

graphics.off()
quartz.options(w = 6, h = 3)
BuildCalibrationHistogram_Thesis(runError, maxError = 0.4)

graphics.off()
quartz.options(w = 10, h = 4)
BuildCalibrationParameterHistGroup_Thesis()

round(colMeans(CalibParamOut), 4)

DefineParmRange()

a <- paste0(round(Quantile_95(CalibParamOut[["rho"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["rho"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["rho"]])[["upper"]], 4), "]")
b <- paste0(round(Quantile_95(CalibParamOut[["q"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["q"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["q"]])[["upper"]], 4), "]")
c <- paste0(round(Quantile_95(CalibParamOut[["epsilon"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["epsilon"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["epsilon"]])[["upper"]], 4), "]")
d <- paste0(round(Quantile_95(CalibParamOut[["kappa"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["kappa"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["kappa"]])[["upper"]], 4), "]")
e <- paste0(round(Quantile_95(CalibParamOut[["gamma"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["gamma"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["gamma"]])[["upper"]], 4), "]")
f <- paste0(round(Quantile_95(CalibParamOut[["theta"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["theta"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["theta"]])[["upper"]], 4), "]")
g <- paste0(round(Quantile_95(CalibParamOut[["p"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["p"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["p"]])[["upper"]], 4), "]")
h <- paste0(round(Quantile_95(CalibParamOut[["omega"]])[["mean"]], 4), " [", round(Quantile_95(CalibParamOut[["omega"]])[["lower"]], 4), " to ", round(Quantile_95(CalibParamOut[["omega"]])[["upper"]], 4), "]")

list(a,b,c,d,e,f,g,h)

Quantile_95(CalibParamOut[["q"]])
Quantile_95(CalibParamOut[["epsilon"]])
Quantile_95(CalibParamOut[["kappa"]])
Quantile_95(CalibParamOut[["gamma"]])
Quantile_95(CalibParamOut[["theta"]])
Quantile_95(CalibParamOut[["p"]])
Quantile_95(CalibParamOut[["omega"]])

####################################################################################################
####################################################################################################
## Projection Model

source("server/misc-functions.R",                      local = FALSE)
source("server/model/baseline-model.R",                local = FALSE)
source("server/model/best-fit-model.R",                local = FALSE)
source("server/model/beta.R",                          local = FALSE)
source("server/model/initial.R",                       local = FALSE)
source("server/model/parameters.R",                    local = FALSE)
source("server/model/sim-abs.R",                       local = FALSE)
source("server/model/sim-prop.R",                      local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",    local = FALSE)
source("server/optimisation/frontier.R",               local = FALSE)
source("server/optimisation/input-functions.R",        local = FALSE)
source("server/optimisation/output-functions.R",       local = FALSE)
source("server/optimisation/parameters.R",             local = FALSE)
source("server/optimisation/plot-functions.R",         local = FALSE)

MasterData <- ZimbabweData

graphics.off()
quartz.options(w = 10, h = 4)
GenCascadePlot_Thesis()

# PLHIV Estimate in 2015
scales::comma(round(t0$res[1], -3))
1.295e+6
scales::comma(round(t5$res[1], -3))
1.378e+6

scales::comma(round(t0$res[2], -3))
scales::comma(round(t5$res[2], -3))

round(t5$res[2], -3) / round(t0$res[2], -3)
7.6\% (1.183M to 1.273M)

scales::comma(round(t0$res[4], -3))
scales::comma(round(t5$res[4], -3))

round(t5$res[4], -3) / round(t0$res[4], -3)
13\% (0.860M to 0.972M)

scales::comma(round(t0$res[5], -3))
scales::comma(round(t5$res[5], -3))
round(t5$res[5], -3) / round(t0$res[5], -3)
21.4\% (0.542M to 0.658M)

t0

t0$res[5]/t0$res[1]
t5$res[5]/t5$res[1]


graphics.off()
quartz.options(w = 9, h = 4)
Gen909090Plot_Thesis()

graphics.off()
quartz.options(w = 15, h = 4)
GenPowersCascadePlot_Thesis()

graphics.off()
quartz.options(w = 6, h = 4)
GenNewInfPlot_Thesis()

graphics.off()
quartz.options(w = 6, h = 4)
GenAidsDeathsPlot_Thesis()

graphics.off()
quartz.options(w = 10, h = 4)
GenDiscreteCascade_Thesis()

####################################################################################################
####################################################################################################
## Optimisation

# intervention switches
intSwitch <- data.frame(
    testing =      TRUE,
    linkage =      TRUE,
    preRetention = TRUE,
    initiation =   TRUE,
    adherence =    TRUE,
    retention =    TRUE
    )

# configure interventions
OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.5
OptInput$intValue_omega <- parRange["omega", "min"]

# Test plot
# BuildCalibrationRandomFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = 1000, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)

intLength = 2

theOut <- RunNSOptimisation(propRuns = 0.1, intLength = intLength)

simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength, target = custom$target)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

intRes <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList, target = custom$target)
colMeans(intRes)

# remember to format retention interventions correctly...

results <- intRes[,c("iTest","iLink","iPreR","iInit","iAdhr","iRetn")]
results$iPreR <- abs(results$iPreR)
results$iRetn <- abs(results$iRetn)
results[results$iTest < 0, "iTest"] <- 0
results[results$iLink < 0, "iLink"] <- 0
results[results$iInit < 0, "iInit"] <- 0
results[results$iAdhr < 0, "iAdhr"] <- 0
results$run <- 1:dim(results)[1]

melted <- reshape2::melt(results, id = "run")

ggplot(melted, aes(x = variable, y = value)) + geom_raster() +  facet_wrap(~ run)

# I think what we need, is just one plot for a single run

test <- melted[melted$run == 1,]

# test

# ggplot(test, aes(x = variable, y = value, fill = run)) + geom_raster()

# ggplot(test, aes(x = variable, y = "")) + stat_density(aes(fill = value), geom = "raster", position = "identity")

# ggplot(melted, aes(x = variable, y = as.factor(value))) + stat_density(aes(fill = ..density..), geom = "raster", position = "identity")  +  facet_wrap(~ run)

# TABULATE RESULTS #
resTable <- results

resTable[resTable$iTest < 0, "iTest"] <- 0
resTable[resTable$iLink < 0, "iLink"] <- 0
resTable[resTable$iInit < 0, "iInit"] <- 0
resTable[resTable$iAdhr < 0, "iAdhr"] <- 0

colMeans(resTable)

# DIVIDE BY FIVE!!!!!!! (because we are displaying the PER YEAR change)
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

mRes <- melted

# DIVIDE ALL VALUES BY FIVE
mRes$value <- mRes$value / 5


# RENAME VARIABLES
mRes$variable <- as.character(mRes$variable)
mRes[mRes$variable == "iTest", "variable"] <- "Testing"
mRes[mRes$variable == "iLink", "variable"] <- "Linkage"
mRes[mRes$variable == "iPreR", "variable"] <- "Pre-ART\nRetention"
mRes[mRes$variable == "iInit", "variable"] <- "ART\nInitiation"
mRes[mRes$variable == "iAdhr", "variable"] <- "Adherence"
mRes[mRes$variable == "iRetn", "variable"] <- "ART\nRetention"

mRes$variable <- factor(mRes$variable, levels = c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention"))

graphics.off()
quartz.options(w = 6, h = 10)
ggOut <- ggplot(mRes, aes(x = variable, y = ""))
ggOut <- ggOut + stat_density(aes(fill = value), geom = "raster", position = "identity")
ggOut <- ggOut + facet_grid(run ~ ., switch = 'y')
ggOut <- ggOut + scale_fill_gradient(low = "grey97")
ggOut <- ggOut + theme_minimal()
ggOut <- ggOut + theme(axis.title.y = element_blank())
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.ticks = element_blank())
ggOut <- ggOut + theme(axis.ticks.length = unit(0, "lines"))
ggOut <- ggOut + theme(strip.text.y = element_text(size = 8, colour = "black", angle = 180))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut


# save image somewhere
# save.image("thesis.RData")
# load("thesis.RData")

# What about a figure that stacks bars on top of each other.
# changes on y, variable on x.
# ALPHA = 0.1
# brewer.pal(9, "Set1")[2]
graphics.off()
quartz.options(w = 7, h = 4)
ggOut <- ggplot(mRes, aes(x = variable, y = value, group = run))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.1, position = "identity", fill = brewer.pal(9, "Set1")[2])
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ylab("Change to Care")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 16e3), breaks = seq(0, 16e3, 2e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut


mRes$mean <- c(
    rep(mean(mRes[mRes$variable == "Testing","value"]), 27),
    rep(mean(mRes[mRes$variable == "Linkage","value"]), 27),
    rep(mean(mRes[mRes$variable == "Pre-ART\nRetention","value"]), 27),
    rep(mean(mRes[mRes$variable == "ART\nInitiation","value"]), 27),
    rep(mean(mRes[mRes$variable == "Adherence","value"]), 27),
    rep(mean(mRes[mRes$variable == "ART\nRetention","value"]), 27)
)

names <- c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention")
means <- c(
    mean(mRes[mRes$variable == "Testing","value"]),
    mean(mRes[mRes$variable == "Linkage","value"]),
    mean(mRes[mRes$variable == "Pre-ART\nRetention","value"]),
    mean(mRes[mRes$variable == "ART\nInitiation","value"]),
    mean(mRes[mRes$variable == "Adherence","value"]),
    mean(mRes[mRes$variable == "ART\nRetention","value"])
    )

labels <- data.frame(names, means)

# Can we add a line for the mean here?
ggOut <- ggplot(mRes, aes(x = variable, y = value, group = run))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.1, position = "identity", fill = brewer.pal(9, "Set1")[2])
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 16e3), breaks = seq(0, 16e3, 2e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + geom_errorbar(data = mRes, aes(x = variable, y = mean, ymax = mean, ymin = mean), alpha = 0.1)
ggOut <- ggOut + geom_text(data = mRes, aes(x = variable, y = mean, label = scales::comma(round(mean, 0))), vjust = -0.5)
ggOut


# MBA Testing
ggOut <- ggplot(mRes, aes(x = variable, y = value, group = run))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.1, position = "identity", fill = brewer.pal(9, "Set1")[2])
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.line.x = element_blank())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 80e3), breaks = seq(0, 80e3, 10e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + geom_errorbar(data = labels, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
ggOut <- ggOut + geom_text(data = labels, aes(x = names, y = means, label = scales::comma(round(means, 0))), vjust = -0.5, family = "Avenir Next")
ggOut

# save.image("thesis2.RData")
# load("thesis2.RData")

# Frontier Plot
BuildFrontierPlot_Thesis(CalibParamOut = CalibParamOut, optResults = optResults, target = custom$target)


# 26/09/16 - We need a new figure
# Baseline bars and then ON TOP (stacked) we have the additional changes required.
# We just need the averages, then we take the AVERAGE and plot that.

head(mRes)
mean(BaselineTest)
mean(BaselineLink)
mean(BaselinePreR)
mean(BaselineInit)
mean(BaselineAdhr)
mean(BaselineRetn)


# new column of 'strategy'

test <- mRes

test$strategy <- c("Intervention")

## BIG BASELINE FIGURE
# BASELINE VALUES

test <- rbind(test,
    data.frame(run = 1:max(test$run), variable = "Testing",            value = mean(BaselineTest) / 5, mean = mean(BaselineTest) / 5, strategy = "Baseline"),
    data.frame(run = 1:max(test$run), variable = "Linkage",            value = mean(BaselineLink) / 5, mean = mean(BaselineLink) / 5, strategy = "Baseline"),
    data.frame(run = 1:max(test$run), variable = "Pre-ART\nRetention", value = mean(BaselinePreR) / 5, mean = mean(BaselinePreR) / 5, strategy = "Baseline"),
    data.frame(run = 1:max(test$run), variable = "ART\nInitiation",    value = mean(BaselineInit) / 5, mean = mean(BaselineInit) / 5, strategy = "Baseline"),
    data.frame(run = 1:max(test$run), variable = "Adherence",          value = mean(BaselineAdhr) / 5, mean = mean(BaselineAdhr) / 5, strategy = "Baseline"),
    data.frame(run = 1:max(test$run), variable = "ART\nRetention",     value = mean(BaselineRetn) / 5, mean = mean(BaselineRetn) / 5, strategy = "Baseline")
)

# Add the mean to the data
test[test$variable == "Testing"            & test$strategy == "Intervention", "value"] <- test[test$variable == "Testing"            & test$strategy == "Intervention", "value"] + (mean(BaselineTest) / 5)
test[test$variable == "Linkage"            & test$strategy == "Intervention", "value"] <- test[test$variable == "Linkage"            & test$strategy == "Intervention", "value"] + (mean(BaselineLink) / 5)
test[test$variable == "Pre-ART\nRetention" & test$strategy == "Intervention", "value"] <- test[test$variable == "Pre-ART\nRetention" & test$strategy == "Intervention", "value"] + (mean(BaselinePreR) / 5)
test[test$variable == "ART\nInitiation"    & test$strategy == "Intervention", "value"] <- test[test$variable == "ART\nInitiation"    & test$strategy == "Intervention", "value"] + (mean(BaselineInit) / 5)
test[test$variable == "Adherence"          & test$strategy == "Intervention", "value"] <- test[test$variable == "Adherence"          & test$strategy == "Intervention", "value"] + (mean(BaselineAdhr) / 5)
test[test$variable == "ART\nRetention"     & test$strategy == "Intervention", "value"] <- test[test$variable == "ART\nRetention"     & test$strategy == "Intervention", "value"] + (mean(BaselineRetn) / 5)


labels2 <- labels
labels2$strategy <- "Intervention"
labels2$values <- labels2$means

labels2$means[1] <- labels2$means[1] + (mean(BaselineTest) / 5)
labels2$means[2] <- labels2$means[2] + (mean(BaselineLink) / 5)
labels2$means[3] <- labels2$means[3] + (mean(BaselinePreR) / 5)
labels2$means[4] <- labels2$means[4] + (mean(BaselineInit) / 5)
labels2$means[5] <- labels2$means[5] + (mean(BaselineAdhr) / 5)
labels2$means[6] <- labels2$means[6] + (mean(BaselineRetn) / 5)

levels(test$strategy)

test$strategy <- factor(test$strategy, levels = c("Intervention", "Baseline"))


graphics.off()
quartz.options(w = 7.5, h = 4)
ggOut <- ggplot(test, aes(x = variable, y = value, fill = strategy))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.1, position = "identity")
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + scale_fill_manual(values = c(brewer.pal(9, "Set1")[2], brewer.pal(9, "Set1")[1]))
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.line.x = element_blank())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 100e3), breaks = seq(0, 100e3, 10e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + geom_errorbar(data = labels2, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
ggOut <- ggOut + geom_text(data = labels2, aes(x = names, y = means, label = paste0("+", scales::comma(round(values, 0)))), vjust = -0.5, family = "Avenir Next")
ggOut <- ggOut + theme(legend.position = 'right')
ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
ggOut

# Need a better baseline tracker of ADHERENCE. i.e. VIRAL SUPPRESSION.
BuildChangesPlot(CalibParamOut, optResults, custom$target)


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


################################################################################

theBase <-  rbind(
    data.frame(variable = "Testing",            value = mean(BaselineTest) / 5, mean = mean(BaselineTest) / 5, strategy = "Baseline"),
    data.frame(variable = "Linkage",            value = mean(BaselineLink) / 5, mean = mean(BaselineLink) / 5, strategy = "Baseline"),
    data.frame(variable = "Pre-ART\nRetention", value = mean(BaselinePreR) / 5, mean = mean(BaselinePreR) / 5, strategy = "Baseline"),
    data.frame(variable = "ART\nInitiation",    value = mean(BaselineInit) / 5, mean = mean(BaselineInit) / 5, strategy = "Baseline"),
    data.frame(variable = "Adherence",          value = mean(BaselineAdhr) / 5, mean = mean(BaselineAdhr) / 5, strategy = "Baseline"),
    data.frame(variable = "ART\nRetention",     value = mean(BaselineRetn) / 5, mean = mean(BaselineRetn) / 5, strategy = "Baseline")
    )


ggOut <- ggplot(theBase, aes(x = variable, y = value, fill = strategy))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 1, position = "identity")
ggOut


mRes

ggOut <- ggplot(mRes, aes(x = variable, y = value))
geom_errorbar(data = labels2, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
ggOut <- ggOut + geom_boxplot(aes(middle = mean), outlier.size = 0, coef = 0, outlier.color = NA, alpha = 0.5)
ggOut

alpha
colour
fill
linetype
shape
size
weight

ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.1, position = "identity")
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + scale_fill_manual(values = c(brewer.pal(9, "Set1")[2], brewer.pal(9, "Set1")[1]))
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.line.x = element_blank())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 100e3), breaks = seq(0, 100e3, 10e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + geom_errorbar(data = labels2, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
ggOut <- ggOut + geom_text(data = labels2, aes(x = names, y = means, label = paste0("+", scales::comma(round(values, 0)))), vjust = -0.5, family = "Avenir Next")
ggOut <- ggOut + theme(legend.position = 'right')
ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
ggOut



# SOMETHING NEW ################################################################

variable <- c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention")

mean <- c(
    Quantile_95(mRes[mRes$variable == "Testing", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "Adherence", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["mean"]]
)

upper <- c(
    Quantile_95(mRes[mRes$variable == "Testing", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "Adherence", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["upper"]]
)

lower <- c(
    Quantile_95(mRes[mRes$variable == "Testing", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "Adherence", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["lower"]]
)

strategy <- "Intervention"

outData <- data.frame(variable, mean, lower, upper, strategy)

ggplot(outData, aes(x = variable)) + geom_crossbar(aes(y = mean, ymax = upper, ymin = lower), fill = "black", alpha = 0.5)

# Putting results on top of baseline
outData$mean  <- outData$mean  + theBase$mean
outData$upper <- outData$upper + theBase$mean
outData$lower <- outData$lower + theBase$mean

ggOut <- ggplot(theBase, aes(x = variable, y = value, group = strategy))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.75, position = "identity", fill = brewer.pal(9, "Set1")[1])
ggOut <- ggOut + geom_crossbar(data = outData, aes(x = variable, y = mean, ymax = upper, ymin = lower), fill = brewer.pal(9, "Set1")[2], alpha = 0.3, linetype = "blank")
ggOut <- ggOut + geom_errorbar(data = labels2, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
ggOut <- ggOut + geom_text(data = labels2, aes(x = names, y = means, label = paste0("+", scales::comma(round(values, 0)))), vjust = -0.5, family = "Avenir Next")
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.line.x = element_blank())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 70e3), breaks = seq(0, 70e3, 10e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
# ggOut <- ggOut + theme(legend.position = 'right')
# ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
ggOut <- ggOut + scale_fill_identity(name = "hello", guide = 'legend', labels = c('bar'))
ggOut

## DONE ##

ggOut <- ggplot(theBase, aes(x = variable, y = value, fill = strategy))
ggOut <- ggOut + geom_bar(stat = "identity", alpha = 0.75, position = "identity")
ggOut <- ggOut + geom_crossbar(data = outData, aes(x = variable, y = mean, ymax = upper, ymin = lower, fill = strategy), alpha = 0.3, linetype = "blank")
ggOut <- ggOut + geom_errorbar(data = labels2, aes(x = names, y = means, ymax = means, ymin = means), alpha = 1)
ggOut <- ggOut + scale_fill_manual(values = c(brewer.pal(9, "Set1")[1],brewer.pal(9, "Set1")[2]), labels = c("Baseline", "Intervention"), guide = "legend")
ggOut <- ggOut + geom_text(data = labels2, aes(x = names, y = means, label = paste0("+", scales::comma(round(values, 0)))), vjust = -0.5, family = "Avenir Next")
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.line.x = element_blank())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 70e3), breaks = seq(0, 70e3, 10e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + theme(legend.position = 'right')
ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
ggOut

# Do we take it any further?

# Quantile_50 <- function(vector) {
#     m <- mean(vector)
#     p95 <- quantile(vector, 0.5)[[1]]
#     p05 <- quantile(vector, 0.5)[[1]]
#     return(c(upper = p95, mean = m, lower = p05))
# }

# Quantile_50(BaselineTest)
# quantile(BaselineTest, 0.70)

################################################################################
## With errorbars? ##
# 28/09/16

variable <- c("Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention")

mean <- c(
    Quantile_95(mRes[mRes$variable == "Testing", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "Adherence", "value"])[["mean"]],
    Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["mean"]]
)

upper <- c(
    Quantile_95(mRes[mRes$variable == "Testing", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "Adherence", "value"])[["upper"]],
    Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["upper"]]
)

lower <- c(
    Quantile_95(mRes[mRes$variable == "Testing", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "Linkage", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "Pre-ART\nRetention", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "ART\nInitiation", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "Adherence", "value"])[["lower"]],
    Quantile_95(mRes[mRes$variable == "ART\nRetention", "value"])[["lower"]]
)

strategy <- "Intervention"

outData <- data.frame(variable, mean, lower, upper, strategy)

theBase <-  rbind(
    data.frame(variable = "Testing",            mean = mean(BaselineTest) / 5, strategy = "Baseline"),
    data.frame(variable = "Linkage",            mean = mean(BaselineLink) / 5, strategy = "Baseline"),
    data.frame(variable = "Pre-ART\nRetention", mean = mean(BaselinePreR) / 5, strategy = "Baseline"),
    data.frame(variable = "ART\nInitiation",    mean = mean(BaselineInit) / 5, strategy = "Baseline"),
    data.frame(variable = "Adherence",          mean = mean(BaselineAdhr) / 5, strategy = "Baseline"),
    data.frame(variable = "ART\nRetention",     mean = mean(BaselineRetn) / 5, strategy = "Baseline")
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
ggOut <- ggOut + geom_errorbar(data = theLabel, aes(x = variable, y = mean, ymax = upper, ymin = lower), alpha = 1, width = 0.25, size = 0.5)
ggOut <- ggOut + geom_label(data = theLabel, aes(x = variable, y = mean, label = paste0("+", scales::comma(round(value, 0)))), vjust = +0.5, family = "Avenir Next", colour = "black", size = 3, alpha = 1, show.legend = FALSE)
ggOut <- ggOut + scale_fill_manual(values = c("#4F8ABA","#E41A1C"))
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ylab("Changes to Care Per Year")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 9))
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.title.y = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 9))
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.line.x = element_blank())
ggOut <- ggOut + scale_y_continuous(limits = c(0, 70e3), breaks = seq(0, 70e3, 10e3), labels = scales::comma, expand = c(0, 0))
ggOut <- ggOut + theme(text = element_text(family = "Avenir Next"))
ggOut <- ggOut + theme(legend.position = 'right')
ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
ggOut <- ggOut + guides(fill = guide_legend(override.aes = list(alpha = 1)))
ggOut

