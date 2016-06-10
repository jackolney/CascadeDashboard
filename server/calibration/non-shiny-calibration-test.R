# This is a calibration test script that will run a non-shiny version of the cascade model.
# For calibration testing, in the absence of all the bells and whistles that come with the shiny app.
# This script will be a template for future shiny versions of the model.
# Ideally, will just be a case of drag and drop of functions.
# Create all functions external to this script.
rm(list=ls())
setwd("~/git/WhoCascade/CascadeDashboard")
# dir()
graphics.off()
quartz.options(w = 10, h = 8)

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
source("server/calibration/non-shiny-calibration.R", local = FALSE)


# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/WhoCascade/cascade")
devtools::test(pkg = "~/git/WhoCascade/cascade")

# Run baseline model (nothing fancy)
KenyaData <- GetMasterDataSet("Kenya")
# RunBaselineModel(data = KenyaData)

# RUN CALIBRATION
RunNSCalibration(data = KenyaData, maxIterations = 1e4, maxError = 2, limit = 100)

# All elements should be present, now.

# Is the minErrorRun = k or v

# CalibOut might be longer than 100, then that needs k
# else everything that is <100 in length needs v.

BuildCalibrationPlotDetail(data = CalibOut, originalData = KenyaData, limit = 100)

head(CalibParamOut)

# We need to know the mean (and distribution of parameter values being used in the model)

# hist(CalibParamOut$rho, col = "lightblue")

# par(mfrow = c(2,4))
# hist(CalibParamOut$rho, col = "lightblue")
# hist(CalibParamOut$epsilon, col = "lightblue")
# hist(CalibParamOut$kappa, col = "lightblue")
# hist(CalibParamOut$gamma, col = "lightblue")
# hist(CalibParamOut$theta, col = "lightblue")
# hist(CalibParamOut$omega, col = "lightblue")
# hist(CalibParamOut$p, col = "lightblue")
# hist(CalibParamOut$q, col = "lightblue")

# DefineParmRange(param = p, min = 0.01, max = 5)

test <- as.data.frame(CalibParamOut)
ggOne <-   ggplot(test, aes(rho)) +     geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggTwo <-   ggplot(test, aes(epsilon)) + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggThree <- ggplot(test, aes(kappa)) +   geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggFour <-  ggplot(test, aes(gamma)) +   geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggFive <-  ggplot(test, aes(theta)) +   geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggSix <-   ggplot(test, aes(omega)) +   geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggSeven <- ggplot(test, aes(p)) +       geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggEight <- ggplot(test, aes(q)) +       geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))

gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ggSix, ggSeven, ggEight, ncol = 4, nrow = 2)


# BuildCalibrationPlotDetail(data = CalibOut, originalData = KenyaData, limit = 500)

BuildCalibrationPlot(data = CalibOut, originalData = KenyaData)
BuildCalibrationPlotComplex(data = CalibOut, originalData = KenyaData)


run <- 1:length(runError)
theError <- data.frame(run, runError)
ggplot(theError, aes(runError)) + geom_histogram(aes(fill = ..count..), bins = 30)

# Need to compare 2015 estimate of cascade WITH projection model in 2015.
# Should be identical...
BuildCalibrationPlot(data = CalibOut, originalData = KenyaData)


CalibOut[CalibOut$year == 2015 & CalibOut$source == "model",][1:5,]


# CALIBRATION <- out[out$year == 2015,][1:5,]
CALIBRATION

# NOW FROM CALIBOUT.
# PROJECTION <- df
PROJECTION[["res"]]

dim(MasterOut)
names(result)

names(MasterOut[[1]])

MasterOut[[100]]

test <- c()
for(i in 1:100) {
    test[i] <- MasterOut[[i]]$N[1]
}
test
sum(test)

sum(unlist(lapply(result, function(x) sum(x$N[year]))))


# ITERATION CHECK
