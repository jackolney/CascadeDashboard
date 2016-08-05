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
source("server/non-shiny/non-shiny-calibration.R",   local = FALSE)
source("server/country/misc-functions.R",            local = FALSE)


# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/WhoCascade/cascade")

# Unit tests
devtools::test(pkg = "~/git/WhoCascade/cascade")
testthat::test_dir("tests")

## Run baseline model (nothing fancy)
# Kenya
KenyaData <- GetMasterDataSet("Kenya")
RunNSCalibration(country = "Kenya", data = KenyaData, maxIterations = 1e4, maxError = 3, limit = 100)
BuildCalibrationPlotDetail(data = CalibOut, originalData = KenyaData, limit = 1000)

# Tanzania
TanzaniaData <- GetMasterDataSet("Tanzania")
RunNSCalibration(country = "Tanzania", data = TanzaniaData, maxIterations = 1e4, maxError = 2, limit = 100)
BuildCalibrationPlotDetail(data = CalibOut, originalData = TanzaniaData, limit = 100)

# Zimbabwe
ZimbabweData <- GetMasterDataSet("Zimbabwe")
RunNSCalibration(country = "Zimbabwe", data = ZimbabweData, maxIterations = 1e4, maxError = 2, limit = 100)
BuildCalibrationPlotDetail(data = CalibOut, originalData = ZimbabweData, limit = 100)

####################################################################################################
####################################################################################################
### TESTING OF WEIGHTS ###

KenyaData <- GetMasterDataSet("Kenya")

# Basic Setup
error = 2
limit = 1000
iterations = 1e4

KenyaData$calib$weight <- "green"
RunNSCalibration(country = "Kenya", data = KenyaData, maxIterations = iterations, maxError = error, limit = limit)
BuildCalibrationTestPlot(data = CalibOut, originalData = KenyaData, limit = limit, runError = runError, maxError = error)
quartz.save(file = "captures/weight-test/kenya-green.pdf", type = 'pdf')

KenyaData$calib$weight <- "amber"
RunNSCalibration(country = "Kenya", data = KenyaData, maxIterations = iterations, maxError = error, limit = limit)
BuildCalibrationTestPlot(data = CalibOut, originalData = KenyaData, limit = limit, runError = runError, maxError = error)
quartz.save(file = "captures/weight-test/kenya-amber.pdf", type = 'pdf')

KenyaData$calib$weight <- "red"
RunNSCalibration(country = "Kenya", data = KenyaData, maxIterations = iterations, maxError = error, limit = limit)
BuildCalibrationTestPlot(data = CalibOut, originalData = KenyaData, limit = limit, runError = runError, maxError = error)
quartz.save(file = "captures/weight-test/kenya-red.pdf", type = 'pdf')




## I would like to see:
# 1. Histogram of errors.
# 2. Mean simulation error (does it decrease when all data are RED?)
# 6. Do the above for all "RED", "AMBER" and "GREEN"


####################################################################################################
####################################################################################################

# Parameter Histograms (why don't I wrap all this crap in a function?)
test <- as.data.frame(CalibParamOut)
ggOne   <- ggplot(test, aes(rho))     + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggTwo   <- ggplot(test, aes(epsilon)) + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggThree <- ggplot(test, aes(kappa))   + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggFour  <- ggplot(test, aes(gamma))   + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggFive  <- ggplot(test, aes(theta))   + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggSix   <- ggplot(test, aes(omega))   + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggSeven <- ggplot(test, aes(p))       + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))
ggEight <- ggplot(test, aes(q))       + geom_histogram(aes(fill = ..count..), bins = 10) + theme_classic() + theme(legend.position = "none", text = element_text(family = "Avenir Next"), axis.line.x = element_line(), axis.line.y = element_line()) + scale_y_continuous(expand = c(0,0))

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


## MIN ERROR RUN CHECK ##
BuildCalibrationPlotDetail(data = CalibOut, originalData = KenyaData, limit = 100)

BuildCalibrationPlotDetail_Best(data = CalibOut, originalData = KenyaData, limit = 500, minErrorRun = minErrorRun)


theMean <- CallMeanModel()
dim(theMean)

meanRun <- AssembleComparisonDataFrame(country = "Kenya", model = theMean, data = KenyaData)

head(output, 100)

head(test,100)

    ### END ###


# dim(CalibOut)

p <- parameters(
    prop_preART_500    = MasterCD4_2015[1,"prop.Off.ART.500"][[1]],
    prop_preART_350500 = MasterCD4_2015[1,"prop.Off.ART.350500"][[1]],
    prop_preART_250350 = MasterCD4_2015[1,"prop.Off.ART.250350"][[1]],
    prop_preART_200250 = MasterCD4_2015[1,"prop.Off.ART.200250"][[1]],
    prop_preART_100200 = MasterCD4_2015[1,"prop.Off.ART.100200"][[1]],
    prop_preART_50100  = MasterCD4_2015[1,"prop.Off.ART.50100"][[1]],
    prop_preART_50     = MasterCD4_2015[1,"prop.Off.ART.50"][[1]],
    t_1 = ConvertYear2015(MasterData[["treatment_guidelines"]][["more500"]]),
    t_2 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less500"]]),
    t_3 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less350"]]),
    t_4 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less250"]]),
    t_5 = ConvertYear2015(MasterData[["treatment_guidelines"]][["less200"]]),
    Rho = CalibParamOut[minErrorRun,"rho"],
    Epsilon = CalibParamOut[minErrorRun,"epsilon"],
    Kappa = CalibParamOut[minErrorRun,"kappa"],
    Gamma = CalibParamOut[minErrorRun,"gamma"],
    Theta = CalibParamOut[minErrorRun,"theta"],
    Omega = CalibParamOut[minErrorRun,"omega"],
    p = CalibParamOut[minErrorRun,"p"],
    q = CalibParamOut[minErrorRun,"q"]
)

p[["Mu"]]

p[["Rho"]]

# # Now we need the initials.
# y <- GetInitial(
#     p = p,
#     iterationResult = GetBestCalibOut(calibOut = CalibOut, minErrorRun = minErrorRun),
#     masterCD4 = MasterCD4_2015
#     )

# p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[minErrorRun,])
