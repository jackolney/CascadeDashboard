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



# THIS NEEDS FIGURING OUT!

I think its a 1:72 type thing.

    y <- GetInitial(
        p = p,
        iterationResult = CalibOut[CalibOut$year == 2015 & CalibOut$source == "model",][1:7 + 7 * (minErrorRun - 1),],
        masterCD4 = MasterCD4_2015
        )

    out <- calibOut[calibOut$year == 2015 & calibOut$source == "model",][1:7 + 7 * (minErrorRun - 1),]
    out <- out[, c("indicator", "value")]

# check all calls to best-fit-modal.R


minErrorRun
minError

dim(CalibOut)


CalibOut[CalibOut$year == 2015 & CalibOut$source == "error",][1:7 + 7 * (minErrorRun - 1),]


CalibOut[CalibOut$year == 2015 & CalibOut$source == "model",][1:7 + 7 * (minErrorRun - 1),],

CalibOut[1:72,]["weight",]

test <- CalibOut[1:72 + 72 * (minErrorRun - 1),]
sum(test[test$source == "error", "value"])
minError


sum(CalibOut[CalibOut$source == "error",][1:7 + 7 * (minErrorRun - 1),"value"])


# Get Best Result

int <- CalibOut[1:72 + 72 * (minErrorRun - 1),]
out <- int[int$year == 2015 & int$source == "model",]

GetBestCalibOut <- function(calibOut, minErrorRun) {
    int <- calibOut[1:72 + 72 * (minErrorRun - 1),]
    out <- int[int$year == 2015 & int$source == "model",]
    if (dim(out)[1] > 7) {
        warning("out length is > 7")
        print(out)
    }
    out
}
