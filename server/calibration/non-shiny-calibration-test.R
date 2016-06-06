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


BuildCalibrationPlotDetail(data = CalibOut, originalData = KenyaData)



out <- data[data$source == "data",]

# Find Minimums & Maximums & Mean of data.
out2 <- AppendMinMaxMean(data[data$source == "model",])
out2$indicator <- factor(out2$indicator, levels = c(
    "PLHIV",
    "PLHIV Diagnosed",
    "PLHIV in Care",
    "PLHIV on ART",
    "PLHIV Suppressed"
    )
)
out2$weight <- 0

# Set Colors
cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
names(cols) <- c("red", "amber", "green")
mycol <- scale_colour_manual(name = "weight", values = cols)

# Create some pretty output plots
ggOne <- ggplot(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
ggOne <- ggOne + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
ggOne <- ggOne + scale_y_continuous(labels = scales::comma)
ggOne <- ggOne + geom_line()
ggOne <- ggOne + geom_point(aes(color = weight), size = 5)
ggOne <- ggOne + mycol
ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, shading shows upper and lower model estimates")
ggOne <- ggOne + theme(legend.position = "none")
ggOne <- ggOne + theme(axis.text.x = element_text(size = 14))
ggOne <- ggOne + theme(axis.text.y = element_text(size = 14))
ggOne <- ggOne + theme(axis.title =  element_text(size = 15))
ggOne <- ggOne + theme(title =       element_text(size = 15))
ggOne <- ggOne + theme(axis.title.y = element_blank())
ggOne <- ggOne + theme(axis.title.x = element_blank())

ggTwo <- ggplot(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
ggTwo <- ggTwo + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV Diagnosed",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma)
ggTwo <- ggTwo + geom_line()
ggTwo <- ggTwo + geom_point(aes(color = weight), size = 5)
ggTwo <- ggTwo + mycol
ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, shading shows upper and lower model estimates")
ggTwo <- ggTwo + theme(legend.position = "none")
ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 14))
ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 14))
ggTwo <- ggTwo + theme(axis.title =  element_text(size = 15))
ggTwo <- ggTwo + theme(title =       element_text(size = 15))
ggTwo <- ggTwo + theme(axis.title.y = element_blank())
ggTwo <- ggTwo + theme(axis.title.x = element_blank())

ggThree <- ggplot(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
ggThree <- ggThree + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV in Care",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
ggThree <- ggThree + scale_y_continuous(labels = scales::comma)
ggThree <- ggThree + geom_line()
ggThree <- ggThree + geom_point(aes(color = weight), size = 5)
ggThree <- ggThree + mycol
ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, shading shows upper and lower model estimates")
ggThree <- ggThree + theme(legend.position = "none")
ggThree <- ggThree + theme(axis.text.x = element_text(size = 14))
ggThree <- ggThree + theme(axis.text.y = element_text(size = 14))
ggThree <- ggThree + theme(axis.title =  element_text(size = 15))
ggThree <- ggThree + theme(title =       element_text(size = 15))
ggThree <- ggThree + theme(axis.title.y = element_blank())
ggThree <- ggThree + theme(axis.title.x = element_blank())

ggFour <- ggplot(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
ggFour <- ggFour + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV on ART",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
ggFour <- ggFour + scale_y_continuous(labels = scales::comma)
ggFour <- ggFour + geom_line()
ggFour <- ggFour + geom_point(aes(color = weight), size = 5)
ggFour <- ggFour + mycol
ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, shading shows upper and lower model estimates")
ggFour <- ggFour + theme(legend.position = "none")
ggFour <- ggFour + theme(axis.text.x = element_text(size = 14))
ggFour <- ggFour + theme(axis.text.y = element_text(size = 14))
ggFour <- ggFour + theme(axis.title =  element_text(size = 15))
ggFour <- ggFour + theme(title =       element_text(size = 15))
ggFour <- ggFour + theme(axis.title.y = element_blank())
ggFour <- ggFour + theme(axis.title.x = element_blank())

ggFive <- ggplot(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
ggFive <- ggFive + geom_ribbon(data = na.omit(out2[out2$indicator == "PLHIV Suppressed",]), aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey12", alpha = 0.3)
ggFive <- ggFive + scale_y_continuous(labels = scales::comma)
ggFive <- ggFive + geom_line()
ggFive <- ggFive + geom_point(aes(color = weight), size = 5)
ggFive <- ggFive + mycol
ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, shading shows upper and lower model estimates")
ggFive <- ggFive + theme(legend.position = "none")
ggFive <- ggFive + theme(axis.text.x = element_text(size = 14))
ggFive <- ggFive + theme(axis.text.y = element_text(size = 14))
ggFive <- ggFive + theme(axis.title =  element_text(size = 15))
ggFive <- ggFive + theme(title =       element_text(size = 15))
ggFive <- ggFive + theme(axis.title.y = element_blank())
ggFive <- ggFive + theme(axis.title.x = element_blank())

gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 1, nrow = 5)
