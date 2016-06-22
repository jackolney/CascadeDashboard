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

TanzaniaData <- GetMasterDataSet("Tanzania")

# RUN CALIBRATION
RunNSCalibration(country = "Kenya", data = KenyaData, maxIterations = 1e4, maxError = 2, limit = 100)

RunNSCalibration(country = "Tanzania", data = TanzaniaData, maxIterations = 1e4, maxError = 2, limit = 100)

# All elements should be present, now.

# Is the minErrorRun = k or v

# CalibOut might be longer than 100, then that needs k
# else everything that is <100 in length needs v.

BuildCalibrationPlotDetail(data = CalibOut, originalData = TanzaniaData, limit = 100)

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

data = CalibOut
limit = 500
    optimal <- data[1:72 + 72 * (minErrorRun - 1),]

    # Subset data to show only 'data'
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

    # 6 for six years (2010 to 2015), and 7 for seven indicators
    out2$sim <- rep(x = 1:limit, each = 6 * 7)

    optimal$sim <- "optimal"
    meanRun$sim <- "optimal"

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    ggOne <- ggplot()
    ggOne <- ggOne + geom_line(data = na.omit(out2[out2$indicator == "PLHIV",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggOne <- ggOne + geom_line(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight))
    ggOne <- ggOne + geom_line(data = optimal[optimal$indicator == "PLHIV" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggOne <- ggOne + geom_line(data = meanRun[meanRun$indicator == "PLHIV" & meanRun$source == "model",], aes(x = year, y = value, group = sim), col = "purple", size = 1)
    ggOne <- ggOne + geom_point(data = out[out$indicator == "PLHIV",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma)
    ggOne <- ggOne + mycol
    ggOne <- ggOne + ggtitle("PLHIV", subtitle = "Points are data, lines represent each simulation")
    ggOne <- ggOne + theme(legend.position = "none")
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 14))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 14))
    ggOne <- ggOne + theme(axis.title =  element_text(size = 15))
    ggOne <- ggOne + theme(title =       element_text(size = 15))
    ggOne <- ggOne + theme(axis.title.y = element_blank())
    ggOne <- ggOne + theme(axis.title.x = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = "Avenir Next"))
    ggOne <- ggOne + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggTwo <- ggplot()
    ggTwo <- ggTwo + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Diagnosed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggTwo <- ggTwo + geom_line(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight))
    ggTwo <- ggTwo + geom_line(data = optimal[optimal$indicator == "PLHIV Diagnosed" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggTwo <- ggTwo + geom_line(data = meanRun[meanRun$indicator == "PLHIV Diagnosed" & meanRun$source == "model",], aes(x = year, y = value, group = sim), col = "purple", size = 1)
    ggTwo <- ggTwo + geom_point(data = out[out$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma)
    ggTwo <- ggTwo + mycol
    ggTwo <- ggTwo + ggtitle("PLHIV Diagnosed", subtitle = "Points are data, lines represent each simulation")
    ggTwo <- ggTwo + theme(legend.position = "none")
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 14))
    ggTwo <- ggTwo + theme(axis.title =  element_text(size = 15))
    ggTwo <- ggTwo + theme(title =       element_text(size = 15))
    ggTwo <- ggTwo + theme(axis.title.y = element_blank())
    ggTwo <- ggTwo + theme(axis.title.x = element_blank())
    ggTwo <- ggTwo + theme(text = element_text(family = "Avenir Next"))
    ggTwo <- ggTwo + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggThree <- ggplot()
    ggThree <- ggThree + geom_line(data = na.omit(out2[out2$indicator == "PLHIV in Care",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggThree <- ggThree + geom_line(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight))
    ggThree <- ggThree + geom_line(data = optimal[optimal$indicator == "PLHIV in Care" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggThree <- ggThree + geom_line(data = meanRun[meanRun$indicator == "PLHIV in Care" & meanRun$source == "model",], aes(x = year, y = value, group = sim), col = "purple", size = 1)
    ggThree <- ggThree + geom_point(data = out[out$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggThree <- ggThree + scale_y_continuous(labels = scales::comma)
    ggThree <- ggThree + mycol
    ggThree <- ggThree + ggtitle("PLHIV in Care", subtitle = "Points are data, lines represent each simulation")
    ggThree <- ggThree + theme(legend.position = "none")
    ggThree <- ggThree + theme(axis.text.x = element_text(size = 14))
    ggThree <- ggThree + theme(axis.text.y = element_text(size = 14))
    ggThree <- ggThree + theme(axis.title =  element_text(size = 15))
    ggThree <- ggThree + theme(title =       element_text(size = 15))
    ggThree <- ggThree + theme(axis.title.y = element_blank())
    ggThree <- ggThree + theme(axis.title.x = element_blank())
    ggThree <- ggThree + theme(text = element_text(family = "Avenir Next"))
    ggThree <- ggThree + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFour <- ggplot()
    ggFour <- ggFour + geom_line(data = na.omit(out2[out2$indicator == "PLHIV on ART",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFour <- ggFour + geom_line(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight))
    ggFour <- ggFour + geom_line(data = optimal[optimal$indicator == "PLHIV on ART" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggFour <- ggFour + geom_line(data = meanRun[meanRun$indicator == "PLHIV on ART" & meanRun$source == "model",], aes(x = year, y = value, group = sim), col = "purple", size = 1)
    ggFour <- ggFour + geom_point(data = out[out$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFour <- ggFour + scale_y_continuous(labels = scales::comma)
    ggFour <- ggFour + mycol
    ggFour <- ggFour + ggtitle("PLHIV on ART", subtitle = "Points are data, lines represent each simulation")
    ggFour <- ggFour + theme(legend.position = "none")
    ggFour <- ggFour + theme(axis.text.x = element_text(size = 14))
    ggFour <- ggFour + theme(axis.text.y = element_text(size = 14))
    ggFour <- ggFour + theme(axis.title =  element_text(size = 15))
    ggFour <- ggFour + theme(title =       element_text(size = 15))
    ggFour <- ggFour + theme(axis.title.y = element_blank())
    ggFour <- ggFour + theme(axis.title.x = element_blank())
    ggFour <- ggFour + theme(text = element_text(family = "Avenir Next"))
    ggFour <- ggFour + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    ggFive <- ggplot()
    ggFive <- ggFive + geom_line(data = na.omit(out2[out2$indicator == "PLHIV Suppressed",]), aes(x = year, y = value, group = sim), alpha = 0.2, size = 1, col = "#4F8ABA")
    ggFive <- ggFive + geom_line(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight))
    ggFive <- ggFive + geom_line(data = optimal[optimal$indicator == "PLHIV Suppressed" & optimal$source == "model",], aes(x = year, y = value, group = sim), col = "red", size = 1)
    ggFive <- ggFive + geom_line(data = meanRun[meanRun$indicator == "PLHIV Suppressed" & meanRun$source == "model",], aes(x = year, y = value, group = sim), col = "purple", size = 1)
    ggFive <- ggFive + geom_point(data = out[out$indicator == "PLHIV Suppressed",], aes(x = year, y = value, group = weight, color = weight), size = 5)
    ggFive <- ggFive + scale_y_continuous(labels = scales::comma)
    ggFive <- ggFive + mycol
    ggFive <- ggFive + ggtitle("PLHIV Suppressed", subtitle = "Points are data, lines represent each simulation")
    ggFive <- ggFive + theme(legend.position = "none")
    ggFive <- ggFive + theme(axis.text.x = element_text(size = 14))
    ggFive <- ggFive + theme(axis.text.y = element_text(size = 14))
    ggFive <- ggFive + theme(axis.title =  element_text(size = 15))
    ggFive <- ggFive + theme(title =       element_text(size = 15))
    ggFive <- ggFive + theme(axis.title.y = element_blank())
    ggFive <- ggFive + theme(axis.title.x = element_blank())
    ggFive <- ggFive + theme(text = element_text(family = "Avenir Next"))
    ggFive <- ggFive + expand_limits(y = c(0, round(max(out2$max), digits = -4)))

    gridExtra::grid.arrange(ggOne, ggTwo, ggThree, ggFour, ggFive, ncol = 2, nrow = 3)
