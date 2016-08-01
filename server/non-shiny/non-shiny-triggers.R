# non-shiny optimisation triggers and setup
# input stuff etc.
graphics.off()
quartz.options(w = 10, h = 8)

source("server/model/baseline-model.R",                local = FALSE)
source("server/model/best-fit-model.R",                local = FALSE)
source("server/model/beta.R",                          local = FALSE)
source("server/model/initial.R",                       local = FALSE)
source("server/model/parameters.R",                    local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",    local = FALSE)
source("server/optimisation/input-functions.R",        local = FALSE)
source("server/optimisation/output-functions.R",       local = FALSE)
source("server/optimisation/parameters.R",             local = FALSE)
source("server/optimisation/plot-functions.R",         local = FALSE)
source("server/optimisation/sim.R",                    local = FALSE)
source("server/projection/CD4-distribution.R",         local = FALSE)
source("server/optimisation/frontier.R",               local = FALSE)

# reactive input setup
MasterCD4_2015 <- GetCD4Distribution2015("Kenya")
MasterData <- GetCountryData("Kenya")

intSwitch <- data.frame(
    testing =      TRUE,
    linkage =      TRUE,
    preRetention = TRUE,
    initiation =   TRUE,
    adherence =    TRUE,
    retention =    TRUE
    )

OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.1
OptInput$intValue_omega <- parRange["rho", "min"]

# ------------ #
# OPTIMISATION #
# ------------ #

BuildCalibrationBestFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = 1000, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)

theOut <- RunNSOptimisation(propRuns = 0.1)
head(theOut)
dim(theOut)

mean(theOut$Testing)

a = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Rho), alpha = 0.2) + theme_minimal()
b = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Q), alpha = 0.2) + theme_minimal()
c = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Kappa), alpha = 0.2) + theme_minimal()
d = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Gamma), alpha = 0.2) + theme_minimal()
e = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Sigma), alpha = 0.2) + theme_minimal()

gridExtra::grid.arrange(a, b, c, d, e, ncol = 2, nrow = 3)

# See one big data.frame may not be the best solution.

# We want to calculate the frontier, or approx the function, for EACH of the (j) parameter sets run.

# What if we just take one simulation


##### HERE WE GO #####
simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = 2))[1]

optRuns <- WhichAchieved73(simData = theOut, simLength = simLength)
optRuns

frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)
frontierList

test <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

mean(test[,"iCost"])
mean(test[,"iTest"])
mean(test[,"iLink"])
mean(test[,"iPreR"])
mean(test[,"iInit"])
mean(test[,"iAdhr"])
mean(test[,"iRetn"])

test


    values <- c(
        testing      = (alt$CumDiag[251] - baseline$CumDiag[251]) / 5,
        linkage      = (alt$CumLink[251] - baseline$CumLink[251]) / 5,
        preRetention = (alt$CumPreL[251] - baseline$CumPreL[251]) / 5,
        initiation   = (alt$CumInit[251] - baseline$CumInit[251]) / 5,
        adherence    = (alt$CumAdhr[251] - baseline$CumAdhr[251]) / 5,
        retention    = (alt$CumLoss[251] - baseline$CumLoss[251]) / 5
    )


cols <- c(rep("green", 2), rep("orange", 2), rep("red", 2))

# values <- test

cols[which(names(values[rev(order(abs(values)))]) == "linkage")],)

values <- colSums(test)
which(names(values[rev(order(abs(values)))]) == "iPreR")

values <- colSums(test)

values[rev(order(abs(values)))]
