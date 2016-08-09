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


##### Frontier Finding #####

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

#### What Plot? ####

# Average Cost / Impact Frontier?

# Can we layer all combinations on top of each other?

# Then a big thick, AVERAGE frontier?

optResults$sim <- rep(x = 1:(dim(optResults)[1] / simLength), each = simLength)


allRuns <- GetFrontiers(simData = theOut, optRuns = 1:(dim(optResults)[1] / simLength), simLength = simLength)

interpol <- list()
for(n in 1:(dim(optResults)[1] / simLength)) {
    lower <- (1 + simLength * (n - 1))
    upper <- (simLength + simLength * (n - 1))
    vals <- theOut[lower:upper,]

    interpolation <- approx(x = vals[,"VS"][allRuns[[n]]], y = vals[,"Cost"][allRuns[[n]]])
    interpol[[n]] <- interpolation
}

# green = "#46A55F"
# blue = "#4F8ABA"

ggPlot <- ggplot(optResults, aes(x = VS, y = Cost))
ggPlot <- ggPlot + geom_vline(xintercept = 0.9^3, alpha = 0.5)
ggPlot <- ggPlot + geom_point(col = '#4F8ABA', alpha = 0.2)
for(n in 1:(dim(optResults)[1] / simLength)) {
    ggPlot <- ggPlot + geom_line(data = as.data.frame(interpol[[n]]), mapping = aes(x = x, y = y), col = 'black', alpha = 0.2, size = 0.5)
}
for(n in 1:length(optRuns)) {
    ggPlot <- ggPlot + geom_line(data = as.data.frame(interpol[[optRuns[n]]]), mapping = aes(x = x, y = y), col = "red", alpha = 0.5, size = 0.75)
}
ggPlot <- ggPlot + theme_classic()
ggPlot <- ggPlot + expand_limits(y = round(max(optResults$Cost), digits = -9))
ggPlot <- ggPlot + scale_y_continuous(
    breaks = base::pretty(c(0, round(max(optResults$Cost), digits = -9)), n = 5),
    labels = scales::scientific)
ggPlot <- ggPlot + scale_x_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent(seq(0, 1, 0.2)))
ggPlot <- ggPlot + theme(axis.text.x = element_text(size = 10))
ggPlot <- ggPlot + theme(axis.text.y = element_text(size = 10))
ggPlot <- ggPlot + theme(axis.title = element_text(size = 10))
ggPlot <- ggPlot + theme(axis.line.x = element_line())
ggPlot <- ggPlot + theme(axis.line.y = element_line())
ggPlot <- ggPlot + xlab("Viral Suppression")
ggPlot <- ggPlot + ylab("Additional Cost of Care")
ggPlot <- ggPlot + ggtitle(label = "Cost-effectiveness Frontiers", subtitle = "Red frontiers indicate simulations achieving 73% viral suppression by 2020")
ggPlot <- ggPlot + theme(text = element_text(family = "Avenir Next"))
ggPlot <- ggPlot + theme(panel.background = element_rect(fill = "#F0F0F0"))
ggPlot <- ggPlot + theme(plot.background = element_rect(fill = "#F0F0F0"))
ggPlot

