# non-shiny optimisation triggers and setup
# input stuff etc.

source("server/model/beta.R",                  local = FALSE)
source("server/model/initial.R",               local = FALSE)
source("server/model/mean-model.R",            local = FALSE)
source("server/model/parameters.R",            local = FALSE)
source("server/optimisation/functions.R",      local = FALSE)
source("server/optimisation/parameters.R",     local = FALSE)
source("server/optimisation/sim.R",            local = FALSE)
source("server/projection/CD4-distribution.R", local = FALSE)

# reactive input setup
MasterCD4_2015 <- GetCD4Distribution2015("Kenya")
MasterData <- GetCountryData("Kenya")

input <- c()
input$TestingCheck      <- TRUE
input$LinkageCheck      <- TRUE
input$PreRetentionCheck <- TRUE
input$InitiationCheck   <- TRUE
input$AdherenceCheck    <- TRUE
input$RetentionCheck    <- TRUE
input$optimParamLength  <- 4


input$userOptRho_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 4),
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 4) * 10
)

input$userOptq_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["q"]], digits = 4),
    1
)

input$userOptKappa_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4) / 10,
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4)
)

input$userOptGamma_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4),
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4) * 10
)

input$userOptSigma_Range <- c(0, 5)

input$userOptOmega_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4) / 10,
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4)
)


# Testing somem functions

###########
## TESTS ##
###########

## TEST ONE ##

# Will need to be packaged properly.

# If we just test the first output of the below, it should give me the baseline parameter values
theTest <- GetParaMatrix(calibParamOut = CalibParamOut)[1,]

theComp <- data.frame(
    Rho =   round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]],   digits = 4),
    Q =     round(lapply(CalibParamOut, function(x) {return(mean(x))})[["q"]],     digits = 4),
    Kappa = round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 4),
    Gamma = round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 4),
    Sigma = 0,
    Omega = round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 4)
    )

theTest
theComp

# testthat::expect_equal(theTest, theComp, tolerance = 1)

testthat::expect_true(sum(theTest == theComp) == 6)

# all.equal doesn't work
# all.equal(round(theTest, 3), round(theComp, 3))


## TEST TWO ##

# A comparison between GetOptpar() and GetMeanPar()
# With any combination of interventions (perhaps test that too)

theOpt <- GetOptPar(
    masterCD4 = MasterCD4_2015,
    data = MasterData,
    iterationParam = GetParaMatrix(calibParamOut = CalibParamOut)[1,],
    calibParamOut = CalibParamOut)

theMean <- GetMeanPar(
    masterCD4 = MasterCD4_2015,
    data = MasterData,
    calibParamOut = CalibParamOut)

# We should get equal
testthat::expect_equal(theOpt, theMean)

theOpt[7]
theMean[7]
theMean["Sigma"]

lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["q"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["sigma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]]

lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["q"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["sigma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["theta"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["mu"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["p"]]

round(lapply(calibParamOut, function(x) {return(mean(x))})[["mu"]], digit = 4)

CalibParamOut
ParamMaxMin

round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 3)
round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 3) * 5

dim(theList[[1]])

names(theList[[1]])

## TEST THREE ##

# Testing the output - of observeEvent(input$optimStart, {DO SOME SHIT})

some <- CallMeanModel()
same <- theList[[1]]

testthat::expect_equal(some, same, tolerance = 1)

some$N == same$N

plot(some$N, type = 'l', lwd = 2)
lines(same$N, lwd = 2, col = "red")

sum(some$DALY) / sum(same$DALY)
# they definitely need to be the same.

# Rounding issues - URGH.


# EXPAND TESTING, FOR ALL!
# TEST FOUR SAMPLES OF HIV TESTING.
# ENSURE PLOT IS CORRECT
# ADD TWO INTERVENTIONS
sum(some$DALY)
