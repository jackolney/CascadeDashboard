# non-shiny optimisation triggers and setup
# input stuff etc.

source("server/model/best-fit-model.R",                local = FALSE)
source("server/model/beta.R",                          local = FALSE)
source("server/model/initial.R",                       local = FALSE)
source("server/model/parameters.R",                    local = FALSE)
source("server/model/baseline-model.R",                local = FALSE)
source("server/optimisation/functions.R",              local = FALSE)
source("server/optimisation/non-shiny-optimisation.R", local = FALSE)
source("server/optimisation/parameters.R",             local = FALSE)
source("server/optimisation/sim.R",                    local = FALSE)
source("server/projection/CD4-distribution.R",         local = FALSE)

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

input <- c()
input$opt_rho_factor <- 10
input$opt_q_factor <- 1
input$opt_kappa_factor <- 10
input$opt_gamma_factor <- 10
input$opt_sigma_factor <- 10
input$opt_omega_factor <- 10

# Can we run the calibration from here?

theTest <- GetParaMatrix(cParamOut = CalibParamOut, minErrorRun = minErrorRun)

# Testing somem functions

# ------------ #
# OPTIMISATION #
# ------------ #

theOut <- RunNSOptimisation()
# pryr::object_size(theOut)
# save(theOut, file = "Optimisation-Output.RData")
# dir()

dim(theOut)

head(theOut)

ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Omega))

# How do we best quantify this output then?

# Compute some metric to assess the quality of results?

# Cost Per Daly Averted?
theOut$Cost / theOut$VS

plot(theOut$VS / theOut$Cost, type = 'l')

plot(theOut$Cost / theOut$VS, type = 'l')

# Of interventions that achieved at least ___ % VS

range(theOut$VS)

sum(theOut$VS > 0.9)


test <- subset(theOut, theOut$VS > 0.6)

ggplot(test, aes(x = VS, y = Cost)) + geom_point(aes(col = Gamma), alpha = 0.5)
ls()

# Bar plot test.
    # ggOut <- ggplot(theError, aes(runError))
    # ggOut <- ggOut + geom_histogram(aes(fill = ..count..), bins = 30)
    # ggOut <- ggOut + theme_classic()

bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)


reshape2::melt(test)

intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

# Something like this?
# sum(test$Rho / baseline$Rho)

a = round(test$Rho / bestPar[["Rho"]]) - 1

# First, pass if > 1 then 1
sum((round(test$Rho, digits = 4) / bestPar[["Rho"]]) > 1)
sum((round(test$Q, digits = 4) / bestPar[["q"]]) > 1)
sum((round(test$Kappa, digits = 4) / bestPar[["Kappa"]]) < 1)
sum((round(test$Gamma, digits = 4) / bestPar[["Gamma"]]) > 1)
sum(round(test$Sigma, digits = 4) > 0)
sum((round(test$Omega, digits = 4) / bestPar[["Omega"]]) < 1)

test <- subset(theOut, theOut$VS > 0.75)
dim(test)
value <- c(
    sum((round(test$Rho, digits = 4) / bestPar[["Rho"]]) > 1) / dim(test)[1],
    sum((round(test$Q, digits = 4) / bestPar[["q"]]) > 1) / dim(test)[1],
    sum((round(test$Kappa, digits = 4) / bestPar[["Kappa"]]) < 1) / dim(test)[1],
    sum((round(test$Gamma, digits = 4) / bestPar[["Gamma"]]) > 1) / dim(test)[1],
    sum(round(test$Sigma, digits = 4) > 0) / dim(test)[1],
    sum((round(test$Omega, digits = 4) / bestPar[["Omega"]]) < 1) / dim(test)[1]
    )
testOut <- data.frame(intervention, value)
testOut$intervention <- factor(testOut$intervention, levels = intervention)

# This show the proportion of selected runs that use a particular intervention.
ggplot(testOut, aes(x = intervention, y = value)) + geom_bar(aes(fill = intervention), stat = "identity")

# Can we illustrate of the ranked best runs, what combinations of interventions were used?
dim(test)

hm <- dplyr::arrange(test, VS)


okay <- dplyr::mutate(hm,
    testing = ((test$Rho / bestPar[["Rho"]]) > 1),
    linkage = ((test$Q / bestPar[["q"]]) > 1))


head(okay)

sum(okay$testing)
sum(okay$linkage)


# Barplot with HOW MUCH of each intervention was used??
test <- subset(theOut, theOut$VS > 0.5)
value <- c(
    sum(round(test$Rho, digits = 4) / bestPar[["Rho"]]) / dim(test)[1],
    sum(round(test$Q, digits = 4) / bestPar[["q"]]) / dim(test)[1],
    sum(round(test$Kappa, digits = 4) / bestPar[["Kappa"]]) / dim(test)[1],
    sum(round(test$Gamma, digits = 4) / bestPar[["Gamma"]]) / dim(test)[1],
    sum(round(test$Sigma, digits = 4)) / dim(test)[1],
    sum(round(test$Omega, digits = 4) / bestPar[["Omega"]]) / dim(test)[1]
    )
testOut <- data.frame(intervention, value)
testOut$intervention <- factor(testOut$intervention, levels = intervention)

# This show the proportion of selected runs that use a particular intervention.
ggplot(testOut, aes(x = intervention, y = value)) + geom_bar(aes(fill = intervention), stat = "identity")

# I think this shows the average percentage increase in each aspect of care.

# TO ACHIEVE THIS LEVEL OF VIRAL SUPPRESSION [expand to 90-90-90 next week]

mean(test$Rho)
mean(test$Q)
mean(test$Kappa)
mean(test$Gamma)
mean(test$Sigma)
mean(test$Omega)

# pass these values into the modell and generate the same estimates that we produced before.

# put in paragraph???
# table???
baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)

alt <- CallBestModel(
    CalibOut = CalibOut,
    minErrorRun = minErrorRun,
    Rho = mean(test$Rho),
    q = mean(test$Q),
    Kappa = mean(test$Kappa),
    Gamma = mean(test$Gamma),
    Sigma = mean(test$Sigma),
    Omega = mean(test$Omega)
    )

# What about negative values though? (because we are strengthening care throughout.)
# maybe just say, over the next 5 years we need to test ___ x many people. not MORE people.

# we need to test ___ more people
base_answer <- cumsum(baseline$Dx)[251] - baseline$Dx[1]
alt_answer <- cumsum(alt$Dx)[251] - alt$Dx[1]
scales::comma(round(alt_answer - base_answer, digits = 0))


# we need to link ___ more people
base_answer <- cumsum(baseline$Care)[251] - baseline$Care[1]
alt_answer <- cumsum(alt$Care)[251] - alt$Care[1]
scales::comma(round(alt_answer - base_answer, digits = 0))

# we need to retain __ more people
base_answer <- cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]
alt_answer <- cumsum(alt$PreLtfu)[251] - alt$PreLtfu[1]
scales::comma(round(base_answer - alt_answer, digits = 0))

# we need to initiate ____ more people
base_answer <- cumsum(baseline$Tx)[251] - baseline$Tx[1]
alt_answer <- cumsum(alt$Tx)[251] - alt$Tx[1]
scales::comma(round(alt_answer - base_answer, digits = 0))

# we need to adhere ___ more people
base_answer <- cumsum(baseline$Vs)[251] - baseline$Vs[1]
alt_answer <- cumsum(alt$Vs)[251] - alt$Vs[1]
scales::comma(round(alt_answer - base_answer, digits = 0))

# we need to retain ___ more people
base_answer <- cumsum(baseline$Ltfu)[251] - baseline$Ltfu[1]
alt_answer <- cumsum(alt$Ltfu)[251] - alt$Ltfu[1]
scales::comma(round(base_answer - alt_answer, digits = 0))

# At an AVERAGE COST COST COST of....
base_answer <- baseline$TotalCost[251]
alt_answer <- alt$TotalCost[251]
scales::comma(round(alt_answer - base_answer, digits = 0))


### TESTING AREA ####

intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

test <- subset(theOut, theOut$VS > -1)
dim(test)

dplyr::arrange(theOut, VS)[1:100,]

value <- c(
    sum((round(test$Rho, digits = 4) / bestPar[["Rho"]]) > -1) / dim(test)[1],
    sum((round(test$Q, digits = 4) / bestPar[["q"]]) > -1) / dim(test)[1],
    sum((round(test$Kappa, digits = 4) / bestPar[["Kappa"]]) > -1) / dim(test)[1],
    sum((round(test$Gamma, digits = 4) / bestPar[["Gamma"]]) > -1) / dim(test)[1],
    sum(round(test$Sigma, digits = 4) > -1) / dim(test)[1],
    sum((round(test$Omega, digits = 4) / bestPar[["Omega"]]) > -1) / dim(test)[1]
    )

strength <- c(
    sum(round(test$Rho, digits = 4) / bestPar[["Rho"]]) / dim(test)[1],
    sum(round(test$Q, digits = 4) / bestPar[["q"]]) / dim(test)[1],
    sum(round(test$Kappa, digits = 4) / bestPar[["Kappa"]]) / dim(test)[1],
    sum(round(test$Gamma, digits = 4) / bestPar[["Gamma"]]) / dim(test)[1],
    sum(round(test$Sigma, digits = 4)) / dim(test)[1],
    sum(round(test$Omega, digits = 4) / bestPar[["Omega"]]) / dim(test)[1]
    )

testOut <- data.frame(intervention, value, strength)
testOut$intervention <- factor(testOut$intervention, levels = intervention)

# This show the proportion of selected runs that use a particular intervention.
ggplot(testOut, aes(x = intervention, y = value)) + geom_bar(aes(fill = strength), stat = "identity")


# Just for testing intervention, then expand.

test <- subset(theOut, theOut$VS > -1)
dim(test)
sum((round(test$Rho, digits = 4) / bestPar[["Rho"]]) > -1)


sum((test$Rho / bestPar[["Rho"]]) == 1)

# total proportional increase in rates from each intervention??
test <- subset(theOut, theOut$VS > 0.5)
dim(test)

# this whether the intervention is ON or not is BORING.
sum(unlist(lapply((test$Rho / bestPar[["Rho"]]), function(x) if (x > 1) TRUE)))

###########################
### I THINK THIS IS IT ####
###########################

value <- c(
sum(unlist(lapply((test$Rho / bestPar[["Rho"]]), function(x) if (x > 1) x))) / dim(test)[1],
sum(unlist(lapply((test$Q / bestPar[["q"]]), function(x) if (x > 1) x))) / dim(test)[1],
sum(unlist(lapply((bestPar[["Kappa"]] / test$Kappa), function(x) if (x > 1) x))) / dim(test)[1],
sum(unlist(lapply((test$Gamma / bestPar[["Gamma"]]), function(x) if (x > 1) x))) / dim(test)[1],
sum(unlist(lapply((test$Sigma), function(x) if (x > 1) x))) / dim(test)[1],
sum(unlist(lapply((bestPar[["Omega"]] / test$Omega), function(x) if (x > 1) x))) / dim(test)[1]
)

barplot(value)

testing <- (test$Rho / bestPar[["Rho"]])

head(testing)

short <- testing[1:10]

length(short)

sum(short > 1)

# Sum all values not equal to one.

sum(unlist(lapply(short, function(x) if(x > 1) x)))

# Try the mean
mean(unlist(lapply(short, function(x) if(x > 1) x)))

# As a proportion of all simulations (n = 10)

##### EXPAND
