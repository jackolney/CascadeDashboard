# non-shiny optimisation triggers and setup
# input stuff etc.
graphics.off()
quartz.options(w = 10, h = 5)

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

OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.01
OptInput$intValue_omega <- parRange["rho", "min"]

# Can we run the calibration from here?

theTest <- GetParaMatrix(cParamOut = CalibParamOut, minErrorRun = minErrorRun, length = 4)
dim(theTest)
theTest

# Testing somem functions

# ------------ #
# OPTIMISATION #
# ------------ #

theOut <- RunNSOptimisation()
theOut

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

sum(theOut$VS > 0.5)


test <- subset(theOut, theOut$VS > 0.6)
dim(test)
# optResult <- test

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

test <- subset(theOut, theOut$VS > 0.6)
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
test <- subset(theOut, theOut$VS > 0.6)
dim(test)
strength <- c(
    sum(unlist(lapply((test$Rho / bestPar[["Rho"]]), function(x) if (x > 1) x))) / dim(test)[1],
    sum(unlist(lapply((test$Q / bestPar[["q"]]), function(x) if (x > 1) x))) / dim(test)[1],
    sum(unlist(lapply((bestPar[["Kappa"]] / test$Kappa), function(x) if (x > 1) x))) / dim(test)[1],
    sum(unlist(lapply((test$Gamma / bestPar[["Gamma"]]), function(x) if (x > 1) x))) / dim(test)[1],
    sum(unlist(lapply((test$Sigma), function(x) if (x > 1) x))) / dim(test)[1],
    sum(unlist(lapply((bestPar[["Omega"]] / test$Omega), function(x) if (x > 1) x))) / dim(test)[1]
)
barplot(strength)

value <- c(
    sum(unlist(lapply((test$Rho / bestPar[["Rho"]]), function(x) if (x > 1) TRUE))) / dim(test)[1],
    sum(unlist(lapply((test$Q / bestPar[["q"]]), function(x) if (x > 1) TRUE))) / dim(test)[1],
    sum(unlist(lapply((bestPar[["Kappa"]] / test$Kappa), function(x) if (x > 1) TRUE))) / dim(test)[1],
    sum(unlist(lapply((test$Gamma / bestPar[["Gamma"]]), function(x) if (x > 1) TRUE))) / dim(test)[1],
    sum(unlist(lapply((test$Sigma), function(x) if (x > 1) TRUE))) / dim(test)[1],
    sum(unlist(lapply((bestPar[["Omega"]] / test$Omega), function(x) if (x > 1) TRUE))) / dim(test)[1]
)

testing <- data.frame(intervention, value, strength)
testing$intervention <- factor(testing$intervention, levels = intervention)

# Convert to just a table DT::renderDataTable()
# testOne <- ggplot(testing, aes(x = intervention, y = value)) +
#     geom_bar(aes(fill = intervention), stat = "identity") +
#     theme_classic() +
#     theme(legend.position = "none") +
#     ggtitle("Proportion of simulations in which each intervention was active") +
#     theme(axis.text.x = element_text(size = 12)) +
#     theme(axis.text.y = element_text(size = 12)) +
#     theme(title =       element_text(size = 13)) +
#     theme(axis.title.y = element_blank()) +
#     theme(axis.title.x = element_blank()) +
#     theme(axis.line.y = element_line()) +
#     scale_y_continuous(labels = scales::percent, expand = c(0, 0))

testTwo <- ggplot(testing, aes(x = intervention, y = strength)) +
    geom_bar(aes(fill = intervention), stat = "identity") +
    theme_classic() +
    theme(legend.position = "none") +
    ggtitle("Average increase in each aspect of the cascade") +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(title =       element_text(size = 13)) +
    theme(axis.title.y = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.line.y = element_line()) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0))
testTwo

date()

# have an inline bar of whether this was used.
testing$value
# Then all the bull shit with average values.

# Then below the master cost / impact plot.
# dropdown for different interventions


head(test)


ggplot(test, aes(x = VS, y = Cost)) + geom_point(aes(col = Rho, alpha = Gamma))

# gridExtra::grid.arrange(testOne, testTwo, ncol = 2, nrow = 1)

a = value * strength

barplot(a)

# ggOne = average
# ggTwo = average increase in each area of the cascade


# Calculate which interventions are combinations.


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

##################
# DATA TABLE FUN #
##################

# require(DT)
# DT::datatable(iris) %>%
  formatStyle('Sepal.Length', fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle(
    'Sepal.Width',
    color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
  ) %>%
  formatStyle(
    'Petal.Length',
    background = styleColorBar(iris$Petal.Length, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'Species',
    transform = 'rotateX(0deg) rotateY(0deg) rotateZ(0deg)',
    backgroundColor = styleEqual(
      unique(iris$Species), c('lightblue', 'lightgreen', 'lightpink')
    )
  )

# setup

baseline <- CallBestModel(
    CalibOut = CalibOut,
    minErrorRun = minErrorRun)

alt <- CallBestModel(
    CalibOut = CalibOut,
    minErrorRun = minErrorRun,
    Rho = mean(selectedResults$Rho),
    q = mean(selectedResults$Q),
    Kappa = mean(selectedResults$Kappa),
    Gamma = mean(selectedResults$Gamma),
    Sigma = mean(selectedResults$Sigma),
    Omega = mean(selectedResults$Omega))

Intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention"
)

Description <- c(
    "The number of individuals requiring diagnosis is:",
    "The number of individuals that need to be linked to care are:",
    "The number of individuals that need to be retained in pre-ART care is:",
    "The number of individuals that need to be initiated onto treatment are:",
    "The number of individuals that need to fully adhere to treatment are:",
    "The number of individuals that need to be retained on ART are:"
)

# The values used in uiOutput()
Value <- c(
    round(cumsum(alt$Dx)[251]      - alt$Dx[1],      digits = 0),
    round(cumsum(alt$Care)[251]    - alt$Care[1],    digits = 0),
    round(cumsum(alt$PreLtfu)[251] - alt$PreLtfu[1], digits = 0),
    round(cumsum(alt$Tx)[251]      - alt$Tx[1],      digits = 0),
    round(cumsum(alt$Vs)[251]      - alt$Vs[1],      digits = 0),
    round(cumsum(alt$Ltfu)[251]    - alt$Ltfu[1],    digits = 0)
)

# The proportion of simulations that required that thing (then will add a bar in post processing)
Use <- c(
    sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
    sum(unlist(lapply((selectedResults$Q     / bestPar[["q"]]),        function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
    sum(unlist(lapply((bestPar[["Kappa"]]    / selectedResults$Kappa), function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
    sum(unlist(lapply((selectedResults$Gamma / bestPar[["Gamma"]]),    function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
    sum(unlist(lapply((selectedResults$Sigma),                         function(x) if (x > 1) TRUE))) / dim(selectedResults)[1],
    sum(unlist(lapply((bestPar[["Omega"]]    / selectedResults$Omega), function(x) if (x > 1) TRUE))) / dim(selectedResults)[1]
)
Use <- scales::percent(Use)

optimDT <- data.frame(Intervention, Description, Value, Use)

DT::datatable(optimDT, options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
)) %>% formatStyle(
    columns = 'Intervention',
    color = 'red',
    backgroundColor = '#fff',
    fontWeight = 'bold'
) %>% formatStyle(
    columns = 'Value',
    background = styleColorBar(data = optimDT$Value, color = 'lightblue'),
    backgroundSize = '100% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
) %>% formatCurrency(columns = 'Value', currency = '', interval = 3)


### ADDING ROWS ###

optResult

names(optResult)

optResult <- dplyr::mutate(optResult,
    'Testing' = scales::percent(test$Rho / bestPar[["Rho"]]),
    'Linkage' = scales::percent(round(test$Q / bestPar[["q"]], digits = 2)),
    'Pre-ART Retention' = scales::percent(round(bestPar[["Kappa"]] / test$Kappa, digits = 0)),
    'Initiation' = scales::percent(test$Gamma / bestPar[["Gamma"]]),
    'Adherence' = scales::percent(round(test$Sigma, digits = 0)),
    'ART Retention' = scales::percent(round(bestPar[["Omega"]] / test$Omega, digits = 0))
)

optResult[["Testing"]] <- factor(optResult[["Testing"]], levels = unique(optResult[["Testing"]]))
optResult[["Linkage"]] <- factor(optResult[["Linkage"]], levels = unique(optResult[["Linkage"]]))
optResult[["Pre-ART Retention"]] <- factor(optResult[["Pre-ART Retention"]], levels = unique(optResult[["Pre-ART Retention"]]))
optResult[["Initiation"]] <- factor(optResult[["Initiation"]], levels = unique(optResult[["Initiation"]]))
optResult[["Adherence"]] <- factor(optResult[["Adherence"]], levels = unique(optResult[["Adherence"]]))
optResult[["ART Retention"]] <- factor(optResult[["ART Retention"]], levels = unique(optResult[["ART Retention"]]))


unique(optResult[["Testing"]])

unlist(lapply((test$Rho / bestPar[["Rho"]]), function(x) if (x > 1) x))

range(test$Q / bestPar[["q"]])

(test$Q / bestPar[["q"]]) > 2

round(test$Q / bestPar[["q"]], digits = 2)

unlist(lapply((test$Q / bestPar[["q"]]), function(x) if (x > 1) x))



color_from_middle <- function(data, color1, color2) {
  max_val <- max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val, color1, max_val, color1, color2, color2, max_val, max_val))
}

data <- data.frame(a=c(rep("a",9)),value=c(-4,-3,-2,-1,0,1,2,3,4))

DT::datatable(data) %>%
  DT::formatStyle('value', background = color_from_middle(data$value, '#FF6347', '#90EE90'))



isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'




#####

selectedResults <- subset(theOut, theOut$VS > 0)
dim(selectedResults)



intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

strength <- c(
    sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) x))) / dim(selectedResults)[1],
    sum(unlist(lapply((selectedResults$Q     / bestPar[["q"]]),        function(x) if (x > 1) x))) / dim(selectedResults)[1],
    sum(unlist(lapply((bestPar[["Kappa"]]    / selectedResults$Kappa), function(x) if (x > 1) x))) / dim(selectedResults)[1],
    sum(unlist(lapply((selectedResults$Gamma / bestPar[["Gamma"]]),    function(x) if (x > 1) x))) / dim(selectedResults)[1],
    sum(unlist(lapply((selectedResults$Sigma),                         function(x) if (x > 1) x))) / dim(selectedResults)[1],
    sum(unlist(lapply((bestPar[["Omega"]]    / selectedResults$Omega), function(x) if (x > 1) x))) / dim(selectedResults)[1]
)

# build data.frame
resultOut <- data.frame(intervention, strength)
ggplot(resultOut, aes(x = intervention, y = strength)) + geom_bar(stat = "identity")

####

intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

strength <- c(
    sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) x))) / sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) TRUE))),
    sum(unlist(lapply((selectedResults$Q     / bestPar[["q"]]),        function(x) if (x > 1) x))) / sum(unlist(lapply((selectedResults$Q     / bestPar[["q"]]),        function(x) if (x > 1) TRUE))),
    sum(unlist(lapply((bestPar[["Kappa"]]    / selectedResults$Kappa), function(x) if (x > 1) x))) / sum(unlist(lapply((bestPar[["Kappa"]]    / selectedResults$Kappa), function(x) if (x > 1) TRUE))),
    sum(unlist(lapply((selectedResults$Gamma / bestPar[["Gamma"]]),    function(x) if (x > 1) x))) / sum(unlist(lapply((selectedResults$Gamma / bestPar[["Gamma"]]),    function(x) if (x > 1) TRUE))),
    sum(unlist(lapply((selectedResults$Sigma),                         function(x) if (x > 1) x))) / sum(unlist(lapply((selectedResults$Sigma),                         function(x) if (x > 1) TRUE))),
    sum(unlist(lapply((bestPar[["Omega"]]    / selectedResults$Omega), function(x) if (x > 1) x))) / sum(unlist(lapply((bestPar[["Omega"]]    / selectedResults$Omega), function(x) if (x > 1) TRUE)))
)

# build data.frame
resultOut2 <- data.frame(intervention, strength)
ggplot(resultOut, aes(x = intervention, y = strength)) + geom_bar(stat = "identity")


resultOut
resultOut2


sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) x))) / dim(selectedResults)[1]

sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) x))) / sum(unlist(lapply((selectedResults$Rho   / bestPar[["Rho"]]),      function(x) if (x > 1) TRUE)))



sum(unlist(lapply((selectedResults$Q     / bestPar[["q"]]),        function(x) if (x > 1) x)))

range(selectedResults$Q / bestPar[["q"]])

range(selectedResults$Q)



1 / 0.2

bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)

test <- theOut
theOut
test[1,]

a <- (test$Rho / bestPar[["Rho"]]) * test$VS

mean(a)


unlist(lapply((test$Rho / bestPar[["Rho"]]), function(x, vs) if (x > 1) x * vs, vs = test$VS))[1]

(test$Rho / bestPar[["Rho"]])[2] * test$VS[2]


calc = test$Rho[1] / bestPar[["Rho"]]
vs = test$VS[1]

WeightResults <- function(calc, vs) {
    if (calc > 1) calc * vs
}

WeightResults(calc = calc, vs = vs)

for(i in 1:dim(test)[1]) {
    i = 2
    WeightResults(calc = test$Rho[i] / bestPar[["Rho"]], vs = test$VS[i])
}
mean(out)

i = 2
WeightResults(calc = test$Rho[i] / bestPar[["Rho"]], vs = test$VS[i])

# lapply((test$Rho / bestPar[["Rho"]]), function(x, vs) if (x > 1) x * vs, vs = test$VS)

WeightResults <- function(x, vs) {
    if (x > 1) x * vs
}

lapply((test$Rho / bestPar[["Rho"]]), function(x, vs) if (x > 1) x * vs, vs = test$VS)


# What if I take a subset first?
a <- subset(test, test$Rho / bestPar[["Rho"]] > 1)
length(a)


#######################################
### THIS IS NEW AND THIS IS COOL!!! ###
#######################################

test <- subset(theOut, theOut$VS > 0)
dim(test)
hi <- test

bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)


# Add columns to data.frame
hi$testing <- hi$Rho / bestPar[["Rho"]]
hi$linkage <- hi$Q / bestPar[["q"]]
hi$preRetention <- bestPar[["Kappa"]] / hi$Kappa
hi$initiation <- hi$Gamma / bestPar[["Gamma"]]
hi$adherence <- hi$Sigma
hi$retention <- bestPar[["Omega"]] / hi$Omega

intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

strength <- c(
    mean(hi[hi$testing > 1, "testing"] * hi[hi$testing > 1, "VS"]),
    mean(hi[hi$linkage > 1, "linkage"] * hi[hi$linkage > 1, "VS"]),
    mean(hi[hi$preRetention > 1, "preRetention"] * hi[hi$preRetention > 1, "VS"]),
    mean(hi[hi$initiation > 1, "initiation"] * hi[hi$initiation > 1, "VS"]),
    mean(hi[hi$adherence > 1, "adherence"] * hi[hi$adherence > 1, "VS"]),
    mean(hi[hi$retention > 1, "retention"] * hi[hi$retention > 1, "VS"])
)

strength2 <- c(
    mean(hi[, "testing"] * hi[, "VS"]),
    mean(hi[, "linkage"] * hi[, "VS"]),
    mean(hi[, "preRetention"] * hi[, "VS"]),
    mean(hi[, "initiation"] * hi[, "VS"]),
    mean(hi[, "adherence"] * hi[, "VS"]),
    mean(hi[, "retention"] * hi[, "VS"])
)

strength3 <- c(
    sum(hi[hi$testing > 1, "testing"] * hi[hi$testing > 1, "VS"]) / dim(hi)[1],
    sum(hi[hi$linkage > 1, "linkage"] * hi[hi$linkage > 1, "VS"])  / dim(hi)[1],
    sum(hi[hi$preRetention > 1, "preRetention"] * hi[hi$preRetention > 1, "VS"])  / dim(hi)[1],
    sum(hi[hi$initiation > 1, "initiation"] * hi[hi$initiation > 1, "VS"])  / dim(hi)[1],
    sum(hi[hi$adherence > 1, "adherence"] * hi[hi$adherence > 1, "VS"]) / dim(hi)[1],
    sum(hi[hi$retention > 1, "retention"] * hi[hi$retention > 1, "VS"])  / dim(hi)[1]
)

# build data.frame
resultOut <- data.frame(intervention, strength)
resultOut2 <- data.frame(intervention, strength2)
resultOut3 <- data.frame(intervention, strength3)

resultOut$intervention <- factor(resultOut$intervention, levels = intervention)
resultOut2$intervention <- factor(resultOut2$intervention, levels = intervention)
resultOut3$intervention <- factor(resultOut3$intervention, levels = intervention)


a = ggplot(resultOut, aes(x = intervention, y = strength)) + geom_bar(stat = "identity")
b = ggplot(resultOut2, aes(x = intervention, y = strength2)) + geom_bar(stat = "identity")
c = ggplot(resultOut3, aes(x = intervention, y = strength3)) + geom_bar(stat = "identity")

gridExtra::grid.arrange(a, b, c, nrow = 1, ncol = 3)



# Thursday

just take a subset

section <- hi[1:10,]

mean(section[section$testing > 1, "testing"] * section[section$testing > 1, "VS"])

section[section$testing > 1, "testing"]
section[section$testing > 1, "VS"]

mean(hi[hi$adherence > 1, "adherence"] * hi[hi$adherence > 1, "VS"])


Viral <- 0
Viral <- Viral + 1
# Subset data using opt_VS_cutoff
selectedResults <- subset(theOut, theOut$VS >= (Viral / 10))
dim(selectedResults)

selectedResults$testing      <- selectedResults$Rho   / bestPar[["Rho"]]
selectedResults$linkage      <- selectedResults$Q     / bestPar[["q"]]
selectedResults$preRetention <- bestPar[["Kappa"]]    / selectedResults$Kappa
selectedResults$initiation   <- selectedResults$Gamma / bestPar[["Gamma"]]
selectedResults$adherence    <- selectedResults$Sigma
selectedResults$retention    <- bestPar[["Omega"]]    / selectedResults$Omega

intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

strength <- c(
    sum(selectedResults[selectedResults$testing      > 1, "testing"]      * selectedResults[selectedResults$testing      > 1, "VS"]) / dim(selectedResults)[1],
    sum(selectedResults[selectedResults$linkage      > 1, "linkage"]      * selectedResults[selectedResults$linkage      > 1, "VS"]) / dim(selectedResults)[1],
    sum(selectedResults[selectedResults$preRetention > 1, "preRetention"] * selectedResults[selectedResults$preRetention > 1, "VS"]) / dim(selectedResults)[1],
    sum(selectedResults[selectedResults$initiation   > 1, "initiation"]   * selectedResults[selectedResults$initiation   > 1, "VS"]) / dim(selectedResults)[1],
    sum(selectedResults[selectedResults$adherence    > 1, "adherence"]    * selectedResults[selectedResults$adherence    > 1, "VS"]) / dim(selectedResults)[1],
    sum(selectedResults[selectedResults$retention    > 1, "retention"]    * selectedResults[selectedResults$retention    > 1, "VS"]) / dim(selectedResults)[1]
)

# build data.frame
resultOut <- data.frame(intervention, strength)

# Check level order
resultOut$intervention <- factor(resultOut$intervention, levels = intervention)

# This show the proportion of selected runs that use a particular intervention.
ggOut <- ggplot(resultOut, aes(x = intervention, y = strength))
ggOut <- ggOut + geom_bar(aes(fill = intervention), stat = "identity")
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + ggtitle(
    label = paste("VS =", Viral),
    subtitle =
        paste0("Percentage increase brought about by ",
            scales::comma(dim(selectedResults)[1]),
            " interventions, achieving ",
            input$opt_VS_cutoff,
            "% viral suppression"
        )
    )
ggOut <- ggOut + theme(legend.position = "none")
ggOut <- ggOut + theme(axis.text.x = element_text(size = 14))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 14))
ggOut <- ggOut + theme(title =       element_text(size = 15))
ggOut <- ggOut + theme(axis.title.y = element_blank())
ggOut <- ggOut + theme(axis.title.x = element_blank())
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
ggOut


ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Sigma))


### What about a totally different approach
test <- subset(theOut, theOut$VS > 0)
dim(test)

head(test)

bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)


# Add columns to data.frame
test$testing <- test$Rho / bestPar[["Rho"]]
test$linkage <- test$Q / bestPar[["q"]]
test$preRetention <- bestPar[["Kappa"]] / test$Kappa
test$initiation <- test$Gamma / bestPar[["Gamma"]]
test$adherence <- test$Sigma
test$retention <- bestPar[["Omega"]] / test$Omega

head(test)

cols <- ggColorHue(6)

ggplot(test, aes(x = testing, y = VS)) + geom_point(alpha = 0.01, col = cols[1]) +
geom_point(aes(x = linkage, y = VS), col = cols[2], alpha = 0.01) +
geom_point(aes(x = preRetention, y = VS), col = cols[3], alpha = 0.01) +
geom_point(aes(x = initiation, y = VS), col = cols[4], alpha = 0.01) +
geom_point(aes(x = adherence, y = VS), col = cols[5], alpha = 0.01) +
geom_point(aes(x = retention, y = VS), col = cols[6], alpha = 0.01)


ggplot(test) + stat_summary(aes(x = testing, y = VS), fun.y = "mean", colour = cols[1], size = 1, geom = "line", alpha = 0.5) +
stat_summary(aes(x = linkage, y = VS), fun.y = "mean", colour = cols[2], size = 1, geom = "line", alpha = 0.5) +
stat_summary(aes(x = preRetention, y = VS), fun.y = "mean", colour = cols[3], size = 1, geom = "line", alpha = 0.5) +
stat_summary(aes(x = initiation, y = VS), fun.y = "mean", colour = cols[4], size = 1, geom = "line", alpha = 0.5) +
stat_summary(aes(x = adherence, y = VS), fun.y = "mean", colour = cols[5], size = 1, geom = "line", alpha = 0.5) +
stat_summary(aes(x = retention, y = VS), fun.y = "mean", colour = cols[6], size = 1, geom = "line", alpha = 0.5)

ggplot(test) + stat_summary(aes(x = testing, y = VS), fun.y = "mean", colour = cols[1], size = 2, geom = "point") +
stat_summary(aes(x = linkage, y = VS), fun.y = "mean", colour = cols[2], size = 2, geom = "point") +
stat_summary(aes(x = preRetention, y = VS), fun.y = "mean", colour = cols[3], size = 2, geom = "point") +
stat_summary(aes(x = initiation, y = VS), fun.y = "mean", colour = cols[4], size = 2, geom = "point") +
stat_summary(aes(x = adherence, y = VS), fun.y = "mean", colour = cols[5], size = 2, geom = "point") +
stat_summary(aes(x = retention, y = VS), fun.y = "mean", colour = cols[6], size = 2, geom = "point")

geom_point(aes(x = linkage, y = VS), col = cols[2], alpha = 0) + stat_summary(aes(x = linkage, y = VS), fun.y = "mean", colour = cols[2], size = 2, geom = "point")
geom_point(aes(x = preRetention, y = VS), col = cols[3], alpha = 0) +
geom_point(aes(x = initiation, y = VS), col = cols[4], alpha = 0) +
geom_point(aes(x = adherence, y = VS), col = cols[5], alpha = 0) +
geom_point(aes(x = retention, y = VS), col = cols[6], alpha = 0)



# ggplot(test[test$testing > 1,], aes(x = cumsum(VS))) + geom_density(col = "red") +
# geom_density(data = test[test$adherence > 1,], aes(x = cumsum(VS)), col = "blue")


# Density, strength vs. VS plot.


ggplot(test, aes(VS)) + geom_histogram(aes(fill = ..count..), bins = 50)

# Can we melt everything to see the combinations.
dim(test)
# hi <- test[1:10,]
hi <- test

reshape2::melt(data = hi,
    id.vars = c('testing', 'linkage', 'preRetention', 'initiation', 'adherence', 'retention'),
    variable.name = 'A',
    value.name = 'VS')


names(hi)

for(i in 1:dim(hi)[1]) {
    hi[i,'testing'] <- if(hi[i,'testing'] > 1) {TRUE} else {FALSE}
    hi[i,'linkage'] <- if(hi[i,'linkage'] > 1) {TRUE} else {FALSE}
    hi[i,'preRetention'] <- if(hi[i,'preRetention'] > 1) {TRUE} else {FALSE}
    hi[i,'initiation'] <- if(hi[i,'initiation'] > 1) {TRUE} else {FALSE}
    hi[i,'adherence'] <- if(hi[i,'adherence'] > 1) {TRUE} else {FALSE}
    hi[i,'retention'] <- if(hi[i,'retention'] > 1) {TRUE} else {FALSE}
}

hi$intNumber <- seq(1, dim(hi)[1])

hihi <- reshape2::melt(data = hi,
    id.vars = c('testing', 'linkage', 'preRetention', 'initiation', 'adherence', 'retention'),
    measure.vars = 'VS')


for(i in 1:dim(hi)[1]) {
    hi[i,"intNumber"] <- 0
    hi[i,"intNumber"] <- if (hi[i,'testing'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'linkage'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'preRetention'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'initiation'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'adherence'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'retention'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
}

ggplot(hi, aes(x = intNumber, y = VS)) + geom_point(aes(col = as.factor(testing)))

as.factor(hi$testing)

hi <- test[1:10,]
dim(hi)

hi

# Number each intervention
hi$intNumber <- seq(1, dim(hi)[1])

hi

#### penalty for multiple interventions. ####

hi <- subset(theOut, theOut$VS > 0)
# selectedResults <- hi

bestPar <- GetBestPar(
        masterCD4 = MasterCD4_2015,
        data = MasterData,
        calibParamOut = CalibParamOut,
        minErrorRun = minErrorRun)


# Add columns to data.frame
hi$testing <- hi$Rho / bestPar[["Rho"]]
hi$linkage <- hi$Q / bestPar[["q"]]
hi$preRetention <- bestPar[["Kappa"]] / hi$Kappa
hi$initiation <- hi$Gamma / bestPar[["Gamma"]]
hi$adherence <- hi$Sigma
hi$retention <- bestPar[["Omega"]] / hi$Omega

test_time <- proc.time()
for(i in 1:dim(hi)[1]) {
    hi[i,"intNumber"] <- 0
    hi[i,"intNumber"] <- if (hi[i,'testing'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'linkage'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'preRetention'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'initiation'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'adherence'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
    hi[i,"intNumber"] <- if (hi[i,'retention'] > 1) {hi[i,"intNumber"] + 1} else {hi[i,"intNumber"]}
}
proc.time() - test_time

intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention")

strength <- c(
    sum( hi[hi$testing      > 1, "testing"]      * hi[hi$testing      > 1, "VS"] * (1 / hi[hi$testing > 1, "intNumber"]) ) / dim(hi)[1],
    sum( hi[hi$linkage      > 1, "linkage"]      * hi[hi$linkage      > 1, "VS"] * (1 / hi[hi$linkage > 1, "intNumber"]) ) / dim(hi)[1],
    sum( hi[hi$preRetention > 1, "preRetention"] * hi[hi$preRetention > 1, "VS"] * (1 / hi[hi$preRetention > 1, "intNumber"]) ) / dim(hi)[1],
    sum( hi[hi$initiation   > 1, "initiation"]   * hi[hi$initiation   > 1, "VS"] * (1 / hi[hi$initiation > 1, "intNumber"]) ) / dim(hi)[1],
    sum( hi[hi$adherence    > 1, "adherence"]    * hi[hi$adherence    > 1, "VS"] * (1 / hi[hi$adherence > 1, "intNumber"]) ) / dim(hi)[1],
    sum( hi[hi$retention    > 1, "retention"]    * hi[hi$retention    > 1, "VS"] * (1 / hi[hi$retention > 1, "intNumber"]) ) / dim(hi)[1]
)

# build data.frame
resultOut <- data.frame(intervention, strength)
resultOut$intervention <- factor(resultOut$intervention, levels = intervention)

ggplot(resultOut, aes(x = intervention, y = strength)) + geom_bar(stat = "identity")


##############
##############
##############
##############
##############

# Subset data using opt_VS_cutoff
selectedResults <- subset(theOut, theOut$VS >= 0)

baseline <- CallBestModel(
    CalibOut = CalibOut,
    minErrorRun = minErrorRun)

alt <- CallBestModel(
    CalibOut = CalibOut,
    minErrorRun = minErrorRun,
    Rho = mean(selectedResults$Rho),
    q = mean(selectedResults$Q),
    Kappa = mean(selectedResults$Kappa),
    Gamma = mean(selectedResults$Gamma),
    Sigma = mean(selectedResults$Sigma),
    Omega = mean(selectedResults$Omega))

Intervention <- c(
    "Testing",
    "Linkage",
    "Pre-ART Retention",
    "Initiation",
    "Adherence",
    "ART Retention"
)

Description <- c(
    "The number of additional individuals requiring diagnosis",
    "The number of additional individuals that need to be linked to care",
    "The number of additional individuals that need to be retained in pre-ART care",
    "The number of additional individuals that need to be initiated onto treatment",
    "The number of additional individuals that need to fully adhere to treatment",
    "The number of additional individuals that need to be retained on ART"
)

# The values used in uiOutput()
Value <- c(
    round(  (cumsum(alt$Dx)[251]           - alt$Dx[1]          ) - (cumsum(baseline$Dx)[251]   - baseline$Dx[1]    ),   digits = 0),
    round(  (cumsum(alt$Care)[251]         - alt$Care[1]        ) - (cumsum(baseline$Care)[251] - baseline$Care[1]  ),   digits = 0),
    round(  (cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]) - (cumsum(alt$PreLtfu)[251]   - alt$PreLtfu[1]    ),   digits = 0),
    round(  (cumsum(alt$Tx)[251]           - alt$Tx[1]          ) - (cumsum(baseline$Tx)[251]   - baseline$Tx[1]    ),   digits = 0),
    round(  (cumsum(alt$Vs)[251]           - alt$Vs[1]          ) - (cumsum(baseline$Vs)[251]   - baseline$Vs[1]    ),   digits = 0),
    round(  (cumsum(baseline$Ltfu)[251]    - baseline$Ltfu[1]   ) - (cumsum(alt$Ltfu)[251]      - alt$Ltfu[1]       ),   digits = 0)
)

optimDT <- data.frame(Intervention, Value)
optimDT$Value[3] <- -1e8
optimDT$Intervention <- factor(optimDT$Intervention, levels = Intervention)

ggplot(optimDT, aes(x = Intervention, y = Value, fill = Intervention)) +
geom_bar(stat = "identity") +
scale_y_continuous(labels = scales::comma) +
theme_classic() +
theme(plot.title = element_text(hjust = 0.5)) +
theme(title = element_text(size = 18)) +
theme(axis.title = element_blank()) +
theme(axis.text.x = element_text(size = 12)) +
theme(axis.text.y = element_text(size = 12)) +
theme(legend.position = "none") +
theme(plot.background = element_blank()) +
theme(panel.background = element_blank()) +
theme(axis.line.y = element_line()) +
theme(text = element_text(family = "Avenir Next"))
