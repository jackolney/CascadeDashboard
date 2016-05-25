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

dim(theOut)

head(theOut)

ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Omega))

# How do we best quantify this output then?

pryr::object_size(theOut)

