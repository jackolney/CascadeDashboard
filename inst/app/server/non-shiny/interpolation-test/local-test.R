##### Local Intervention Interpolation Test #####

message("Starting local intervention interpolation test...")

setwd("~/git/WhoCascade/CascadeDashboard")

source("server/calibration/master.R",                         local = FALSE)
source("server/calibration/initial.R",                        local = FALSE)
source("server/calibration/model.R",                          local = FALSE)
source("server/calibration/error.R",                          local = FALSE)
source("server/calibration/calibration.R",                    local = FALSE)
source("server/calibration/assumptions.R",                    local = FALSE)
source("server/calibration/calibration-data.R",               local = FALSE)
source("server/calibration/marrakech-data.R",                 local = FALSE)
source("server/calibration/misc-functions.R",                 local = FALSE)
source("server/misc-functions.R",                             local = FALSE)
source("server/calibration/plot-functions.R",                 local = FALSE)
source("server/non-shiny/non-shiny-calibration.R",            local = FALSE)
source("server/country/misc-functions.R",                     local = FALSE)
source("server/model/baseline-model.R",                       local = FALSE)
source("server/model/best-fit-model.R",                       local = FALSE)
source("server/model/beta.R",                                 local = FALSE)
source("server/model/initial.R",                              local = FALSE)
source("server/model/parameters.R",                           local = FALSE)
source("server/model/sim-abs.R",                              local = FALSE)
source("server/model/sim-prop.R",                             local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",           local = FALSE)
source("server/optimisation/frontier.R",                      local = FALSE)
source("server/optimisation/input-functions.R",               local = FALSE)
source("server/optimisation/output-functions.R",              local = FALSE)
source("server/optimisation/parameters.R",                    local = FALSE)
source("server/optimisation/plot-functions.R",                local = FALSE)
source("server/non-shiny/interpolation-test/test-function.R", local = FALSE)

# load 'cascade' package and ensure it is the latest build.
require(cascade)

KenyaData <- GetMasterDataSet("Kenya")

RunNSCalibration(country = "Kenya", data = KenyaData, maxIterations = 1e4, maxError = 3, limit = 1000)

MasterData <- GetMasterDataSet("Kenya")

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

Two <- CheckInterpolation(propRuns = 0.1, intLength = 2)
Three <- CheckInterpolation(propRuns = 0.1, intLength = 3)
Four <- CheckInterpolation(propRuns = 0.1, intLength = 4)

save.image(file = "currentSession.RData")

# test <- reshape2::melt(cbind(Two, Three))
# test2 <- test[test$Var1 != "Cost",]

# ggplot(test2, aes(x = Var1, y = value, group = Var2)) + geom_point(aes(col = Var2))

# # Make plot then deploy on the cluster for simulation.
# graphics.off()
# quartz.options(w = 8, h = 5)

# test2$value <- abs(test2$value)

# levels(test2$Var1) <- c("Cost", "Testing", "Linkage", "Pre-ART\nRetention", "ART\nInitiation", "Adherence", "ART\nRetention")

# ggplot(test2, aes(x = Var1, y = value, group = Var2)) +
# geom_point(aes(col = Var2), size = 3, alpha = 0.5) +
# guides(col = guide_legend(title = "Intervention Levels")) +
# theme_classic() +
# theme(axis.line.y = element_line()) +
# theme(axis.line.x = element_line()) +
# theme(axis.title.x = element_blank()) +
# theme(axis.title.y = element_blank()) +
# theme(legend.title = element_text(size = 10)) +
# theme(legend.text = element_text(size = 8)) +
# ggtitle(label = "Intervention Interpolation Test", subtitle = "Increasing number of intervention levels against interpolation result") +
# scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))

