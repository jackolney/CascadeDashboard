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
source("server/calibration/calibration.R",      local = FALSE)
source("server/calibration/assumptions.R",      local = FALSE)
source("server/calibration/calibration-data.R", local = FALSE)
source("server/calibration/marrakech-data.R",   local = FALSE)
source("server/calibration/misc-functions.R",   local = FALSE)
source("server/calibration/plot-functions.R",   local = FALSE)

# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/WhoCascade/cascade")
devtools::test(pkg = "~/git/WhoCascade/cascade")

# Run baseline model (nothing fancy)
KenyaData <- GetMasterDataSet("Kenya")
# RunBaselineModel(data = KenyaData)

#####################
# START OF FUNCTION #
#####################

# Need a SINGLE FUNCTION, that:
    # - Assembles model
    # - Takes changing parameter values (p)
    # - Runs model
    # - Analyses data & calculates ERROR

# Hadleys new visual profiler (awesome, cool).
# profvis::profvis({
#     SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
# })

# Create a parameter set
# Use: FME::Latinhyper = DONE.

# WHAT assumptions do we make about model parameters here?
# These are ALL based on the previous Kenya model - perhaps we should start with a more blank slate here?
# We want a simple calibration, that is also fast.

# BASELINE SIMULATION.
# BREAK IT DOWN. ONLY LOOK AT ONE PARAMETER AT A TIME.
# Start with RHO and then expand.
# HOW WILL I WEIGHT THIS?

# -------------------------------------------------------------------------------- #
# Allowing natural mortality to be adjusted, allows us to better fit to PLHIV data #
# -------------------------------------------------------------------------------- #

# Run Calibration Model
KenyaData <- GetMasterDataSet("Kenya")
RunCalibration(data = KenyaData, iterations = 100)

# 1000 simulations and picking the top 100 takes 48 seconds.

# The outcome of the calibration is two-fold:
    # 1) Return a range of values (min, max) of "ghost values". = DONE.
    # 2) Return a barplot of the "CASCADE"


############
# THURSDAY #
############

# 1) Get calibration to return barplot of the cascade. = DONE.
#     - Mean of the runs as the bar with shaded area around it? (just some error_bars)

RunCalibration(100)

# 2) Are we getting reasonable parameter values?
    # - They aren't totally nuts.

RunCalibration(1000)

# 3) Are we ready to share?
    # - Not quite.
    # - What about WEIGHTING???? (not just yet) YES YES YES! DONE.
        # - Just multiply each error by some value between 0 and 1, depending on additional info in iData.
        # - Color data points with 'traffic light' scheme (red/amber/green)
        # - Weighting the data will just be a case of adding a column to the .csv files and then including it in the AssembleComparisonDataFrame()
        # - Then choosing colours for each value.
    # - Add data points from 2015 estimates to barplot = DONE.
    # - Calibration to "PLHIV Suppressed" = DONE.
    # - Neaten up source code (will need to translate into shiny friendly code relatively soon) = DONE.

# 4) How easily can we do the 'whole thing' here?
    # - Need some UI tweaks, but they are coming!!

# 5) Expand to include "PLHIV Virally Suppressed" in cascade plot. = DONE.

# 6) What should be returned to shiny is a dataframe containing ALL parameters used for each run.
    # - Idea is that then we blast through them to produce each result with the 'projection' model.

# READY TO SHARE I THINK!!!

# LONG TERM GOAL (BEFORE FLORIDA HOLIDAY),
# Have the 'whole thing' done for KENYA.

# THERE AFTER, expand.
# end of this week, produce calibrated version of Kenya.

# --------- #
# QUESTIONS #
# --------- #
# (1) - How do we define the upper and lower bounds of parameters?
# (2) - How do we weight the data? (just divide it into 3 equal segments of 0.33?)
# (3) - Is the best 100 of 1k runs sufficient?


# ----------- #
# CALIBRATION #
# ----------- #

# LHS or optim(). Going with LHS for now. Can always pivot back.
# Plot test outputs.
# dynamic labelling of input data (what the hell do I mean here?)
# What is the best strategy? LHS and brute force.

# IDEA:
    # - Pull all 'error' values together.
    # - Multiply by some weight, depending on what they are
    # - Ensure that we are still able to minimise values to ZERO (well, I can dream)

# --------- #
# WEIGHTING #
# --------- #

# How do we weight studies?????

# PLOT FOR DATA REVIEW

data = KenyaData$calib

head(data)

ggOut <- ggplot(data, aes(x = year, y = value))
ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + scale_y_continuous(
    breaks = seq(0, max(data$value), 2e5),
    labels = scales::comma,
    expand = c(0, 0))
ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
ggOut <- ggOut + theme(axis.title = element_text(size = 12))
ggOut <- ggOut + theme(axis.line.x = element_line())
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(axis.title.y = element_blank())
ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + xlab("Year")
ggOut
