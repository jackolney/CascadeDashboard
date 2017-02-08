## Blank variables that replace deprecated globals
# Need to know the type of each to initialise variables

# Start by eliminating 'MasterData' from .GlobalEnv then do the rest
# remember that I do a lot of rm(MasterData) crap so I need to re-write this too.
# Just reset it to MasterData <- list()

## Muy Importante
MasterData <- list()

## Calibration
CalibIncOut       <- data.frame()
CalibInitOut      <- data.frame()
CalibOut          <- c()
CalibParamOut     <- data.frame()
countryReportName <- character()
minError          <- numeric()
minErrorRun       <- c()
ParamMaxMin       <- data.frame()
runError          <- c()
selectedRuns      <- c()

## Optimisation
BaselineAdhr               <- numeric()
BaselineCost               <- numeric()
BaselineInit               <- numeric()
BaselineLink               <- numeric()
BaselinePreR               <- numeric()
BaselineRetn               <- numeric()
BaselineTest               <- numeric()
bestTenPercentCalibInitial <- data.frame()
optResults                 <- data.frame()
shuffledRuns               <- integer()

## Report
report_909090_adherence         <- character()
report_909090_cost              <- character()
report_909090_initiation        <- character()
report_909090_linkage           <- character()
report_909090_preRetention      <- character()
report_909090_retention         <- character()
report_909090_testing           <- character()
report_909090_adherence_BASE    <- character()
report_909090_cost_BASE         <- character()
report_909090_initiation_BASE   <- character()
report_909090_linkage_BASE      <- character()
report_909090_preRetention_BASE <- character()
report_909090_retention_BASE    <- character()
report_909090_testing_BASE      <- character()
