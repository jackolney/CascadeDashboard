## Blank variables that replace deprecated globals
# Need to know the type of each to initialise variables

# Start by eliminating 'MasterData' from .GlobalEnv then do the rest
# remember that I do a lot of rm(MasterData) crap so I need to re-write this too.
# Just reset it to MasterData <- list()

## Muy Importante
MasterData <- list()

## Calibration
countryReportName <- character()
CalibIncOut <- data.frame()
CalibInitOut <- data.frame()
CalibParamOut <- data.frame()

# CalibOut
# minError
# minErrorRun
# ParamMaxMin
# runError
# selectedRuns

## Optimisation
# baseTenPercentCalibInitial
# shuffledRuns
# optResults
# BaselineAdhr
# BaselineCost
# BaselineInit
# BaselineLink
# BaselinePreR
# BaselineRetn
# BaselineTest

## Report
# report_909090_adherence
# report_909090_adherence_BASE
# report_909090_cost
# report_909090_cost_BASE
# report_909090_initiation
# report_909090_initiation_BASE
# report_909090_linkage
# report_909090_linkage_BASE
# report_909090_preRetention
# report_909090_preRetention_BASE
# report_909090_retention
# report_909090_retention_BASE
# report_909090_testing
# report_909090_testing_BASE
