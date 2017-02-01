#' Cluster optimisation function.
#'
#' This function controls the entire optimisation procedure in CascadeDashboard, and is a port of
#'  the version used in the R-package
#'
#' @param AdvCalib advanced mortality adjustments set during calibration
#'
#' @param CalibOut results from calibration
#'
#' @param CalibParamOut parameter values from calibration
#'
#' @param intLength integer defining number of individual intervention permutations to simulate
#'
#' @param intSwitch intervention boolean switches
#'
#' @param OptInput optimisation input data.frame
#'
#' @param propRuns proportion of total simulations to sample during optimisation
#'
#' @param reactiveAdjustCost cost-adjustment switch (scaling HIV-testing unit cost)
#'
#' @param reactiveCost data.frame of unit costs
#'
#' @param runError errors resulting from calibration
#'
#' @param SafeReactiveCost default unit cost values
#'
#' @param selectedRuns sampled runs from calibration that will be optimised
#'
#'
#' @export
RunClusterOptimisation <- function(AdvCalib, CalibOut, CalibParamOut, intLength, intSwitch,
    OptInput, propRuns, reactiveAdjustCost, reactiveCost, runError, SafeReactiveCost, selectedRuns)
    {

    # This needs to source a package specific file.
    source(system.file("app/clustr-initial.R", package = "CascadeDashboard"), local = FALSE)

    # CHECKLIST
    message(paste("OptInput$intValue_rho =", OptInput$intValue_rho))
    message(paste("OptInput$intValue_q =", OptInput$intValue_q))
    message(paste("OptInput$intValue_kappa =", OptInput$intValue_kappa))
    message(paste("OptInput$intValue_gamma =", OptInput$intValue_gamma))
    message(paste("OptInput$intValue_sigma =", OptInput$intValue_sigma))
    message(paste("OptInput$intValue_omega =", OptInput$intValue_omega))

    message("\nStarting optimisation...\n")

    # Simulation Loop
    time <- proc.time()[[1]]

    # Extract the initial values of a random 10% of simulations from calibration
    bestTenPercentCalibInitial <- GetRandomTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = propRuns)

    # Account for testing HIV-negatives
    AdjustHIVTestCost(reactiveAdjustCost, reactiveCost, SafeReactiveCost)

    # index counter
    iC <- 1L

    # output vectors
    rFirst90  <- c()
    rSecond90 <- c()
    rThird90  <- c()
    rVS       <- c()
    rImpact   <- c()
    rCost     <- c()
    rRho      <- c()
    rQ        <- c()
    rKappa    <- c()
    rGamma    <- c()
    rSigma    <- c()
    rOmega    <- c()
    rTest     <- c()
    rLink     <- c()
    rPreR     <- c()
    rInit     <- c()
    rAdhr     <- c()
    rRetn     <- c()

    # additional cost vectors
    rCostOrg  <- c()
    rCostTot  <- c()

    # baseline trackers
    bTest     <- c()
    bLink     <- c()
    bPreR     <- c()
    bInit     <- c()
    bAdhr     <- c()
    bRetn     <- c()

    # because seven indicators
    for (j in 1:(dim(bestTenPercentCalibInitial)[1] / 7)) {

        message(paste('Simulation', j))

        # Run Baseline simulation
        BaseModel <- CallBaselineModel(runNumber = shuffledRuns[j], initVals = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),])
        BaseDALY  <- Calc_DALY(BaseModel)
        BaseCost  <- Calc_Cost(BaseModel)
        rCostOrg[j] <- BaseCost
        message(paste("\t", scales::comma(BaseDALY), "DALYs, at", scales::dollar(BaseCost)))

        # Need some functions to calculate the BASELINE changes to care.
        bTest[j] <- BaseModel$CumDiag[251]
        bLink[j] <- BaseModel$CumLink[251]
        bPreR[j] <- BaseModel$CumPreL[251]
        bInit[j] <- BaseModel$CumInit[251]
        bAdhr[j] <- BaseModel$Vs[251] - BaseModel$Vs[1]
        bRetn[j] <- BaseModel$CumLoss[251]

        parSteps <- GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = shuffledRuns[j], length = intLength)

        for (i in 1:dim(parSteps)[1]) {
            cat("=")

            # This need modifying
            p <- GetOptRunPar(
                masterCD4 = MasterData$cd4_2015,
                data = MasterData,
                iterationParam = parSteps[i,],
                calibParamOut = CalibParamOut,
                runNumber = shuffledRuns[j])

            # Now we need the initials.
            y <- GetInitial(
                p = p,
                iterationResult = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),],
                masterCD4 = MasterData$cd4_2015)

            p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[shuffledRuns[j],])

            SimResult <- RunSim_Prop(y = y, p = p)

            # These guys keep going
            rFirst90[iC]  <- Calc_909090_Result(  SimResult )[1]
            rSecond90[iC] <- Calc_909090_Result(  SimResult )[2]
            rThird90[iC]  <- Calc_909090_Result(  SimResult )[3]
            rVS[iC]       <- Calc_VS(             SimResult )
            rImpact[iC]   <- Calc_DALYsAverted(   SimResult , BaseDALY)
            rCost[iC]     <- Calc_AdditionalCost( SimResult , BaseCost)
            rCostTot[iC]  <- Calc_Cost(SimResult)

            # Care Calculations
            rTest[iC]     <- Calc_CareTesting(baseResult      = BaseModel, simResult = SimResult)
            rLink[iC]     <- Calc_CareLinkage(baseResult      = BaseModel, simResult = SimResult)
            rPreR[iC]     <- Calc_CarePreRetention(baseResult = BaseModel, simResult = SimResult)
            rInit[iC]     <- Calc_CareInitiation(baseResult   = BaseModel, simResult = SimResult)
            rAdhr[iC]     <- Calc_CareAdherence(baseResult    = BaseModel, simResult = SimResult)
            rRetn[iC]     <- Calc_CareRetention(baseResult    = BaseModel, simResult = SimResult)

            # These should always just reference i in all cases (as they repeat)
            rRho[iC]    <- parSteps[i,"Rho"]
            rQ[iC]      <- parSteps[i,"Q"]
            rKappa[iC]  <- parSteps[i,"Kappa"]
            rGamma[iC]  <- parSteps[i,"Gamma"]
            rSigma[iC]  <- parSteps[i,"Sigma"]
            rOmega[iC]  <- parSteps[i,"Omega"]

            iC <- iC + 1L
        }
        cat("\n")
    }

    optResults <- data.frame(rFirst90, rSecond90, rThird90, rVS, rCost, rRho, rQ, rKappa, rGamma, rSigma, rOmega, rTest, rLink, rPreR, rInit, rAdhr, rRetn, rCostTot)
    colnames(optResults) <- c("First 90", "Second 90", "Third 90", "VS", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega", "Testing", "Linkage", "Pre-ART Retention", "Initiation", "Adherence", "ART Retention", "Total Cost")

    # Make all the baseline stuff global
    BaselineCost <- rCostOrg
    BaselineTest <- bTest
    BaselineLink <- bLink
    BaselinePreR <- bPreR
    BaselineInit <- bInit
    BaselineAdhr <- bAdhr
    BaselineRetn <- bRetn

    message("\nFinished")

    # output a list
    out <- list(
        bestTenPercentCalibInitial = bestTenPercentCalibInitial,
        optResults = optResults,
        BaselineCost = BaselineCost,
        BaselineTest = BaselineTest,
        BaselineLink = BaselineLink,
        BaselinePreR = BaselinePreR,
        BaselineInit = BaselineInit,
        BaselineAdhr = BaselineAdhr,
        BaselineRetn = BaselineRetn
    )
    out
}
