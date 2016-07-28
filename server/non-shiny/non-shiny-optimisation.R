RunNSOptimisation <- function(propRuns) {
    # This should be triggered by the renderPlot().

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

    # Extract the initial values of the 10% of 'best' fit simulation from calibration
    bestTenPercentCalibInitial <<- GetBestTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = propRuns)

    # Identify the order of simulations ranked by error (low to high)
    orderedRuns <- order(runError[selectedRuns])

    # index counter
    iC <- 1L

    # output vectors
    r90    <- c()
    r9090  <- c()
    r909090<- c()
    rVS    <- c()
    rImpact<- c()
    rCost  <- c()
    rRho   <- c()
    rQ     <- c()
    rKappa <- c()
    rGamma <- c()
    rSigma <- c()
    rOmega <- c()

    # because seven indicators
    for (j in 1:(dim(bestTenPercentCalibInitial)[1] / 7)) {

        message(paste('Simulation', j))

        # Run Baseline simulation
        BaseModel <- CallBaselineModel(runNumber = orderedRuns[j], initVals = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),])
        BaseDALY  <- Calc_DALY(BaseModel)
        BaseCost  <- Calc_Cost(BaseModel)
        message(paste("\t", scales::comma(BaseDALY), "DALYs, at", scales::dollar(BaseCost)))

        parSteps <- GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = orderedRuns[j], length = 2)

        for (i in 1:dim(parSteps)[1]) {
            cat("#")

            # This need modifying
            p <- GetOptRunPar(
                masterCD4 = MasterCD4_2015,
                data = MasterData,
                iterationParam = parSteps[i,],
                calibParamOut = CalibParamOut,
                runNumber = orderedRuns[j])

            # Now we need the initials.
            y <- GetInitial(
                p = p,
                iterationResult = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),],
                masterCD4 = MasterCD4_2015)

            p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[orderedRuns[j],])

            SimResult <- RunSim(y = y, p = p)

            # These guys keep going
            r90[iC]     <- Calc_909090_Result(  SimResult )[1]
            r9090[iC]   <- Calc_909090_Result(  SimResult )[2]
            r909090[iC] <- Calc_909090_Result(  SimResult )[3]
            rVS[iC]     <- Calc_VS(             SimResult )
            rImpact[iC] <- Calc_DALYsAverted(   SimResult , BaseDALY)
            rCost[iC]   <- Calc_AdditionalCost( SimResult , BaseCost)

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

    optResults <<- data.frame(r90, r9090, r909090, rVS, rCost, rRho, rQ, rKappa, rGamma, rSigma, rOmega)
    colnames(optResults) <<- c("90", "90-90", "90-90-90", "VS", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")

    message("\nFinished")
    optResults
}
