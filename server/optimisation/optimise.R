RunOptimisation <- function(propRuns = 0.1) {
    # This should be triggered by the renderPlot().
    withProgress(min = 0, max = 1, {

        # Simulation Loop
        time <- proc.time()[[1]]

        setProgress(value = 0, message = 'Starting optimisation', detail = 'creating parameter matrix')

        # Extract the initial values of the 10% of 'best' fit simulation from calibration
        bestTenPercentCalibInitial <<- GetBestTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = propRuns)

        # Identify the order of simulations ranked by error (low to high)
        orderedRuns <- order(runError[selectedRuns])

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

        updateButton(session,
            inputId = "optimStart",
            label = "",
            style = "primary",
            block = TRUE,
            size = "large",
            icon = icon("refresh", class = "fa-lg fa-spin", lib = "font-awesome"))

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

                setProgress(
                    value = iC / ((dim(bestTenPercentCalibInitial)[1] / 7) * dim(parSteps)[1]),
                    message = paste('Simulation', j),
                    detail = paste('Run', i))

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
                rFirst90[iC]  <- Calc_909090_Result(  SimResult )[1]
                rSecond90[iC] <- Calc_909090_Result(  SimResult )[2]
                rThird90[iC]  <- Calc_909090_Result(  SimResult )[3]
                rVS[iC]       <- Calc_VS(             SimResult )
                rImpact[iC]   <- Calc_DALYsAverted(   SimResult , BaseDALY)
                rCost[iC]     <- Calc_AdditionalCost( SimResult , BaseCost)

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
        }

        optResults <<- data.frame(rFirst90, rSecond90, rThird90, rVS, rCost, rRho, rQ, rKappa, rGamma, rSigma, rOmega, rTest, rLink, rPreR, rInit, rAdhr, rRetn)
        colnames(optResults) <<- c("First 90", "Second 90", "Third 90", "VS", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega", "Testing", "Linkage", "Pre-ART Retention", "Initiation", "Adherence", "ART Retention")

        setProgress(value = 1, message = "Finished", detail = paste(round(proc.time()[[1]] - time, 0), "seconds"))

        updateButton(session,
            inputId = "optimStart",
            label = "Start",
            style = "success",
            block = TRUE,
            size = "large",
            icon = icon("play", class = "fa-lg fa-fw", lib = "font-awesome"))
    })
    optResults
}
