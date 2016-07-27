RunNSOptimisation <- function() {
    # This should be triggered by the renderPlot().

    # CHECKLIST
    print("OptInput$intValue_rho =")
    print(OptInput$intValue_rho)

    print("OptInput$intValue_q =")
    print(OptInput$intValue_q)

    print("OptInput$intValue_kappa =")
    print(OptInput$intValue_kappa)

    print("OptInput$intValue_gamma =")
    print(OptInput$intValue_gamma)

    print("OptInput$intValue_sigma =")
    print(OptInput$intValue_sigma)

    print("OptInput$intValue_omega =")
    print(OptInput$intValue_omega)

    message("Starting optimisation...")

    par <- GetParaMatrix(cParamOut = CalibParamOut, minErrorRun = minErrorRun, length = 2)

    # Simulation Loop
    time <- proc.time()[[1]]

    jList <- list()

    # Extract the initial values of the 10% of 'best' fit simulation from calibration
    bestTenPercentCalibInitial <<- GetBestTenPercentCalibOut(CalibOut = CalibOut, runError = runError, selectedRuns = selectedRuns, propRuns = propRuns)

    # Identify the order of simulations ranked by error (low to high)
    orderedRuns <- order(runError[selectedRuns])

    # because seven indicators
    for (j in 1:(dim(bestTenPercentCalibInitial)[1] / 7)) {

        message(paste('Simulation', j))

        iList <- list()

        ## THE BIG LOOP ##
        for (i in 1:dim(par)[1]) {

            cat("#")

            # This need modifying
            p <- GetOptRunPar(
                masterCD4 = MasterCD4_2015,
                data = MasterData,
                iterationParam = par[i,],
                calibParamOut = CalibParamOut,
                runNumber = orderedRuns[j])

            # Now we need the initials.
            y <- GetInitial(
                p = p,
                iterationResult = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),],
                masterCD4 = MasterCD4_2015)

            p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[orderedRuns[j],])

            iList[[rownames(par)[i]]] <- RunSim(y = y, p = p)
        }
        ## END OF i LOOP ##
        cat("\n")
        # Write iList to jList
        jList[[j]] <- iList

    }
    pryr::object_size(jList)

    message('Compiling results')

    # Dealing with the results #
    # I think we need to pass the average values to BaselineModel() not let it pick from the other crap.
    # Although, may need to reverse on this strategy tomorrow.
    # Need a sensible answer first.
    # BaseModel <- BaselineModel()

    # here
    # how do we want outputs presented.
    # basically just ONE BIG ASS data.frame

    # initial vals
    Result90        <- c()
    Result9090      <- c()
    Result909090    <- c()
    ResultVS        <- c()
    ResultImpact    <- c()
    ResultCost      <- c()
    ResultPar_Rho   <- c()
    ResultPar_Q     <- c()
    ResultPar_Kappa <- c()
    ResultPar_Gamma <- c()
    ResultPar_Sigma <- c()
    ResultPar_Omega <- c()

    # j loop
    for (j in 1:(dim(bestTenPercentCalibInitial)[1] / 7)) {
        message(paste('Compiling sim', j))

        # baseline calls
        BaseModel <- CallBaselineModel(runNumber = orderedRuns[j], initVals = bestTenPercentCalibInitial[1:7 + 7 * (j - 1),])
        BaseDALY  <- Calc_DALY(BaseModel)
        BaseCost  <- Calc_Cost(BaseModel)
        message(paste("\t", scales::comma(BaseDALY), "DALYs, at", scales::dollar(BaseCost)))

        # i loop
        for (i in 1:dim(par)[1]) {
            cat("#")

            Result90[i]        <- Calc_909090_Result(iList[[i]])[1]

        }
    }

i = 1
j = 1

64


iTotal = 64
jTotal = 10

# HERE ON WEDNESDAY EVENING!

# Need to merge iList and jList into a single vector for results.

save.image(file = "~/Desktop/saveSpace.Rdata")




    for (i in 1:length(iList)) {

        Result90[i]        <- Calc_909090_Result(iList[[i]])[1]
        Result9090[i]      <- Calc_909090_Result(iList[[i]])[2]
        Result909090[i]    <- Calc_909090_Result(iList[[i]])[3]
        ResultVS[i]        <- Calc_VS(iList[[i]])
        ResultImpact[i]    <- Calc_DALYsAverted(iList[[i]], BaseDALY)
        ResultCost[i]      <- Calc_AdditionalCost(iList[[i]], BaseCost)
        ResultPar_Rho[i]   <- par[i,"Rho"]
        ResultPar_Q[i]     <- par[i,"Q"]
        ResultPar_Kappa[i] <- par[i,"Kappa"]
        ResultPar_Gamma[i] <- par[i,"Gamma"]
        ResultPar_Sigma[i] <- par[i,"Sigma"]
        ResultPar_Omega[i] <- par[i,"Omega"]
    }

    # Result data.frame for plot(vs,cost)
    Result_909090 <<- data.frame(Result90, Result9090, Result909090, ResultVS, ResultCost, ResultPar_Rho, ResultPar_Q, ResultPar_Kappa, ResultPar_Gamma, ResultPar_Sigma, ResultPar_Omega)
    colnames(Result_909090) <<- c("90", "90-90", "90-90-90", "VS", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")

    # Result data.frame for plot(DALYs,cost)
    Result_DALYs <<- data.frame(ResultImpact, ResultCost, ResultPar_Rho, ResultPar_Q, ResultPar_Kappa, ResultPar_Gamma, ResultPar_Sigma, ResultPar_Omega)
    colnames(Result_DALYs) <<- c("DALYs", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")

    # ----------------------------------------- #
    # Subsetting those achieving 90-90-90 Stuff #
    # ----------------------------------------- #

    theList_909090 <- FindResults_909090(theList)

    ResultPar_909090 <- FindPar_909090(theList, par)

    Result909090Impact    <- c()
    Result909090Cost      <- c()
    Result909090Par_Rho   <- c()
    Result909090Par_Q     <- c()
    Result909090Par_Kappa <- c()
    Result909090Par_Gamma <- c()
    Result909090Par_Sigma <- c()
    Result909090Par_Omega <- c()
    if (length(theList_909090) > 0) {
        for (i in 1:length(theList_909090)) {
            message(paste0(round((i / length(theList_909090) * 100), digits = 0), "%"))
            Result909090Impact[i]    <- Calc_DALYsAverted(theList_909090[[i]], BaseDALY)
            Result909090Cost[i]      <- Calc_AdditionalCost(theList_909090[[i]], BaseCost)
            Result909090Par_Rho[i]   <- par[i,"rho"]
            Result909090Par_Q[i]     <- par[i,"q"]
            Result909090Par_Kappa[i] <- par[i,"kappa"]
            Result909090Par_Gamma[i] <- par[i,"gamma"]
            Result909090Par_Sigma[i] <- par[i,"sigma"]
            Result909090Par_Omega[i] <- par[i,"omega"]
        }

        Result_DALYs_909090 <<- data.frame(Result909090Impact, Result909090Cost, Result909090Par_Rho, Result909090Par_Q, Result909090Par_Kappa, Result909090Par_Gamma, Result909090Par_Sigma, Result909090Par_Omega)
        colnames(Result_DALYs_909090) <<- c("DALYs", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")
    } else {
        Result_DALYs_909090 <<- data.frame(0, 0, 0, 0, 0, 0, 0, 0)
        colnames(Result_DALYs_909090) <<- c("DALYs", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")
    }

    message("finished")
    Result_909090
}
