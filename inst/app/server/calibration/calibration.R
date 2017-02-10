# Simple function calls for various permutations of the model
RunCalibration <- function(country, data, maxIterations, maxError, limit, targetIterations = 1e4) {
    # limit = 100
    # maxIterations = 1e4
    # maxError = 2

    # maxError entered as a string so must be converted
    maxError <- as.numeric(maxError)

    withProgress(message = 'Running Calibration:', value = 0, style = 'old', {

        # Set Global Variables
        time <- seq(0, 5, 1)
        p <- parameters(
            prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
            prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
            prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
            prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
            prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
            prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
            prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
            t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
            t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
            t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
            t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
            t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]]),
            # Advanced Calibration Tools (mortality)
            Alpha_1 = 0.004110 * AdvCalib$HIVMort,
            Alpha_2 = 0.011670 * AdvCalib$HIVMort,
            Alpha_3 = 0.009385 * AdvCalib$HIVMort,
            Alpha_4 = 0.016394 * AdvCalib$HIVMort,
            Alpha_5 = 0.027656 * AdvCalib$HIVMort,
            Alpha_6 = 0.047877 * AdvCalib$HIVMort,
            Alpha_7 = 1.081964 * AdvCalib$HIVMort,
            Tau_1 = 0.003905 * AdvCalib$HIVMort,
            Tau_2 = 0.011087 * AdvCalib$HIVMort,
            Tau_3 = 0.008916 * AdvCalib$HIVMort,
            Tau_4 = 0.015574 * AdvCalib$HIVMort,
            Tau_5 = 0.026273 * AdvCalib$HIVMort,
            Tau_6 = 0.045482 * AdvCalib$HIVMort,
            Tau_7 = 1.02785 * AdvCalib$HIVMort,
            Mu = AdvCalib$NatMort
        )

        # Not in care ART initiation rate CD4 adjustment
        message("Warning: theta only scaled for CD4 <200")
        p[["s_1"]] <- 1
        p[["s_2"]] <- 1
        p[["s_3"]] <- 1
        p[["s_4"]] <- 1
        p[["s_5"]] <- 2.824565
        p[["s_6"]] <- 2.824565
        p[["s_7"]] <- 2.824565

        ## Sample Parameters
        # Defines max / min
        # Allows user to override these
        # Uses LHS to sample parameter space
        setProgress(value = 0 / 1, detail = "Defining parameter space")
        intParRange <- DefineParmRange()
        parRange <- UserOverRide(intParRange)
        lhs <- FME::Latinhyper(parRange, num = targetIterations)

        ## Sample Initial Compartment Values
        # Define max / min (also accounts for missing data)
        # Uses LHS to sample parameter space
        # Fishes out only sensical data
        # Deletes previous data.frame

        # While loop setup, ensures we get 10k iterations to trial.
        # Can always be extended to KEEP GOING endlessly
        initRange <- DefineInitRange(data = data, min = 0.5, max = 1.5)
        its <- 0L
        lhsInitial_Out <- matrix()

        while (its < targetIterations) {
            lhsInitial <- FME::Latinhyper(initRange, num = maxIterations * 5)
            lhsInitial_Sense <- FindSense(samples = lhsInitial)
            if (is.na(lhsInitial_Out[[1]])) {
                lhsInitial_Out <- lhsInitial_Sense
            } else {
                lhsInitial_Out <- rbind(lhsInitial_Out, lhsInitial_Sense)
            }
            its <- dim(lhsInitial_Out)[1]
        }

        # Garbage collection
        rm(lhsInitial)
        rm(lhsInitial_Sense)

        ## Sample Incidence
        # Define max / min (from Spectrum Uncertainty Analysis)
        # Uses LHS to sample parameter space
        incRange <- DefineIncidenceRange(incidenceData = data$incidence)
        lhsIncidence <- FME::Latinhyper(incRange, num = targetIterations)

        ## For each draw, update parameter vector (p), run model, calculate error and store it.
        # Initial Calibration
        setProgress(value = 0 / 1, detail = "Running simulations")
        v <- 0
        selectedRuns <<- c()
        minError <<- 1e6
        minErrorRun <<- NULL
        runError <<- c()
        CalibOut <<- c()
        for (k in 1:dim(lhsInitial_Out)[1]) {

            p[["Rho"]]     <- lhs[,"rho"][k]
            p[["Epsilon"]] <- lhs[,"epsilon"][k]
            p[["Kappa"]]   <- lhs[,"kappa"][k]
            p[["Gamma"]]   <- lhs[,"gamma"][k]
            p[["Theta"]]   <- lhs[,"theta"][k]
            p[["Omega"]]   <- lhs[,"omega"][k]
            p[["p"]]       <- lhs[,"p"][k]
            p[["q"]]       <- lhs[,"q"][k]

            i <- incidence(as.double(lhsIncidence[k,]))
            y <- GetCalibInitial(p, data, init2010 = lhsInitial_Out[k,])
            iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))
            runError[k] <<- sum(iOut[iOut$source == "error", "value"])

            # If error <= maxError then store value of k
            if (runError[k] <= maxError & v < limit) {
                v <- v + 1
                if (runError[k] < minError) {
                    minError <<- runError[k]
                    minErrorRun <<- v
                }
                selectedRuns[v] <<- k
                CalibOut <<- rbind(CalibOut, iOut)
                setProgress(value = v / limit, detail = paste0(round((v / limit) * 100, digits = 0), "%"))
                if (v == limit) break;
            }
            if (k == dim(lhsInitial_Out)[1]) warning("Hit iteration wall.")
        }

        # Global Data Frames for Parameters / Initial Values
        CalibParamOut <<- FillParValues(samples = lhs,             positions = selectedRuns, limit = limit)
        CalibInitOut  <<- FillInitValues(samples = lhsInitial_Out, positions = selectedRuns, limit = limit)
        CalibIncOut   <<- FillIncValue(samples = lhsIncidence,     positions = selectedRuns, limit = limit)

        # Calculate min and max values used by parameter set (deprecated now?)
        ParamMaxMin <<- data.frame(
            min = apply(CalibParamOut, 2, min),
            max = apply(CalibParamOut, 2, max)
        )

        # Copy over to reactiveValues
        CalibParamMaxMin$rho_MAX     <- parRange["rho",     "max"]
        CalibParamMaxMin$rho_MIN     <- parRange["rho",     "min"]
        CalibParamMaxMin$epsilon_MAX <- parRange["epsilon", "max"]
        CalibParamMaxMin$epsilon_MIN <- parRange["epsilon", "min"]
        CalibParamMaxMin$q_MAX       <- parRange["q",       "max"]
        CalibParamMaxMin$q_MIN       <- parRange["q",       "min"]
        CalibParamMaxMin$gamma_MAX   <- parRange["gamma",   "max"]
        CalibParamMaxMin$gamma_MIN   <- parRange["gamma",   "min"]
        CalibParamMaxMin$theta_MAX   <- parRange["theta",   "max"]
        CalibParamMaxMin$theta_MIN   <- parRange["theta",   "min"]
        CalibParamMaxMin$kappa_MAX   <- parRange["kappa",   "max"]
        CalibParamMaxMin$kappa_MIN   <- parRange["kappa",   "min"]
        CalibParamMaxMin$omega_MAX   <- parRange["omega",   "max"]
        CalibParamMaxMin$omega_MIN   <- parRange["omega",   "min"]
        CalibParamMaxMin$p_MAX       <- parRange["p",       "max"]
        CalibParamMaxMin$p_MIN       <- parRange["p",       "min"]

        # Info for optimisation intervention setup
        OptInput$intValue_rho   <- CalibParamMaxMin$rho_MAX
        OptInput$intValue_q     <- CalibParamMaxMin$q_MAX
        OptInput$intValue_kappa <- CalibParamMaxMin$kappa_MIN
        OptInput$intValue_gamma <- CalibParamMaxMin$gamma_MAX
        OptInput$intValue_sigma <- 0.1
        OptInput$intValue_omega <- CalibParamMaxMin$omega_MIN

        # Plots (control passed back to shiny::renderPlot())
        setProgress(value = 1, detail = "Building figures")
    })
    ParamMaxMin
}
