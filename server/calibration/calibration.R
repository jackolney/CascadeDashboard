# Simple function calls for various permutations of the model

RunBaselineModel <- function(data) {

    # Set important parameters
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
        t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
        )
    y <- GetCalibInitial(p, data)
    i <- incidence(as.double(data[["incidence"]]))

    # Run C++ model
    result <- CallCalibModel(time, y, p, i)

    # Assemble output data.frame
    df <- AssembleComparisonDataFrame(country = "Kenya", model = result, data = data)

    # Calculate mean squared error between model and data
    original <- SSE(df)

    # Build Plots
    # BuildBaselinePlots(original)
    BuildBaselineErrorPlots(original)
}

RunCalibration <- function(data, maxIterations, maxError, limit) {
    # limit = 100
    # maxIterations = 1e4
    # maxError = 2

    # maxError entered as a string so must be converted
    maxError <- as.numeric(maxError)

    withProgress(message = 'Running Calibration:', value = 0, {

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
            t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
            )

        ## Sample Parameters
        # Defines max / min
        # Allows user to override these
        # Uses LHS to sample parameter space
        setProgress(value = 0 / 1, detail = "Defining parameter space")
        intParRange <- DefineParmRange(param = p, min = 0.01, max = 5)
        parRange <- UserOverRide(intParRange)
        lhs <- FME::Latinhyper(parRange, num = maxIterations)

        ## Sample Initial Compartment Values
        # Define max / min (also accounts for missing data)
        # Uses LHS to sample parameter space
        # Fishes out only sensical data
        # Deletes previous data.frame
        initRange <- DefineInitRange(data = data, min = 0.9, max = 1.1)
        lhsInitial <- FME::Latinhyper(initRange, num = maxIterations)
        lhsInitial_Sense <- FindSense(samples = lhsInitial)
        rm(lhsInitial)

        ## Sample Incidence
        # Define max / min (from Spectrum Uncertainty Analysis)
        # Uses LHS to sample parameter space
        incRange <- DefineIncidenceRange(incidenceData = data$incidence)
        lhsIncidence <- FME::Latinhyper(incRange, num = maxIterations)

        ## For each draw, update parameter vector (p), run model, calculate error and store it.
        # Initial Calibration
        setProgress(value = 0 / 1, detail = "Running simulations")
        v = 0
        selectedRuns <- c()
        runError <<- c()
        for (k in 1:dim(lhsInitial_Sense)[1]) {

            p[["Rho"]]     <- lhs[,"rho"][k]
            p[["Epsilon"]] <- lhs[,"epsilon"][k]
            p[["Kappa"]]   <- lhs[,"kappa"][k]
            p[["Gamma"]]   <- lhs[,"gamma"][k]
            p[["Theta"]]   <- lhs[,"theta"][k]
            p[["Omega"]]   <- lhs[,"omega"][k]
            p[["p"]]       <- lhs[,"p"][k]
            p[["q"]]       <- lhs[,"q"][k]

            i <- incidence(as.double(lhsIncidence[k,]))
            y <- GetCalibInitial(p, data, init2010 = lhsInitial_Sense[k,])
            out <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
            runError[k] <<- sum(out[out$source == "error", "value"])

            # If error <= maxError then store value of k
            if (runError[k] <= maxError) {
                v <- v + 1
                selectedRuns[v] <- k
                setProgress(value = v / limit, detail = paste0(round((v / limit) * 100, digits = 0), "%"))
                if (v == limit) break;
            }
        }

        ## For the runs that had error less than maxError, re-run simulations and store results
        # Faster than storing ALL results in the first place (I think)
        setProgress(value = 0 / 1, detail = "Sampling best runs")
        CalibOut <<- c()
        for (l in 1:limit) {

            p[["Rho"]]     <- lhs[,"rho"][selectedRuns[l]]
            p[["Epsilon"]] <- lhs[,"epsilon"][selectedRuns[l]]
            p[["Kappa"]]   <- lhs[,"kappa"][selectedRuns[l]]
            p[["Gamma"]]   <- lhs[,"gamma"][selectedRuns[l]]
            p[["Theta"]]   <- lhs[,"theta"][selectedRuns[l]]
            p[["Omega"]]   <- lhs[,"omega"][selectedRuns[l]]
            p[["p"]]       <- lhs[,"p"][selectedRuns[l]]
            p[["q"]]       <- lhs[,"q"][selectedRuns[l]]

            i <- incidence(as.double(lhsIncidence[selectedRuns[l],]))
            y <- GetCalibInitial(p, data, init2010 = lhsInitial_Sense[selectedRuns[l],])
            iOut <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
            CalibOut <<- rbind(CalibOut, iOut)
            setProgress(value = l / limit, detail = paste0("Resample ", round((l / limit) * 100, digits = 0), "%"))
        }

        # Global Data Frames for Parameters / Initial Values
        CalibParamOut <<- FillParValues(samples = lhs,               positions = selectedRuns, limit = limit)
        CalibInitOut  <<- FillInitValues(samples = lhsInitial_Sense, positions = selectedRuns, limit = limit)
        CalibIncOut   <<- FillIncValue(samples = lhsIncidence,       positions = selectedRuns, limit = limit)

        # Calculate min and max values used by parameter set (deprecated now?)
        ParamMaxMin <<- data.frame(
            min = apply(CalibParamOut, 2, min),
            max = apply(CalibParamOut, 2, max)
        )

        # Copy over to reactiveValues
        CalibParamMaxMin$rho_MAX     <- ParamMaxMin["rho",   "max"]
        CalibParamMaxMin$rho_MIN     <- ParamMaxMin["rho",   "min"]
        CalibParamMaxMin$q_MAX       <- ParamMaxMin["q",     "max"]
        CalibParamMaxMin$q_MIN       <- ParamMaxMin["q",     "min"]
        CalibParamMaxMin$gamma_MAX   <- ParamMaxMin["gamma", "max"]
        CalibParamMaxMin$gamma_MIN   <- ParamMaxMin["gamma", "min"]
        CalibParamMaxMin$theta_MAX   <- ParamMaxMin["theta", "max"]
        CalibParamMaxMin$theta_MIN   <- ParamMaxMin["theta", "min"]
        CalibParamMaxMin$kappa_MAX   <- ParamMaxMin["kappa", "max"]
        CalibParamMaxMin$kappa_MIN   <- ParamMaxMin["kappa", "min"]
        CalibParamMaxMin$omega_MAX   <- ParamMaxMin["omega", "max"]
        CalibParamMaxMin$omega_MIN   <- ParamMaxMin["omega", "min"]
        CalibParamMaxMin$p_MAX       <- ParamMaxMin["p",     "max"]
        CalibParamMaxMin$p_MIN       <- ParamMaxMin["p",     "min"]

        # Plots
        setProgress(value = 1, detail = "Building figures")
        BuildCalibrationPlots(data = CalibOut, originalData = data)
        BuildCalibrationHistogram(runError = runError, maxError = maxError)
    })
    ParamMaxMin
}
