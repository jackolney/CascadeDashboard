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
        intParRange <- DefineParmRange()
        parRange <- UserOverRide(intParRange)
        lhs <- FME::Latinhyper(parRange, num = maxIterations)

        ## Sample Initial Compartment Values
        # Define max / min (also accounts for missing data)
        # Uses LHS to sample parameter space
        # Fishes out only sensical data
        # Deletes previous data.frame
        initRange <- DefineInitRange(data = data, min = 0.75, max = 1.25)
        lhsInitial <- FME::Latinhyper(initRange, num = maxIterations * 3)
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
        v <- 0
        selectedRuns <- c()
        minError <<- 1e6
        minErrorRun <<- NULL
        runError <<- c()
        CalibOut <<- c()
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
            iOut <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
            runError[k] <<- sum(iOut[iOut$source == "error", "value"])

            # If error <= maxError then store value of k
            if (runError[k] <= maxError & v < limit) {
                v <- v + 1
                if (runError[k] < minError) {
                    minError <<- runError[k]
                    minErrorRun <<- v
                }
                selectedRuns[v] <- k
                CalibOut <<- rbind(CalibOut, iOut)
                setProgress(value = v / limit, detail = paste0(round((v / limit) * 100, digits = 0), "%"))
                if (v == limit) break;
            }
            if (k == dim(lhsInitial_Sense)[1]) warning("Hit iteration wall.")
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

        # Plots (control passed back to shiny::renderPlot())
        setProgress(value = 1, detail = "Building figures")
    })
    ParamMaxMin
}
