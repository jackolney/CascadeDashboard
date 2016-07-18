# Update MasterData upon hitting 'NEXT'
# Update bug happens if no value is already there to REPLACE.
# Need to run a check... that will rbind() if nothing there.

observeEvent(input$PREV_plhiv, {
    if (input$uPLHIV != 0 & !is.na(input$uPLHIV) & input$uPLHIV_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV"
            year <- input$uPLHIV_year
            value <- input$uPLHIV
            weight <- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uPLHIV =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <<- input$uPLHIV
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            message(paste("MasterData updated with uPLHIV =", input$uPLHIV, "in", input$uPLHIV_year, "with weight", input$uPLHIV_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        }
    }
})

observeEvent(input$NEXT_plhiv, {
    if (input$uPLHIV != 0 & !is.na(input$uPLHIV) & input$uPLHIV_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV"
            year <- input$uPLHIV_year
            value <- input$uPLHIV
            weight <- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uPLHIV =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <<- input$uPLHIV
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            message(paste("MasterData updated with uPLHIV =", input$uPLHIV, "in", input$uPLHIV_year, "with weight", input$uPLHIV_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        }
    }
})

observeEvent(input$NEXT_diag, {
    if (input$uDIAG != 0 & !is.na(input$uDIAG) & input$uDIAG_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV Diagnosed"
            year <- input$uDIAG_year
            value <- input$uDIAG
            weight <- as.vector(SourceList$weight[which(SourceList == input$uDIAG_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uDIAG =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"] <<- input$uDIAG
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uDIAG_source)])
            message(paste("MasterData updated with uDIAG =", input$uDIAG, "in", input$uDIAG_year, "with weight", input$uDIAG_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed",])
        }
    }
})

observeEvent(input$NEXT_care, {
    if (input$uCARE != 0 & !is.na(input$uCARE) & input$uCARE_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV in Care"
            year <- input$uCARE_year
            value <- input$uCARE
            weight <- as.vector(SourceList$weight[which(SourceList == input$uCARE_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uCARE =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"] <<- input$uCARE
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uCARE_source)])
            message(paste("MasterData updated with uCARE =", input$uCARE, "in", input$uCARE_year, "with weight", input$uCARE_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care",])
        }
    }
})

observeEvent(input$NEXT_art, {
    if (input$uART != 0 & !is.na(input$uART) & input$uART_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV on ART"
            year <- input$uART_year
            value <- input$uART
            weight <- as.vector(SourceList$weight[which(SourceList == input$uART_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uART =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"] <<- input$uART
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uART_source)])
            message(paste("MasterData updated with uART =", input$uART, "in", input$uART_year, "with weight", input$uART_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART",])
        }
    }
})

observeEvent(input$NEXT_viral, {
    if (input$uVIRAL != 0 & !is.na(input$uVIRAL) & input$uVIRAL_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV Suppressed"
            year <- input$uVIRAL_year
            value <- input$uVIRAL
            weight <- as.vector(SourceList$weight[which(SourceList == input$uVIRAL_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uVIRAL =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"] <<- input$uVIRAL
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uVIRAL_source)])
            message(paste("MasterData updated with uVIRAL =", input$uVIRAL, "in", input$uVIRAL_year, "with weight", input$uVIRAL_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed",])
        }
    }
})

####################
# RESET function calls

observeEvent(input$resetPLHIV, { shinyjs::reset("plhiv_panel") })
observeEvent(input$resetDIAG,  { shinyjs::reset("diag_panel")  })
observeEvent(input$resetCARE,  { shinyjs::reset("care_panel")  })
observeEvent(input$resetART,   { shinyjs::reset("art_panel")   })
observeEvent(input$resetVIRAL, { shinyjs::reset("viral_panel") })

observeEvent(input$resetParam, {
    # This needs to update ALL numeric inputs
    # shinyjs::reset() fucks it all up.
    parRange <- DefineParmRange()
    updateNumericInput(session, "test_DiagRate_U",       value = parRange["rho", "max"])
    updateNumericInput(session, "test_DiagRate_L",       value = parRange["rho", "min"])
    updateNumericInput(session, "test_LinkProp_U",       value = parRange["q", "max"])
    updateNumericInput(session, "test_LinkProp_L",       value = parRange["q", "min"])
    updateNumericInput(session, "test_LinkRate_U",       value = parRange["epsilon", "max"])
    updateNumericInput(session, "test_LinkRate_L",       value = parRange["epsilon", "min"])
    updateNumericInput(session, "test_ARTRate_U",        value = parRange["gamma", "max"])
    updateNumericInput(session, "test_ARTRate_L",        value = parRange["gamma", "min"])
    updateNumericInput(session, "test_ARTsideRate_U",    value = parRange["theta", "max"])
    updateNumericInput(session, "test_ARTsideRate_L",    value = parRange["theta", "min"])
    updateNumericInput(session, "test_PreARTDropRate_U", value = parRange["kappa", "max"])
    updateNumericInput(session, "test_PreARTDropRate_L", value = parRange["kappa", "min"])
    updateNumericInput(session, "test_ARTDropRate_U",    value = parRange["omega", "max"])
    updateNumericInput(session, "test_ARTDropRate_L",    value = parRange["omega", "min"])
    updateNumericInput(session, "test_AdhProp_U",        value = parRange["p", "max"])
    updateNumericInput(session, "test_AdhProp_L",        value = parRange["p", "min"])
})

observeEvent(input$resetTxGuidelines, {

    theGuidelines <- GetTreatmentGuidelines(uCountry = input$selectCountry)

    updateSelectInput(session, inputId = "userTx_l200", selected = theGuidelines[["less200"]])
    updateSelectInput(session, inputId = "userTx_l250", selected = theGuidelines[["less250"]])
    updateSelectInput(session, inputId = "userTx_l350", selected = theGuidelines[["less350"]])
    updateSelectInput(session, inputId = "userTx_l500", selected = theGuidelines[["less500"]])
    updateSelectInput(session, inputId = "userTx_m500", selected = theGuidelines[["more500"]])
})

observeEvent(input$resetDATA, {
    if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
    MasterData <<- GetMasterDataSet(input$selectCountry)
    shinyjs::reset("plhiv_panel")
    shinyjs::reset("diag_panel")
    shinyjs::reset("care_panel")
    shinyjs::reset("art_panel")
    shinyjs::reset("viral_panel")
})

####################
# Update parRange for calibration

observeEvent(input$uCalib_rho,     { userParRange$rho     <<- input$uCalib_rho     })
observeEvent(input$uCalib_epsilon, { userParRange$epsilon <<- input$uCalib_epsilon })
observeEvent(input$uCalib_kappa,   { userParRange$kappa   <<- input$uCalib_kappa   })
observeEvent(input$uCalib_gamma,   { userParRange$gamma   <<- input$uCalib_gamma   })
observeEvent(input$uCalib_theta,   { userParRange$theta   <<- input$uCalib_theta   })
observeEvent(input$uCalib_omega,   { userParRange$omega   <<- input$uCalib_omega   })
observeEvent(input$uCalib_p,       { userParRange$p       <<- input$uCalib_p       })
observeEvent(input$uCalib_q,       { userParRange$q       <<- input$uCalib_q       })

# need a max and min to override too.
# ALSO A RESET.

observeEvent(input$test_DiagRate_U,       { userParRange$rho_MAX     <<- input$test_DiagRate_U       })
observeEvent(input$test_DiagRate_L,       { userParRange$rho_MIN     <<- input$test_DiagRate_L       })
observeEvent(input$test_LinkProp_U,       { userParRange$q_MAX       <<- input$test_LinkProp_U       })
observeEvent(input$test_LinkProp_L,       { userParRange$q_MIN       <<- input$test_LinkProp_L       })
observeEvent(input$test_LinkRate_U,       { userParRange$epsilon_MAX <<- input$test_LinkRate_U       })
observeEvent(input$test_LinkRate_L,       { userParRange$epsilon_MIN <<- input$test_LinkRate_L       })
observeEvent(input$test_ARTRate_U,        { userParRange$gamma_MAX   <<- input$test_ARTRate_U        })
observeEvent(input$test_ARTRate_L,        { userParRange$gamma_MIN   <<- input$test_ARTRate_L        })
observeEvent(input$test_ARTsideRate_U,    { userParRange$theta_MAX   <<- input$test_ARTsideRate_U    })
observeEvent(input$test_ARTsideRate_L,    { userParRange$theta_MIN   <<- input$test_ARTsideRate_L    })
observeEvent(input$test_PreARTDropRate_U, { userParRange$kappa_MAX   <<- input$test_PreARTDropRate_U })
observeEvent(input$test_PreARTDropRate_L, { userParRange$kappa_MIN   <<- input$test_PreARTDropRate_L })
observeEvent(input$test_ARTDropRate_U,    { userParRange$omega_MAX   <<- input$test_ARTDropRate_U    })
observeEvent(input$test_ARTDropRate_L,    { userParRange$omega_MIN   <<- input$test_ARTDropRate_L    })
observeEvent(input$test_AdhProp_U,        { userParRange$p_MAX       <<- input$test_AdhProp_U        })
observeEvent(input$test_AdhProp_L,        { userParRange$p_MIN       <<- input$test_AdhProp_L        })

##### Calibration Settings #####

observeEvent(input$calib_speed, {
    updateNumericInput(session, inputId = "minResults", value = 100)
    updateSelectInput(session, inputId = "maxError", selected = "3")
})

observeEvent(input$calib_quality, {
    updateNumericInput(session, inputId = "minResults", value = 1000)
    updateSelectInput(session, inputId = "maxError", selected = "2")
})
