# Observe Functions for Mission Control
observeEvent(input$selectCountry, {

    if (CheckCSV_PLHIV(input$selectCountry) & CheckCSV_ART(input$selectCountry)) {
        updateButton(session, inputId = "CASCADE_FLAG",    style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "CASCADE_FLAG",    style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_CD4(input$selectCountry)) {
        updateButton(session, inputId = "CD4_FLAG",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "CD4_FLAG",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Incidence(input$selectCountry)) {
        updateButton(session, inputId = "INCIDENCE_FLAG",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "INCIDENCE_FLAG",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Treatment(input$selectCountry)) {
        updateButton(session, inputId = "GUIDELINES_FLAG",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "GUIDELINES_FLAG",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    # Trial placement of disable tags (we should be using the check functions instead of Check_CSV)
    updateButton(session, inputId = "CASCADE_FLAG",     disabled = FALSE)
    updateButton(session, inputId = "CD4_FLAG",         disabled = FALSE)
    updateButton(session, inputId = "INCIDENCE_FLAG",   disabled = FALSE)
    updateButton(session, inputId = "GUIDELINES_FLAG",  disabled = FALSE)

    # If MasterData exists then destroy it, then re-assign.
    if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
    try(MasterData <<- GetMasterDataSet(input$selectCountry), silent = FALSE)
    if (exists("MasterData")) {
        shinyBS::closeAlert(session, alertId = "alertId_DONOTPROCEED")
        shinyBS::createAlert(session,
            anchorId = "_PROCEED_",
            alertId = "alertId_PROCEED",
            title = paste(icon("check", class = "fa-lg fa-fw", lib = "font-awesome"), "PROCEED"),
            content = "The model has sufficient data to quantify the cascade.",
            style = "success",
            dismiss = TRUE,
            append = TRUE)
    } else {
        shinyBS::closeAlert(session, alertId = "alertId_PROCEED")
        shinyBS::createAlert(session,
            anchorId = "_DONOTPROCEED_",
            alertId = "alertId_DONOTPROCEED",
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "DO NOT PROCEED"),
            content = "The model has insufficient data to quantify the cascade. Please select another country.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
    }

})
