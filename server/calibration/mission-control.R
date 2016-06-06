# Observe Functions for Mission Control
observeEvent(input$selectCountry, {

    if (CheckCSV_PLHIV(input$selectCountry) & CheckCSV_ART(input$selectCountry)) {
        updateButton(session, inputId = "_CASCADE_FLAG_",    style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_CASCADE_FLAG_",    style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_CD4(input$selectCountry)) {
        updateButton(session, inputId = "_CD4_FLAG_",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_CD4_FLAG_",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Incidence(input$selectCountry)) {
        updateButton(session, inputId = "_Incidence_FLAG_",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Incidence_FLAG_",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Treatment(input$selectCountry)) {
        updateButton(session, inputId = "_Treatment_FLAG_",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Treatment_FLAG_",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

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
