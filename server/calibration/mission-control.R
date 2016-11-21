# Observe Functions for Mission Control
observeEvent(input$selectCountry, {

    # If master$data exists then make it NA, then re-assign.
    if (!is.na("master$data")) { master$data <- NA }
    # Assign values to master$data
    try(master$data <- GetMasterDataSet(input$selectCountry), silent = FALSE)

    if (!is.na("master$data")) {
        if (Check_NewCascade(theData = master$data)) {
            updateButton(session, inputId = "CASCADE_FLAG",    disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        } else {
            updateButton(session, inputId = "CASCADE_FLAG",    disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        }

        if (Check_NewCD4(theData = master$data) & Check_NewCD42015(theData = master$data)) {
            updateButton(session, inputId = "CD4_FLAG",        disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        } else {
            updateButton(session, inputId = "CD4_FLAG",        disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        }

        if (Check_NewIncidence(theData = master$data)) {
            updateButton(session, inputId = "INCIDENCE_FLAG",  disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        } else {
            updateButton(session, inputId = "INCIDENCE_FLAG",  disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        }

        if (Check_NewGuidelines(theData = master$data)) {
            updateButton(session, inputId = "GUIDELINES_FLAG",  disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        } else {
            updateButton(session, inputId = "GUIDELINES_FLAG",  disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        }

        if (Check_NewCascade(theData = master$data) & Check_NewCD4(theData = master$data) & Check_NewCD42015(theData = master$data) & Check_NewIncidence(theData = master$data) & Check_NewGuidelines(theData = master$data)) {
            shinyBS::closeAlert(session, alertId = "alertId_DONOTPROCEED")
            shinyBS::createAlert(session,
                anchorId = "_PROCEED_",
                alertId = "alertId_PROCEED",
                title = paste(icon("check", class = "fa-lg fa-fw", lib = "font-awesome"), "PROCEED"),
                content = "The model has sufficient data to quantify the cascade.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
            updateButton(session, inputId = "NEXT_country", disabled = FALSE)
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
            updateButton(session, inputId = "NEXT_country", disabled = TRUE)
        }
    } else {
        updateButton(session, inputId = "CASCADE_FLAG",    disabled = TRUE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, inputId = "CD4_FLAG",        disabled = TRUE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, inputId = "INCIDENCE_FLAG",  disabled = TRUE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, inputId = "GUIDELINES_FLAG", disabled = TRUE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        shinyBS::closeAlert(session, alertId = "alertId_PROCEED")
        shinyBS::createAlert(session,
            anchorId = "_DONOTPROCEED_",
            alertId = "alertId_DONOTPROCEED",
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "DO NOT PROCEED"),
            content = "The model has insufficient data to quantify the cascade. Please select another country.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        updateButton(session, inputId = "NEXT_country", disabled = TRUE)
    }
})
