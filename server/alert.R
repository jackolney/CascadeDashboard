# StartUp Caution Alert
shinyBS::createAlert(session,
    anchorId = "startAlert",
    alertId = NULL,
    title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
    content = "This interactive web-based model is still under development.
    Any data entered into the model is done so at the users own risk.
    Clicking 'save' in any tab saves the current inputs to a centrally accessible spreadsheet hosted by Google.
    Results produced by this model are not finalised.
    Use with caution!",
    style = "danger",
    dismiss = TRUE,
    append = TRUE)

# PopOver
shinyBS::addPopover(session, id = "plotOpt909090",
    title = "Info",
    content = "Vertical line represents 73% viral suppression (end goal of 90-90-90 targets, 0.9^3 = 0.729).",
    placement = "bottom",
    trigger = "hover",
    options = NULL)

## Setup Alerts
# Designed to catch users trying to enter non-sensical data
# Error if we don't enter ALL data - urgh.
observeEvent(input$uDIAG, {
    if (input$uDIAG != 0 & !is.na(input$uDIAG) & input$uPLHIV != 0 & !is.na(input$uPLHIV)) {
        if (input$uDIAG > input$uPLHIV) {
            # shinyBS alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT",
                alertId = "alertId_uDIAG",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Value entered for 'PLHIV Diagnosed' exceeds the value of 'PLHIV' in this year.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
            # JS alert & disable
            shinyjs::info(text = "Computer says no.")
            shinyjs::disable(id = "NEXT_diag")
        } else {
            shinyjs::enable(id = "NEXT_diag")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG")
        }
    }
})

observeEvent(input$uCARE, {
    if (input$uCARE != 0 & !is.na(input$uCARE) & input$uDIAG != 0 & !is.na(input$uDIAG)) {
        if (input$uCARE > input$uDIAG) {
            # shinyBS alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT",
                alertId = "alertId_uCARE",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Value entered for 'PLHIV in Care' exceeds the value of 'PLHIV Diagnosed' in this year.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
            # JS alert & disable
            shinyjs::info(text = "Computer says no.")
            shinyjs::disable(id = "NEXT_care")
        } else {
            shinyjs::enable(id = "NEXT_care")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE")
        }
    }
})

observeEvent(input$uART, {
    if (input$uART != 0 & !is.na(input$uART) & input$uCARE != 0 & !is.na(input$uCARE)) {
        if (input$uART > input$uCARE) {
            # shinyBS alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT",
                alertId = "alertId_uART",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Value entered for 'PLHIV on ART' exceeds the value of 'PLHIV in Care' in this year.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
            # JS alert & disable
            shinyjs::info(text = "Computer says no.")
            shinyjs::disable(id = "NEXT_art")
        } else {
            shinyjs::enable(id = "NEXT_art")
            shinyBS::closeAlert(session, alertId = "alertId_uART")
        }
    }
})

observeEvent(input$uVIRAL, {
    if (input$uVIRAL != 0 & !is.na(input$uVIRAL) & input$uART != 0 & !is.na(input$uART)) {
        if (input$uVIRAL > input$uART) {
            # shinyBS alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT",
                alertId = "alertId_uVIRAL",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Value entered for 'PLHIV Virally Suppressed' exceeds the value of 'PLHIV on ART' in this year.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
            # JS alert & disable
            shinyjs::info(text = "Computer says no.")
            shinyjs::disable(id = "NEXT_viral")
        } else {
            shinyjs::enable(id = "NEXT_viral")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL")
        }
    }
})

##########################
# 'Traffic Light' Alerts #
##########################

observeEvent(input$uPLHIV_source, {
    if (input$uPLHIV_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uPLHIV_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uPLHIV_ALERT_green",
                alertId = "alertId_uPLHIV_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uPLHIV_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_green")
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uPLHIV_ALERT_amber",
                alertId = "alertId_uPLHIV_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uPLHIV_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_green")
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uPLHIV_ALERT_red",
                alertId = "alertId_uPLHIV_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_green")
        shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_amber")
        shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_red")
    }
})

observeEvent(input$uDIAG_source, {
    if (input$uDIAG_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uDIAG_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT_green",
                alertId = "alertId_uDIAG_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uDIAG_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_green")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT_amber",
                alertId = "alertId_uDIAG_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uDIAG_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_green")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT_red",
                alertId = "alertId_uDIAG_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        shinyBS::closeAlert(session, alertId = "alertId_uDIAG_green")
        shinyBS::closeAlert(session, alertId = "alertId_uDIAG_amber")
        shinyBS::closeAlert(session, alertId = "alertId_uDIAG_red")
    }
})

observeEvent(input$uCARE_source, {
    if (input$uCARE_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uCARE_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT_green",
                alertId = "alertId_uCARE_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uCARE_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_green")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT_amber",
                alertId = "alertId_uCARE_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uCARE_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_green")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT_red",
                alertId = "alertId_uCARE_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        shinyBS::closeAlert(session, alertId = "alertId_uCARE_green")
        shinyBS::closeAlert(session, alertId = "alertId_uCARE_amber")
        shinyBS::closeAlert(session, alertId = "alertId_uCARE_red")
    }
})

observeEvent(input$uART_source, {
    if (input$uART_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uART_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uART_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uART_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT_green",
                alertId = "alertId_uART_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uART_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uART_green")
            shinyBS::closeAlert(session, alertId = "alertId_uART_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT_amber",
                alertId = "alertId_uART_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uART_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uART_green")
            shinyBS::closeAlert(session, alertId = "alertId_uART_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT_red",
                alertId = "alertId_uART_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        shinyBS::closeAlert(session, alertId = "alertId_uART_green")
        shinyBS::closeAlert(session, alertId = "alertId_uART_amber")
        shinyBS::closeAlert(session, alertId = "alertId_uART_red")
    }
})

observeEvent(input$uVIRAL_source, {
    if (input$uVIRAL_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uVIRAL_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT_green",
                alertId = "alertId_uVIRAL_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uVIRAL_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_green")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT_amber",
                alertId = "alertId_uVIRAL_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uVIRAL_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_green")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT_red",
                alertId = "alertId_uVIRAL_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_green")
        shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_amber")
        shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_red")
    }
})
