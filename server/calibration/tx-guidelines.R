## Treatment Guidelines ##

# MAYBE A RESET BUTTON TOO!?

GuidelineCheck <- function(tx_l200, tx_l250, tx_l350, tx_l500, tx_m500) {
    if (tx_l200 <= tx_l250 & tx_l200 <= tx_l350 & tx_l200 <= tx_l500 & tx_l200 <= tx_m500) {
        if (tx_l250 <= tx_l350 & tx_l250 <= tx_l500 & tx_l250 <= tx_m500) {
            if (tx_l350 <= tx_l500 & tx_l350 <= tx_m500) {
                if (tx_l500 <= tx_m500) {
                    return(TRUE)
                } else {return(FALSE)}
            } else {return(FALSE)}
        } else {return(FALSE)}
    } else {return(FALSE)}
}

# Render UI
output$UI_l200_tx <- renderUI({
    selectInput(inputId = "userTx_l200", label = "CD4 <200", choices = c("", seq(1990, 2020, 1)), selected = MasterData$treatment_guidelines[["less200"]])
})

output$UI_l250_tx <- renderUI({
    selectInput(inputId = "userTx_l250", label = "CD4 <250", choices = c("", seq(1990, 2020, 1)), selected = MasterData$treatment_guidelines[["less250"]])
})

output$UI_l350_tx <- renderUI({
    selectInput(inputId = "userTx_l350", label = "CD4 <350", choices = c("", seq(1990, 2020, 1)), selected = MasterData$treatment_guidelines[["less350"]])
})

output$UI_l500_tx <- renderUI({
    selectInput(inputId = "userTx_l500", label = "CD4 <500", choices = c("", seq(1990, 2020, 1)), selected = MasterData$treatment_guidelines[["less500"]])
})

output$UI_m500_tx <- renderUI({
    selectInput(inputId = "userTx_m500", label = "CD4 >500", choices = c("", seq(1990, 2020, 1)), selected = MasterData$treatment_guidelines[["more500"]])
})

# Need to observe changes and then update MasterData
# Also createAlert / closeAlert

observeEvent(input$userTx_l200, {
    if (input$userTx_l200 != "") {
        MasterData$treatment_guidelines[["less200"]] <- input$userTx_l200
        message("check <200:")
        print(MasterData$treatment_guidelines[["less200"]])

        # Down
        if (input$userTx_l250 != "" & input$userTx_l200 > input$userTx_l250) {
            shinyBS::createAlert(session,
                anchorId = "tx_l200_alert_l250",
                alertId = "alertId_l200_tx_l250",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <200 is higher than threshold for CD4 <250.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l200_tx_l250")
        }

        if (input$userTx_l350 != "" & input$userTx_l200 > input$userTx_l350) {
            shinyBS::createAlert(session,
                anchorId = "tx_l200_alert_l350",
                alertId = "alertId_l200_tx_l350",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <200 is higher than threshold for CD4 <350.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l200_tx_l350")
        }

        if (input$userTx_l500 != "" & input$userTx_l200 > input$userTx_l500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l200_alert_l500",
                alertId = "alertId_l200_tx_l500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <200 is higher than threshold for CD4 <500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l200_tx_l500")
        }

        if (input$userTx_m500 != "" & input$userTx_l200 > input$userTx_m500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l200_alert_m500",
                alertId = "alertId_l200_tx_m500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <200 is higher than threshold for CD4 >500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l200_tx_m500")
        }

        # Write a function to do GuidelineCheck() and return a boolean flag for button disabling.
        if (GuidelineCheck(tx_l200 = input$userTx_l200, tx_l250 = input$userTx_l250, tx_l350 = input$userTx_l350, tx_l500 = input$userTx_l500, tx_m500 = input$userTx_m500)) {
            updateButton(session, inputId = "CALIB_data", disabled = FALSE)
        } else {
            updateButton(session, inputId = "CALIB_data", disabled = TRUE)
        }
    }
})

observeEvent(input$userTx_l250, {
    if (input$userTx_l250 != "") {
        MasterData$treatment_guidelines[["less250"]] <- input$userTx_l250
        message("check <250:")
        print(MasterData$treatment_guidelines[["less250"]])

        # Down
        if (input$userTx_l350 != "" & input$userTx_l250 > input$userTx_l350) {
            shinyBS::createAlert(session,
                anchorId = "tx_l250_alert_l350",
                alertId = "alertId_l250_tx_l350",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <250 is higher than threshold for CD4 <350.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l250_tx_l350")
        }

        if (input$userTx_l500 != "" & input$userTx_l250 > input$userTx_l500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l250_alert_l500",
                alertId = "alertId_l250_tx_l500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <250 is higher than threshold for CD4 <500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l250_tx_l500")
        }

        if (input$userTx_m500 != "" & input$userTx_l250 > input$userTx_m500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l250_alert_m500",
                alertId = "alertId_l250_tx_m500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <250 is higher than threshold for CD4 >500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l250_tx_m500")
        }

        # UP
        if (input$userTx_l200 != "" & input$userTx_l250 < input$userTx_l200) {
            shinyBS::createAlert(session,
                anchorId = "tx_l250_alert_l200",
                alertId = "alertId_l250_tx_l200",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <250 is lower than threshold for CD4 <200.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l250_tx_l200")
        }

        if (GuidelineCheck(tx_l200 = input$userTx_l200, tx_l250 = input$userTx_l250, tx_l350 = input$userTx_l350, tx_l500 = input$userTx_l500, tx_m500 = input$userTx_m500)) {
            updateButton(session, inputId = "CALIB_data", disabled = FALSE)
        } else {
            updateButton(session, inputId = "CALIB_data", disabled = TRUE)
        }
    }
})

observeEvent(input$userTx_l350, {
    if (input$userTx_l350 != "") {
        MasterData$treatment_guidelines[["less350"]] <- input$userTx_l350
        message("check <350:")
        print(MasterData$treatment_guidelines[["less350"]])

        # Down
        if (input$userTx_l500 != "" & input$userTx_l350 > input$userTx_l500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l350_alert_l500",
                alertId = "alertId_l350_tx_l500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <350 is higher than threshold for CD4 <500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l350_tx_l500")
        }

        if (input$userTx_m500 != "" & input$userTx_l350 > input$userTx_m500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l350_alert_m500",
                alertId = "alertId_l350_tx_m500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <350 is higher than threshold for CD4 >500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l350_tx_m500")
        }

        # UP
        if (input$userTx_l200 != "" & input$userTx_l350 < input$userTx_l200) {
            shinyBS::createAlert(session,
                anchorId = "tx_l350_alert_l200",
                alertId = "alertId_l350_tx_l200",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <350 is lower than threshold for CD4 <200.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l350_tx_l200")
        }

        if (input$userTx_l250 != "" & input$userTx_l350 < input$userTx_l250) {
            shinyBS::createAlert(session,
                anchorId = "tx_l350_alert_l250",
                alertId = "alertId_l350_tx_l250",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <350 is lower than threshold for CD4 <250.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l350_tx_l250")
        }

        if (GuidelineCheck(tx_l200 = input$userTx_l200, tx_l250 = input$userTx_l250, tx_l350 = input$userTx_l350, tx_l500 = input$userTx_l500, tx_m500 = input$userTx_m500)) {
            updateButton(session, inputId = "CALIB_data", disabled = FALSE)
        } else {
            updateButton(session, inputId = "CALIB_data", disabled = TRUE)
        }
    }
})

observeEvent(input$userTx_l500, {
    if (input$userTx_l500 != "") {
        MasterData$treatment_guidelines[["less500"]] <- input$userTx_l500
        message("check <500:")
        print(MasterData$treatment_guidelines[["less500"]])

        # Down
        if (input$userTx_l500 != "" & input$userTx_l500 > input$userTx_l500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l500_alert_l500",
                alertId = "alertId_l500_tx_l500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <500 is higher than threshold for CD4 <500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l500_tx_l500")
        }

        if (input$userTx_m500 != "" & input$userTx_l500 > input$userTx_m500) {
            shinyBS::createAlert(session,
                anchorId = "tx_l500_alert_m500",
                alertId = "alertId_l500_tx_m500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <500 is higher than threshold for CD4 >500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l500_tx_m500")
        }

        # UP
        if (input$userTx_l200 != "" & input$userTx_l500 < input$userTx_l200) {
            shinyBS::createAlert(session,
                anchorId = "tx_l500_alert_l200",
                alertId = "alertId_l500_tx_l200",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <500 is lower than threshold for CD4 <200.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l500_tx_l200")
        }

        if (input$userTx_l250 != "" & input$userTx_l500 < input$userTx_l250) {
            shinyBS::createAlert(session,
                anchorId = "tx_l500_alert_l250",
                alertId = "alertId_l500_tx_l250",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <500 is lower than threshold for CD4 <250.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l500_tx_l250")
        }

        if (input$userTx_l350 != "" & input$userTx_l500 < input$userTx_l350) {
            shinyBS::createAlert(session,
                anchorId = "tx_l500_alert_l350",
                alertId = "alertId_l500_tx_l350",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 <500 is lower than threshold for CD4 <350.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_l500_tx_l350")
        }

        if (GuidelineCheck(tx_l200 = input$userTx_l200, tx_l250 = input$userTx_l250, tx_l350 = input$userTx_l350, tx_l500 = input$userTx_l500, tx_m500 = input$userTx_m500)) {
            updateButton(session, inputId = "CALIB_data", disabled = FALSE)
        } else {
            updateButton(session, inputId = "CALIB_data", disabled = TRUE)
        }
    }
})

observeEvent(input$userTx_m500, {
    if (input$userTx_m500 != "") {
        MasterData$treatment_guidelines[["more500"]] <- input$userTx_m500
        message("check >500:")
        print(MasterData$treatment_guidelines[["more500"]])

        # UP
        if (input$userTx_l200 != "" & input$userTx_m500 < input$userTx_l200) {
            shinyBS::createAlert(session,
                anchorId = "tx_m500_alert_l200",
                alertId = "alertId_m500_tx_l200",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 >500 is lower than threshold for CD4 <200.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_m500_tx_l200")
        }

        if (input$userTx_l250 != "" & input$userTx_m500 < input$userTx_l250) {
            shinyBS::createAlert(session,
                anchorId = "tx_m500_alert_l250",
                alertId = "alertId_m500_tx_l250",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 >500 is lower than threshold for CD4 <250.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_m500_tx_l250")
        }

        if (input$userTx_l350 != "" & input$userTx_m500 < input$userTx_l350) {
            shinyBS::createAlert(session,
                anchorId = "tx_m500_alert_l350",
                alertId = "alertId_m500_tx_l350",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 >500 is lower than threshold for CD4 <350.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_m500_tx_l350")
        }

        if (input$userTx_l500 != "" & input$userTx_m500 < input$userTx_l500) {
            shinyBS::createAlert(session,
                anchorId = "tx_m500_alert_l500",
                alertId = "alertId_m500_tx_l500",
                title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
                content = "Treatment threshold for CD4 >500 is lower than threshold for CD4 <500.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        } else {
            shinyBS::closeAlert(session, alertId = "alertId_m500_tx_l500")
        }

        if (GuidelineCheck(tx_l200 = input$userTx_l200, tx_l250 = input$userTx_l250, tx_l350 = input$userTx_l350, tx_l500 = input$userTx_l500, tx_m500 = input$userTx_m500)) {
            updateButton(session, inputId = "CALIB_data", disabled = FALSE)
        } else {
            updateButton(session, inputId = "CALIB_data", disabled = TRUE)
        }
    }
})

