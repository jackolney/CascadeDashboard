## PLHIV
observeEvent(input$uPLHIV, {
    if (!is.na(input$uPLHIV)) {
        output$UI_uPLHIV_national <- renderUI({
            tags$div(checkboxInput(inputId = "confirmPLHIV", label = "Confirm data is nationally representative", value = FALSE, width = NULL), style = "width: 60%; margin: auto; font-weight: bold; font-size: 110%;")
        })
        updateButton(session, inputId = "PREV_plhiv", disabled = TRUE)
        updateButton(session, inputId = "NEXT_plhiv", disabled = TRUE)
    } else {
        output$UI_uPLHIV_national <- renderUI({})
        updateButton(session, inputId = "PREV_plhiv", disabled = FALSE)
        updateButton(session, inputId = "NEXT_plhiv", disabled = FALSE)
    }
})

observeEvent(input$confirmPLHIV, {
    if (input$confirmPLHIV) {
        updateButton(session, inputId = "PREV_plhiv", disabled = FALSE)
        updateButton(session, inputId = "NEXT_plhiv", disabled = FALSE)
    } else {
        updateButton(session, inputId = "PREV_plhiv", disabled = TRUE)
        updateButton(session, inputId = "NEXT_plhiv", disabled = TRUE)
    }
})

## Diagnosed
observeEvent(input$uDIAG, {
    if (!is.na(input$uDIAG)) {
        output$UI_uDIAG_national <- renderUI({
            tags$div(checkboxInput(inputId = "confirmDIAG", label = "Confirm data is nationally representative", value = FALSE, width = NULL), style = "width: 60%; margin: auto; font-weight: bold; font-size: 110%;")
        })
        updateButton(session, inputId = "PREV_diag", disabled = TRUE)
        updateButton(session, inputId = "NEXT_diag", disabled = TRUE)
    } else {
        output$UI_uDIAG_national <- renderUI({})
        updateButton(session, inputId = "PREV_diag", disabled = FALSE)
        updateButton(session, inputId = "NEXT_diag", disabled = FALSE)
    }
})

observeEvent(input$confirmDIAG, {
    if (input$confirmDIAG) {
        updateButton(session, inputId = "PREV_diag", disabled = FALSE)
        updateButton(session, inputId = "NEXT_diag", disabled = FALSE)
    } else {
        updateButton(session, inputId = "PREV_diag", disabled = TRUE)
        updateButton(session, inputId = "NEXT_diag", disabled = TRUE)
    }
})

## CARE
observeEvent(input$uCARE, {
    if (!is.na(input$uCARE)) {
        output$UI_uCARE_national <- renderUI({
            tags$div(checkboxInput(inputId = "confirmCARE", label = "Confirm data is nationally representative", value = FALSE, width = NULL), style = "width: 60%; margin: auto; font-weight: bold; font-size: 110%;")
        })
        updateButton(session, inputId = "PREV_care", disabled = TRUE)
        updateButton(session, inputId = "NEXT_care", disabled = TRUE)
    } else {
        output$UI_uCARE_national <- renderUI({})
        updateButton(session, inputId = "PREV_care", disabled = FALSE)
        updateButton(session, inputId = "NEXT_care", disabled = FALSE)
    }
})

observeEvent(input$confirmCARE, {
    if (input$confirmCARE) {
        updateButton(session, inputId = "PREV_care", disabled = FALSE)
        updateButton(session, inputId = "NEXT_care", disabled = FALSE)
    } else {
        updateButton(session, inputId = "PREV_care", disabled = TRUE)
        updateButton(session, inputId = "NEXT_care", disabled = TRUE)
    }
})

## ART
observeEvent(input$uART, {
    if (!is.na(input$uART)) {
        output$UI_uART_national <- renderUI({
            tags$div(checkboxInput(inputId = "confirmART", label = "Confirm data is nationally representative", value = FALSE, width = NULL), style = "width: 60%; margin: auto; font-weight: bold; font-size: 110%;")
        })
        updateButton(session, inputId = "PREV_art", disabled = TRUE)
        updateButton(session, inputId = "NEXT_art", disabled = TRUE)
    } else {
        output$UI_uART_national <- renderUI({})
        updateButton(session, inputId = "PREV_art", disabled = FALSE)
        updateButton(session, inputId = "NEXT_art", disabled = FALSE)
    }
})

observeEvent(input$confirmART, {
    if (input$confirmART) {
        updateButton(session, inputId = "PREV_art", disabled = FALSE)
        updateButton(session, inputId = "NEXT_art", disabled = FALSE)
    } else {
        updateButton(session, inputId = "PREV_art", disabled = TRUE)
        updateButton(session, inputId = "NEXT_art", disabled = TRUE)
    }
})

## VIRAL
observeEvent(input$uVIRAL, {
    if (!is.na(input$uVIRAL)) {
        output$UI_uVIRAL_national <- renderUI({
            tags$div(checkboxInput(inputId = "confirmVIRAL", label = "Confirm data is nationally representative", value = FALSE, width = NULL), style = "width: 60%; margin: auto; font-weight: bold; font-size: 110%;")
        })
        updateButton(session, inputId = "PREV_viral", disabled = TRUE)
        updateButton(session, inputId = "NEXT_viral", disabled = TRUE)
    } else {
        output$UI_uVIRAL_national <- renderUI({})
        updateButton(session, inputId = "PREV_viral", disabled = FALSE)
        updateButton(session, inputId = "NEXT_viral", disabled = FALSE)
    }
})

observeEvent(input$confirmVIRAL, {
    if (input$confirmVIRAL) {
        updateButton(session, inputId = "PREV_viral", disabled = FALSE)
        updateButton(session, inputId = "NEXT_viral", disabled = FALSE)
    } else {
        updateButton(session, inputId = "PREV_viral", disabled = TRUE)
        updateButton(session, inputId = "NEXT_viral", disabled = TRUE)
    }
})
