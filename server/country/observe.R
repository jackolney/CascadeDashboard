observeEvent(input$new_country_name, {
    message(paste("A new country name has been entered:", input$new_country_name))


    # new we need to 'undisable' the buttons ...
    # activate links to 'edit' pages. (conditional on input$new_country_name != "")
    # have some methods of populating the MasterData list()

    # The below buttons are still looking at the current country selected... when a name is entered,
    # we need to override this.
    # So when a 'name' of country is entered, all boxes should go RED and be enabled to allow data to be entered


    # CASCADE_FLAG
    # How do we entere data for this?? (pretty much replicating the before methods)

    #CD4_FLAG
    # Enter CD4 distribution? Sliders? range(0,1)

    # INCIDENCE_FLAG
    # values that update a plot?

    # GUIDELINES_FLAG
    # We already have this framework, just adapt for a full page

    # Does all this new work deprecate 'data-review'???

    # I'm not sure that is does straight away,, maybe we can loop both in

    if(input$new_country_name == "") {
        updateTextInput(session, inputId = "new_country_name", value = "")
        updateButton(session,    inputId = "CASCADE_FLAG",   disabled = TRUE)
        updateButton(session,    inputId = "CD4_FLAG",       disabled = TRUE)
        updateButton(session,    inputId = "INCIDENCE_FLAG", disabled = TRUE)
        updateButton(session,    inputId = "GUIDELINES_FLAG", disabled = TRUE)
    } else {
        # These actually need to be CHECK functions (as when we edit the country name, then will all go red again)
        if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
        try(MasterData <<- GetBlankMasterDataSet(input$new_country_name), silent = FALSE)
        print(MasterData)
        updateButton(session, inputId = "CASCADE_FLAG",     style = "danger",  disabled = FALSE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, inputId = "CD4_FLAG",         style = "danger",  disabled = FALSE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, inputId = "INCIDENCE_FLAG",   style = "danger",  disabled = FALSE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, inputId = "GUIDELINES_FLAG",  style = "danger",  disabled = FALSE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
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

# This observe will kill any input into "new_country_name" if the 'NEW_country' button is deactivated
observeEvent(input$NEW_country, {
    if(input$NEW_country == FALSE) {
        updateTextInput(session, inputId = "new_country_name", value = "")
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


# Need to replicate mission-control.R

# if (CheckCSV_PLHIV(input$selectCountry) & CheckCSV_ART(input$selectCountry)) {
#     updateButton(session, inputId = "CASCADE_FLAG",    style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
# } else {
#     updateButton(session, inputId = "CASCADE_FLAG",    style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
# }

# if (CheckCSV_CD4(input$selectCountry)) {
#     updateButton(session, inputId = "CD4_FLAG",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
# } else {
#     updateButton(session, inputId = "CD4_FLAG",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
# }

# if (CheckCSV_Incidence(input$selectCountry)) {
#     updateButton(session, inputId = "INCIDENCE_FLAG",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
# } else {
#     updateButton(session, inputId = "INCIDENCE_FLAG",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
# }

# if (CheckCSV_Treatment(input$selectCountry)) {
#     updateButton(session, inputId = "GUIDELINES_FLAG",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
# } else {
#     updateButton(session, inputId = "GUIDELINES_FLAG",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
# }

# # # If MasterData exists then destroy it, then re-assign.
# if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
# try(MasterData <<- GetMasterDataSet(input$selectCountry), silent = FALSE)
# # if (exists("MasterData")) {/
# #     shinyBS::closeAlert(session, alertId = "alertId_DONOTPROCEED")
# #     shinyBS::createAlert(session,
# #         anchorId = "_PROCEED_",
# #         alertId = "alertId_PROCEED",
# #         title = paste(icon("check", class = "fa-lg fa-fw", lib = "font-awesome"), "PROCEED"),
# #         content = "The model has sufficient data to quantify the cascade.",
# #         style = "success",
# #         dismiss = TRUE,
# #         append = TRUE)
# # } else {
# #     shinyBS::closeAlert(session, alertId = "alertId_PROCEED")
# #     shinyBS::createAlert(session,
# #         anchorId = "_DONOTPROCEED_",
# #         alertId = "alertId_DONOTPROCEED",
# #         title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "DO NOT PROCEED"),
# #         content = "The model has insufficient data to quantify the cascade. Please select another country.",
# #         style = "danger",
# #         dismiss = TRUE,
# #         append = TRUE)
# # }

# GetBlankMasterDataSet


# if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
# try(MasterData <<- GetBlankMasterDataSet(input$selectCountry), silent = FALSE)

observeEvent(input$PREV_editCascade, {
    if (Check_NewCascade(MasterData)) {
        updateButton(session, inputId = "CASCADE_FLAG",    style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "CASCADE_FLAG",    style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})

