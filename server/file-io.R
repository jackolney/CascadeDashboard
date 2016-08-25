output$downloadMasterDataSet <- downloadHandler(
    filename = 'MasterData.RDS',
    content = function(file) {
        message("trying to save data.")
        if (exists("MasterData")) {
            saveRDS(MasterData, file = file)
        }
    }
)

observeEvent(input$uploadMasterDataSet, {
    # input$uploadMasterDataSet will be NULL initially.
    # After the user selects and uploads a file, it will be a
    # data frame with 'name', # 'size', 'type', and 'datapath' columns.
    # The 'datapath' column will contain the local filenames where the
    # data can be found.
    inFile <- input$uploadMasterDataSet

    if (!is.null(inFile)) {
        # Read the new file. RDS is a single R object
        newFile <- readRDS(inFile$datapath)
        # Then Overwrite MasterData... maybe see what happens?
        # Check with 'mission control' to see how it is handled.

        # Check to see if length of list == 6, verifying that it is a 'MasterData' object
        if (length(newFile) == 6) {
            # Remove old MasterData
            if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
            # Apply new one
            try(MasterData <<- newFile, silent = FALSE)

            # Shit load of button updates
            if (Check_NewCascade(theData = MasterData)) {
                updateButton(session, inputId = "CASCADE_FLAG",    disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
            } else {
                updateButton(session, inputId = "CASCADE_FLAG",    disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
            }

            if (Check_NewCD4(theData = MasterData) & Check_NewCD42015(theData = MasterData)) {
                updateButton(session, inputId = "CD4_FLAG",        disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
            } else {
                updateButton(session, inputId = "CD4_FLAG",        disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
            }

            if (Check_NewIncidence(theData = MasterData)) {
                updateButton(session, inputId = "INCIDENCE_FLAG",  disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
            } else {
                updateButton(session, inputId = "INCIDENCE_FLAG",  disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
            }

            if (Check_NewGuidelines(theData = MasterData)) {
                updateButton(session, inputId = "GUIDELINES_FLAG",  disabled = FALSE, style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
            } else {
                updateButton(session, inputId = "GUIDELINES_FLAG",  disabled = FALSE, style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
            }

            # Proceed flag updates
            if (Check_NewCascade(theData = MasterData) & Check_NewCD4(theData = MasterData) & Check_NewCD42015(theData = MasterData) & Check_NewIncidence(theData = MasterData) & Check_NewGuidelines(theData = MasterData)) {
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
        }
    }
})
