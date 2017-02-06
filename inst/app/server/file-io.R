output$downloadMasterDataSet <- downloadHandler(
    filename = 'MasterData.RDS',
    content = function(file) {
        message("trying to save data.")
        if (!is.null(MasterData)) {
            saveRDS(MasterData, file = file)
        }
    }
)

output$downloadExcel <- downloadHandler(
    filename = 'MasterData.xlsx',
    content = function(file) {
        message("trying to save data.")
        if (!is.null(MasterData)) {
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb = wb, sheetName = "cascade", gridLines = FALSE)
            openxlsx::writeDataTable(wb = wb, sheet = 1, x = MasterData$calib)
            openxlsx::addWorksheet(wb = wb, sheetName = "new infections", gridLines = FALSE)
            openxlsx::writeDataTable(wb = wb, sheet = 2, x = MasterData$incidence)
            openxlsx::addWorksheet(wb = wb, sheetName = "guidelines", gridLines = FALSE)
            openxlsx::writeDataTable(wb = wb, sheet = 3, x = MasterData$treatment_guidelines)
            openxlsx::addWorksheet(wb = wb, sheetName = "CD4 2010", gridLines = FALSE)
            openxlsx::writeDataTable(wb = wb, sheet = 4, x = MasterData$cd4)
            openxlsx::addWorksheet(wb = wb, sheetName = "CD4 2015", gridLines = FALSE)
            openxlsx::writeDataTable(wb = wb, sheet = 5, x = MasterData$cd4_2015)
            openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
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
            if (!is.null(MasterData)) MasterData <<- list()
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

observeEvent(input$uploadCascade, {
    # input$uploadCascade will be NULL initially.
    # After the user selects and uploads a file, it will be a
    # data frame with 'name', # 'size', 'type', and 'datapath' columns.
    # The 'datapath' column will contain the local filenames where the
    # data can be found.
    inFile <- input$uploadCascade

    if (!is.null(inFile)) {
        # Create filename shortcut
        fileName <- inFile$datapath
        # Read the file
        country <- openxlsx::read.xlsx(xlsxFile = fileName, sheet = 1, startRow = 3, cols = 1:2, colNames = FALSE)[1,2]
        # Check MasterData exists
        if (!is.null(MasterData)) {
            # Check name matches MasterData country name.
            if (MasterData$calib$country[1] == country) {
                # Read in inputValues
                inputValues <- openxlsx::read.xlsx(xlsxFile = fileName, sheet = 3, startRow = 31, colNames = TRUE, rows = 31:36)
                # Start creating variables for data.frame
                indicator <- c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed")
                year <- 2015
                value <- as.numeric(inputValues$Absolute.number)
                weight <- factor(x = "red", levels = c("red", "amber", "green"))
                source <- as.character(openxlsx::read.xlsx(xlsxFile = fileName, sheet = 3, startRow = 40, colNames = TRUE, rows = 40:45)[,2])
                source[is.na(source)] <- ""
                # Create data.frame
                new <- data.frame(country, indicator, year, value, weight, source, stringsAsFactors = FALSE)
                old <- MasterData$calib
                # Merge with MasterData
                MasterData$calib <<- rbind(old[old$year != 2015, ], new)
            } else {
                warning("Imported country name does not match MasterData")
            }
        } else {
            warning("MasterData does not exist")
        }
    }
})
