# Observe Functions for Mission Control
observeEvent(input$selectCountry, {

    if (CheckCSV_Incidence(input$selectCountry)) {
        updateButton(session, inputId = "_Incidence_FLAG_",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Incidence_FLAG_",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_CD4(input$selectCountry)) {
        updateButton(session, inputId = "_CD4_FLAG_",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_CD4_FLAG_",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Treatment(input$selectCountry)) {
        updateButton(session, inputId = "_Treatment_FLAG_",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Treatment_FLAG_",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_PLHIV(input$selectCountry)) {
        updateButton(session, inputId = "_PLHIV_FLAG_",      style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_PLHIV_FLAG_",      style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_ART(input$selectCountry)) {
        updateButton(session, inputId = "_ART_FLAG_",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_ART_FLAG_",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Additional(input$selectCountry)) {
        updateButton(session, inputId = "_Additional_FLAG_", style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Additional_FLAG_", style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if (CheckCSV_Rate(input$selectCountry)) {
        updateButton(session, inputId = "_Rates_FLAG_",      style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Rates_FLAG_",      style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    # If MasterData exists then destroy it, then re-assign.
    if (exists("MasterData")) rm(MasterData, pos = ".GlobalEnv")
    try(MasterData <<- GetMasterDataSet(input$selectCountry), silent = FALSE)
    if (exists("MasterData")) {
        updateButton(session, inputId = "_PROCEED_", label = "PROCEED", style = "success", icon = icon("thumbs-up ", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_PROCEED_", label = "DO NOT PROCEED", style = "danger", icon = icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

})
