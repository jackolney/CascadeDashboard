observeEvent(input$new_country_name, {
    message("okay, somebody just entered some text in new_country_name")
    print(input$new_country_name)

    # new we need to 'undisable' the buttons ...
    # activate links to 'edit' pages. (conditional on input$new_country_name != "")
    # have some methods of populating the MasterData list()
    if(input$new_country_name != "") {
        updateButton(session, inputId = "_CASCADE_FLAG_",   disabled = FALSE)
        updateButton(session, inputId = "_CD4_FLAG_",       disabled = FALSE)
        updateButton(session, inputId = "_Incidence_FLAG_", disabled = FALSE)
        updateButton(session, inputId = "_Treatment_FLAG_", disabled = FALSE)
    } else {
        updateButton(session, inputId = "_CASCADE_FLAG_",   disabled = TRUE)
        updateButton(session, inputId = "_CD4_FLAG_",       disabled = TRUE)
        updateButton(session, inputId = "_Incidence_FLAG_", disabled = TRUE)
        updateButton(session, inputId = "_Treatment_FLAG_", disabled = TRUE)
    }
})
