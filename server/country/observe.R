observeEvent(input$new_country_name, {
    message("okay, somebody just entered some text in new_country_name")
    print(input$new_country_name)

    # new we need to 'undisable' the buttons ...
    # activate links to 'edit' pages. (conditional on input$new_country_name != "")
    # have some methods of populating the MasterData list()

    # The below buttons are still looking at the current country selected... when a name is entered,
    # we need to override this.
    # So when a 'name' of country is entered, all boxes should go RED and be enabled to allow data to be entered


    # _CASCADE_FLAG_
    # How do we entere data for this?? (pretty much replicating the before methods)

    #_CD4_FLAG_
    # Enter CD4 distribution? Sliders? range(0,1)

    # _Incidence_FLAG_
    # values that update a plot?

    # _Treatment_FLAG_
    # We already have this framework, just adapt for a full page

    # Does all this new work deprecate 'data-review'???

    # I'm not sure that is does straight away,, maybe we can loop both in

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
