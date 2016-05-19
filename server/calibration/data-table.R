# Data Table Render Functions
output$dataTable_DATA <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_DATA

    return(
        datatable(MasterData$calib,
            options = list(
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value",'')
    )
})

output$dataTable_PLHIV <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_PLHIV

    return(
        datatable(MasterData$calib,
            options = list(
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value",'')
    )
})

output$dataTable_DIAG <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_DIAG

    return(
        datatable(MasterData$calib,
            options = list(
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value",'')
    )
})

output$dataTable_CARE <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_CARE

    return(
        datatable(MasterData$calib,
            options = list(
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value",'')
    )
})

output$dataTable_ART <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_ART

    return(
        datatable(MasterData$calib,
            options = list(
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value",'')
    )
})

output$dataTable_VIRAL <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_VIRAL

    return(
        datatable(MasterData$calib,
            options = list(
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value",'')
    )
})
