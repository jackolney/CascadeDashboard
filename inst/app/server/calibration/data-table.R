# Data Table Render Functions
output$dataTable_DATA <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_DATA

    return(
        datatable(MasterData$calib,
            style = 'bootstrap',
            options = list(
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                    "}"),
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value", currency = '')
    )
})

output$dataTable_PLHIV <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_PLHIV

    return(
        datatable(MasterData$calib,
            style = 'bootstrap',
            options = list(
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                    "}"),
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value", currency = '')
    )
})

output$dataTable_DIAG <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_DIAG

    return(
        datatable(MasterData$calib,
            style = 'bootstrap',
            options = list(
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                    "}"),
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value", currency = '')
    )
})

output$dataTable_CARE <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_CARE

    return(
        datatable(MasterData$calib,
            style = 'bootstrap',
            options = list(
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                    "}"),
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value", currency = '')
    )
})

output$dataTable_ART <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_ART

    return(
        datatable(MasterData$calib,
            style = 'bootstrap',
            options = list(
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                    "}"),
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value", currency = '')
    )
})

output$dataTable_VIRAL <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable_VIRAL

    return(
        datatable(MasterData$calib,
            style = 'bootstrap',
            options = list(
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4F8ABA', 'color': '#fff'});",
                    "}"),
                pageLength = 100,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        ) %>% formatCurrency("value", currency = '')
    )
})
