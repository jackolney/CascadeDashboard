output$editCascadePlot <- renderPlot({
    input$hot_cascade
    BuildEditCascadePlot(data = values[["hot_cascade"]])
}, height = 250, width = 'auto', bg = 'transparent')

output$editCD4Plot <- renderPlot({
    input$hot_cd4
    BuildEditCD42010Plot(data = values[["hot_cd4"]])
}, height = 250, width = 'auto', bg = 'transparent')

output$editCD42015Plot <- renderPlot({
    input$hot_cd4_2015
    BuildEditCD42015Plot(data = values[["hot_cd4_2015"]])
}, height = 250, width = 'auto', bg = 'transparent')

output$editIncidencePlot <- renderPlot({
    input$hot_incidence
    BuildEditIncidencePlot(data = values[["hot_incidence"]])
}, height = 250, width = 'auto', bg = 'transparent')


output$weightTable <- renderTable(SourceListTable, include.rownames = FALSE)
