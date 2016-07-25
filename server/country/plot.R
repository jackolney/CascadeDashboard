output$editCascadePlot <- renderPlot({
    input$hot_cascade
    BuildEditCascadePlot(data = values[["hot_cascade"]])
}, height = 250, width = 'auto', bg = 'transparent')

output$editCD4Plot <- renderPlot({
    input$hot_cd4
    BuildEditCD4Plot(data = values[["hot_cd4"]])
}, height = 250, width = 'auto', bg = 'transparent')
