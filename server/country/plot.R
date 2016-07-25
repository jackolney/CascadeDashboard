output$editCascadePlot <- renderPlot({
    input$hot_cascade
    BuildEditCascadePlot(data = values[["hot_cascade"]])
}, height = 250, width = 'auto', bg = 'transparent')
