output$miniDataReview <- renderPlot({
    input$hot_cascade
    BuildDataReviewPlotMINI(data = values[["hot_cascade"]])
}, height = 200, width = 'auto', bg = 'transparent')
