# Optimisation Test Bed
output$plotOptim_result <- renderPlot({
    # Repeat trigger
    input$REPEAT_optim
    # Run Optimsation Function
    theOut <- RunOptimisation()
    # BuildOptimisationPlot(theOut)
})
