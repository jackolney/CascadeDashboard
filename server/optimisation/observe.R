observeEvent(input$NEXT_optIntro, {
    optResults <<- RunOptimisation()
})

plotFrontier.ranges  <- reactiveValues(x = NULL, y = NULL)

observeEvent(input$plotFrontier_dblclick, {
    brush <- input$plotFrontier_brush
    if (!is.null(brush)) {
        plotFrontier.ranges$x <- c(brush$xmin, brush$xmax)
        plotFrontier.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotFrontier.ranges$x <- NULL
        plotFrontier.ranges$y <- NULL
    }
})
