output$console <- renderPrint({
    options(width = 120)
    devtools::session_info()
})
