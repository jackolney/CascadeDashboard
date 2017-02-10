output$console <- renderPrint({
    options(width = 120)
    devtools::session_info()
})

# Global Variable Testing
# output$global <- renderPrint({
#     # dependency on button press
#     input$update_global
#     options(width = 120)
#     as.data.frame(names(.GlobalEnv))
# })
