## uPLHIV ##

output$UI_uPLHIV <- renderUI({
    numericInput("uPLHIV", "Number of people living with HIV:",
        value = NA,
        min = 0,
        width = '100%')
})

output$UI_uDIAG <- renderUI({
    numericInput("uDIAG", "Number of people diagnosed with HIV:",
        value = NA,
        min = 0,
        width = '100%')
})

output$UI_uCARE <- renderUI({
    numericInput("uCARE", "Number of people in HIV care:",
        value = NA,
        min = 0,
        width = '100%')
})

output$UI_uART <- renderUI({
    numericInput("uART", "Number of people on ART:",
        value = NA,
        min = 0,
        width = '100%')
})

output$UI_uVIRAL <- renderUI({
    numericInput("uVIRAL", "Number of people on ART and Virally Suppressed:",
        value = NA,
        min = 0,
        width = '100%')
})

output$UI_uPLHIV_source <- renderUI({
    selectInput("uPLHIV_source", "Source of Data:", as.vector(SourceList$name), selected = "Please select source...")
})

output$UI_uDIAG_source <- renderUI({
    selectInput("uDIAG_source", "Source of Data:",  as.vector(SourceList$name), selected = "Please select source...")
})

output$UI_uCARE_source <- renderUI({
    selectInput("uCARE_source", "Source of Data:",  as.vector(SourceList$name), selected = "Please select source...")
})

output$UI_uART_source <- renderUI({
    selectInput("uART_source", "Source of Data:",   as.vector(SourceList$name), selected = "Please select source...")
})

output$UI_uVIRAL_source <- renderUI({
    selectInput("uVIRAL_source", "Source of Data:", as.vector(SourceList$name), selected = "Please select source...")
})

output$UI_uPLHIV_year <- renderUI({
    selectInput("uPLHIV_year", "Year data represents:", YearList, selected = 2015)
})

output$UI_uDIAG_year <- renderUI({
    selectInput("uDIAG_year", "Year data represents:",  YearList, selected = 2015)
})

output$UI_uCARE_year <- renderUI({
    selectInput("uCARE_year", "Year data represents:",  YearList, selected = 2015)
})

output$UI_uART_year <- renderUI({
    selectInput("uART_year", "Year data represents:",   YearList, selected = 2015)
})

output$UI_uVIRAL_year <- renderUI({
    selectInput("uVIRAL_year", "Year data represents:", YearList, selected = 2015)
})
