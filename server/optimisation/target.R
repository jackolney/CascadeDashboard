# custom will be the editable target that will re-generate
# optimisation results

custom <- reactiveValues(target = 0.9)

# slider UI
output$UI_909090_1_slider <- renderUI({
    sliderInput(inputId = "slider_909090_1", label = NULL, min = 0, max = 1, value = 0.9, step = 0.01, round = FALSE, ticks = FALSE, width = NULL)
})

output$UI_909090_2_slider <- renderUI({
    sliderInput(inputId = "slider_909090_2", label = NULL, min = 0, max = 1, value = 0.9, step = 0.01, round = FALSE, ticks = FALSE, width = NULL)
})

output$UI_909090_3_slider <- renderUI({
    sliderInput(inputId = "slider_909090_3", label = NULL, min = 0, max = 1, value = 0.9, step = 0.01, round = FALSE, ticks = FALSE, width = NULL)
})

# valueBox UI
output$VB_909090_1 <- renderValueBox({
    valueBox(value = scales::percent(input$slider_909090_1), subtitle = "Diagnosed / PLHIV", color = "red", width = NULL, icon = icon("bullseye", lib = "font-awesome"))
})
output$VB_909090_2 <- renderValueBox({
    valueBox(value = scales::percent(input$slider_909090_2), subtitle = "On Treatment / Diagnosed", color = "red", width = NULL, icon = icon("bullseye", lib = "font-awesome"))
})
output$VB_909090_3 <- renderValueBox({
    valueBox(value = scales::percent(input$slider_909090_3), subtitle = "Virally Suppressed / On Treatment", color = "red", width = NULL, icon = icon("bullseye", lib = "font-awesome"))
})

# cumulative valueBox UI
output$VB_cum_909090_1 <- renderValueBox({
    valueBox(value = scales::percent(input$slider_909090_1),
        subtitle = "Diagnosed / PLHIV ", color = "orange", width = NULL, icon = icon("bullseye", lib = "font-awesome"))
})

output$VB_cum_909090_2 <- renderValueBox({
    valueBox(value = scales::percent(round(input$slider_909090_1 * input$slider_909090_2, digits = 2)),
        subtitle = "On Treatment / PLHIV", color = "orange", width = NULL, icon = icon("bullseye", lib = "font-awesome"))
})

output$VB_cum_909090_3 <- renderValueBox({
    valueBox(value = scales::percent(round(input$slider_909090_1 * input$slider_909090_2 * input$slider_909090_3, digits = 2)),
        subtitle = "Virally Suppressed / PLHIV", color = "orange", width = NULL, icon = icon("bullseye", lib = "font-awesome"))
})

# Observe function on any change to the sliders, and update custom$target
observe({
    # dependency on sliders
    input$slider_909090_1
    input$slider_909090_2
    input$slider_909090_3

    # update reactiveValues
    custom$target <- input$slider_909090_1 * input$slider_909090_2 * input$slider_909090_3
    print(paste("custom$target =", custom$target))
})

# reset targets button
observeEvent(input$resetTarget, {
    shinyjs::reset("slider_909090_1")
    shinyjs::reset("slider_909090_2")
    shinyjs::reset("slider_909090_3")
})
