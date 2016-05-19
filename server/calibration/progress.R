prgoressBar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,
                        striped = FALSE, active = FALSE, vertical = FALSE) {
    stopifnot(is.numeric(value))
    if (value < 0 || value > 100)
        stop("'value' should be in the range from 0 to 100.", call. = FALSE)
    if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
        stop("'color' should be a valid status or color.", call. = FALSE)
    if (!is.null(size))
        size <- match.arg(size, c("sm", "xs", "xxs"))
    text_value <- paste0(value, "%")
    if (vertical)
        style <- htmltools::css(height = text_value, `min-height` = "2em")
    else
        style <- htmltools::css(width = text_value, `min-width` = "2em")
    tags$div(
        class = "progress",
        class = if (!is.null(size)) paste0("progress-", size),
        class = if (vertical) "vertical",
        class = if (active) "active",
        tags$div(
            class = "progress-bar",
            class = paste0("progress-bar-", color),
            class = if (striped) "progress-bar-striped",
            style = style,
            role = "progressbar",
            `aria-valuenow` = value,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100,
            tags$span(class = if (!label) "sr-only", text_value)
        )
    )
}

progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
    stopifnot(is.character(text))
    stopifnot(is.numeric(value))
    if (value < min || value > max)
        stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
    tags$div(
        class = "progress-group",
        tags$span(class = "progress-text", text),
        tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
        prgoressBar(round(value / max * 100), color = color, size = "sm")
    )
}

progressValue <- reactiveValues()
progressValue$one   <- 10
progressValue$two   <- 25
progressValue$three <- 50
progressValue$four  <- 75

# Render UI output
output$progressOne <- renderUI({
    progressGroup(text = "Sample Parameter Space",    value = progressValue$one,   min = 0, max = 100, color = "aqua")
})

output$progressTwo <- renderUI({
    progressGroup(text = "Evaluate Simulation Error", value = progressValue$two,   min = 0, max = 100, color = "red")
})

output$progressThree <- renderUI({
    progressGroup(text = "Resample top 10%",          value = progressValue$three, min = 0, max = 100, color = "green")
})

output$progressFour <- renderUI({
    progressGroup(text = "Compile Output",            value = progressValue$four,  min = 0, max = 100, color = "yellow")
})
