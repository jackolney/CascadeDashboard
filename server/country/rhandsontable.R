# rhandsontable elements

# create some reactive elements
values <- reactiveValues()
setHot <- function(x) values[["hot"]] = x

# Observe and update data.frame on button press and also when values[["hot"]] changes
observe({
    # dependency
    input$PREV_editCascade

    if (!is.null(values[["hot"]])) {
        MasterData$calib <- values[["hot"]]
        print(MasterData$calib)
    }
})

output$hot <- renderRHandsontable({
    if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
        print(input$hot)
    } else {
        DF = MasterData$calib
    }
    setHot(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "value", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "country", readOnly = TRUE) %>%
            hot_col(col = "indicator", readOnly = TRUE) %>%
            hot_col(col = "year", type = "date", format = "%Y", readOnly = TRUE) %>%
            hot_col(col = "weight", type = "dropdown", source = c("green", "amber", "red"), strict = TRUE, allowInvalid = FALSE, halign = "htLeft")
})
