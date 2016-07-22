# rhandsontable elements

values <- reactiveValues()

### CASCADE
setHotCascade <- function(x) values[["hot_cascade"]] = x

# Observe and update data.frame on button press and also when values[["hot_cascade"]] changes
observe({
    # dependency
    input$PREV_editCascade

    if (!is.null(values[["hot_cascade"]])) {
        MasterData$calib <<- values[["hot_cascade"]]
        print(MasterData$calib)
    }
})

output$hot_cascade <- renderRHandsontable({
    if (!is.null(input$hot_cascade)) {
        DF = hot_to_r(input$hot_cascade)
        print(input$hot_cascade)
    } else {
        DF = MasterData$calib
    }
    setHotCascade(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "value", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "country", readOnly = TRUE) %>%
            hot_col(col = "indicator", readOnly = TRUE) %>%
            hot_col(col = "year", type = "date", format = "%Y", readOnly = TRUE) %>%
            hot_col(col = "weight", type = "dropdown", source = c("green", "amber", "red"), default = as.character(NA), strict = TRUE, allowInvalid = FALSE, halign = "htLeft",
                renderer = "function (instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value == 'green') {
                        td.style.background = 'green';
                    } else if (value == 'amber') {
                        td.style.background = 'orange';
                    } else if (value == 'red') {
                        td.style.background = 'red';
                    }
                }") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

### CD4
setHotCD4 <- function(x) values[["hot_cd4"]] = x

# Observe and update data.frame on button press and also when values[["hot_cd4"]] changes
observe({
    # dependency
    input$PREV_editCD4

    print("here")
    print(values[["hot_cd4"]])

    if (!is.null(values[["hot_cd4"]])) {
        MasterData$cd4[2:15] <<- values[["hot_cd4"]]$Proportion
        MasterData$cd4[[1]] <<- input$new_country_name
        print("hello")
        print(MasterData$cd4)
    }
})

output$hot_cd4 <- renderRHandsontable({
    if (!is.null(input$hot_cd4)) {
        DF = hot_to_r(input$hot_cd4)
        print(input$hot_cd4)
    } else {
        ART <- c(rep("Off ART", 7), rep("On ART", 7))
        Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
        Proportion <- as.numeric(NA)
        DF = data.frame(ART, Category, Proportion)
    }
    setHotCD4(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "ART", readOnly = TRUE) %>%
            hot_col(col = "Category", readOnly = TRUE) %>%
            hot_col(col = "Proportion", type = "numeric", halign = "htLeft") %>%
            hot_validate_numeric(col = "Proportion", min = 0, max = 1) %>%
            # hot_heatmap(cols = 3, color_scale = c("#ED6D47", "#17F556")) %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})
