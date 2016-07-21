# fname = tempfile()

# uncomment lines below if action button is used to commit changes
# values = list()
# setHot = function(x) values[["hot"]] <<- x

# comment lines below if action button is used to commit changes
values = reactiveValues()
setHot = function(x) values[["hot"]] = x

observe({
    input$saveBtn

    if (!is.null(values[["hot"]])) {
        # write.csv(values[["hot"]], fname)
        # print(fname)
        print(values[["hot"]])
        # the above just holds the ORIGINAL df.
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

# So the above works and hitting 'save' returns the correct data.frame.. which can then be used to OVERRIDE MasterData$calib
# Why does the weight just overloaded though? THIS WORKS.
