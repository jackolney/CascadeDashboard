# rhandsontable elements

values <- reactiveValues()

### CASCADE
setHotCascade <- function(x) values[["hot_cascade"]] = x

# Observe and update data.frame on button press and also when values[["hot_cascade"]] changes

# indicators
val_cascade = NULL
val_cascadeCountry = NULL

observe({
    # dependency
    input$PREV_editCascade
    if (!is.null(values[["hot_cascade"]])) {
        MasterData$calib <<- na.omit(values[["hot_cascade"]])
        message("step back trigger")
        # print(MasterData$calib)
    }
})

output$hot_cascade <- renderRHandsontable({
    input$CASCADE_FLAG
    # if (!is.null(input$hot_cascade)) {
    #     DF = hot_to_r(input$hot_cascade)
    #     message("not null trigger")
    #     # print(DF)
    # } else {
    #     if (input$new_country_name == "") {
    #         # This will pad out the MasterData with NA's and update its name
    #         DF = AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
    #     } else {
    #         # This will be a blank MasterData
    #         DF = MasterData$calib
    #     }
    # }

    # Still a work in progress
    # discussed here:
    # https://github.com/jrowen/rhandsontable/issues/27
    if (input$NEW_country == TRUE & input$new_country_name != "") {
        val_cascade <<- NULL
        if (is.null(input$hot_cascade) || is.null(val_cascadeCountry)) {
            # This will be a blank MasterData
            val_ <<- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
            val_cascadeCountry <<- 1L
            DF = MasterData$calib
        } else if (!is.null(input$hot_cascade)) {
            DF = hot_to_r(input$hot_cascade)
        }
    } else {
        val_cascadeCountry <<- NULL
        if (is.null(input$hot_cascade) || val_$value != MasterData$calib$value) {
            # This will pad out the MasterData with NA's and update its name
            val_ <<- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
            DF = AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        } else if (!is.null(input$hot_cascade)) {
            DF = hot_to_r(input$hot_cascade)
        }
    }

    setHotCascade(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "value", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "country", readOnly = TRUE) %>%
            hot_col(col = "indicator", readOnly = TRUE) %>%
            hot_col(col = "year", type = "date", dateFormat = "%Y", readOnly = TRUE) %>%
            # the 'weight' column still shows NA instead of blank, due to the presence of the renderer function.
            hot_col(col = "weight", type = "dropdown", source = c("green", "amber", "red"), strict = TRUE, allowInvalid = FALSE, halign = "htLeft") %>%
                # renderer = "function (instance, td, row, col, prop, value, cellProperties) {
                #     Handsontable.renderers.TextRenderer.apply(this, arguments);
                #     if (value == 'green') {
                #         td.style.background = 'green';
                #     } else if (value == 'amber') {
                #         td.style.background = 'orange';
                #     } else if (value == 'red') {
                #         td.style.background = 'red';
                #     }
                # }") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

### CD4
setHotCD4 <- function(x) values[["hot_cd4"]] = x

# Observe and update data.frame on button press and also when values[["hot_cd4"]] changes
observe({
    # dependency
    input$PREV_editCD4
    if (!is.null(values[["hot_cd4"]])) {
        MasterData$cd4[2:15] <<- values[["hot_cd4"]]$Proportion
        MasterData$cd4[[1]] <<- input$new_country_name
        print(MasterData$cd4)
    }
})

output$hot_cd4 <- renderRHandsontable({
    input$CD4_FLAG
    if (!is.null(input$hot_cd4)) {
        DF = hot_to_r(input$hot_cd4)
        print(DF)
    } else {
        if (input$new_country_name == "") {
            if (isReallyEmpty(MasterData$cd4)) {
                Proportion <- as.numeric(NA)
            } else {
                Proportion <- as.numeric(MasterData$cd4[2:15])
            }
        } else {
            Proportion <- as.numeric(NA)
        }
        ART <- c(rep("Off ART", 7), rep("On ART", 7))
        Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
        DF = data.frame(ART, Category, Proportion)
    }
    setHotCD4(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "ART", readOnly = TRUE) %>%
            hot_col(col = "Category", readOnly = TRUE) %>%
            hot_col(col = "Proportion", type = "numeric", halign = "htLeft") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

### INCIDENCE
setHotIncidence <- function(x) values[["hot_incidence"]] = x

# Observe and update data.frame on button press and also when values[["hot_incidence"]] changes
observe({
    # dependency
    input$PREV_editIncidence
    if (!is.null(values[["hot_incidence"]])) {
        MasterData$incidence <<- values[["hot_incidence"]]
        print(MasterData$incidence)
    }
})

output$hot_incidence <- renderRHandsontable({
    input$INCIDENCE_FLAG
    if (!is.null(input$hot_incidence)) {
        DF = hot_to_r(input$hot_incidence)
        print(DF)
    } else {
        theData <- MasterData$incidence
        DF = theData[order(theData$type, decreasing = TRUE),]
    }
    setHotIncidence(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "country", readOnly = TRUE) %>%
            hot_col(col = "type", readOnly = TRUE) %>%
            hot_col(col = "2010", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "2011", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "2012", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "2013", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "2014", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "2015", format = '0,0', halign = "htLeft") %>%
            hot_col(col = "2016", format = '0,0', halign = "htLeft") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

### GUIDELINES
setHotGuidelines <- function(x) values[["hot_guidelines"]] = x

# Observe and update data.frame on button press and also when values[["hot_guidelines"]] changes
observe({
    # dependency
    input$PREV_editGuidelines
    if (!is.null(values[["hot_guidelines"]])) {
        MasterData$treatment_guidelines[,c("less200", "less250", "less350", "less500", "more500")] <<- values[["hot_guidelines"]]$Year
        message("hey jack")
        print(MasterData$treatment_guidelines)
    }
})

output$hot_guidelines <- renderRHandsontable({
    input$GUIDELINES_FLAG
    if (!is.null(input$hot_guidelines)) {
        DF = hot_to_r(input$hot_guidelines)
        print(DF)
    } else {
        if (input$new_country_name == "") {
            Year <- as.numeric(MasterData$treatment_guidelines[,c("less200", "less250", "less350", "less500", "more500")])
        } else {
            Year <- as.numeric(NA)
        }
        Threshold <- c("CD4 <200", "CD4 <250", "CD4 <350", "CD4 <500", "CD4 >500")
        DF = data.frame(Threshold, Year)

    }
    setHotGuidelines(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "Threshold", readOnly = TRUE) %>%
            hot_col(col = "Year", type = "date", dateFormat = "YYYY") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})
