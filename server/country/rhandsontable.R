# rhandsontable elements

values <- reactiveValues()

# Indicators
vCascade           = NULL
vCascadeCountry    = NULL
vCD4               = NULL
vCD4Country        = NULL
vCD42015           = NULL
vCD42015Country    = NULL
vIncidence         = NULL
vIncidenceCountry  = NULL
vGuidelines        = NULL
vGuidelinesCountry = NULL

### CASCADE
setHotCascade <- function(x) values[["hot_cascade"]] = x

# Observe and update data.frame on button press and also when values[["hot_cascade"]] changes

observe({
    # dependency
    input$PREV_editCascade
    if (!is.null(values[["hot_cascade"]]) & !is.null(master$data)) {
        master$data$calib <- na.omit(values[["hot_cascade"]])
    }
})

output$hot_cascade <- renderRHandsontable({
    input$CASCADE_FLAG

    # Update (01/08/16), working as it should (discussion: https://github.com/jrowen/rhandsontable/issues/27)
    if (input$NEW_country == TRUE & input$new_country_name != "") {
        vCascade <<- NULL
        if (is.null(input$hot_cascade) || is.null(vCascadeCountry)) {
            # This will be a blank master$data
            vCascade <<- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = master$data$calib)
            vCascadeCountry <<- 1L
            DF = master$data$calib
        } else if (!is.null(input$hot_cascade)) {
            DF = hot_to_r(input$hot_cascade)
        }
    } else {
        vCascadeCountry <<- NULL

        # BUG
        # If you select a country, then select some data, then select another country,
        # look at cascade data, don't edit and come out of it, and then click to edit
        # it again, it reverts to the previous country.
        # Maybe we need a flag that ignores input$hot_cascade if the country is incorrect
        # A 'vCascadeName' flag does not work, as when entering data MasterData overwrites it.

        # print("testing:")
        # if (!is.null(input$hot_cascade)) {
        #     message("hot_to_r(input$hot_cascade)$country[1]")
        #     print(hot_to_r(input$hot_cascade)$country[1])
        #     message("countryReportName")
        #     print(countryReportName)
        # }

        # message("values[['hot_cascade']]")
        # print(na.omit(values[["hot_cascade"]]))

        # Attempted fix.
        # Will the same hold for other values.
        # if (is.null(vCascade)) {
        #     # This will pad out the MasterData with NA's and update its name
        #     vCascade <- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        #     DF = AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        # } else if (is.null(input$hot_cascade)) {
        #     # This will pad out the MasterData with NA's and update its name
        #     vCascade <- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        #     DF = AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        # } else if (hot_to_r(input$hot_cascade)$country[1] != countryReportName) {
        #     # This will pad out the MasterData with NA's and update its name
        #     vCascade <- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        #     DF = AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = MasterData$calib)
        # } else {
        #     DF = hot_to_r(input$hot_cascade)
        # }

        # ORIGINAL - CURRENTLY IN USE
        # 06/09/16 - Reverting to the original as rhandsontable updates are too slow with the bugfix (will leave commented out in case we need to return to it)


        # UPDATE - 21/11/16
        # Now with a reactiveValue master$data, when you enter data, then change country, the data is not deleted.
        # Perhaps we need to include is.null(values[["hot_cascade"]]) in these if statements (it is set to NULL upon choosing a new country)
        # As of now, the answer is not clear, needs some more solid testing.

        # The problem is that this all gets run TWICE. Because master$data is REACTIVE, and so are the flags, they trigger multiple repeat calls of the function
        # Therefore, if INPUT is already defined, then it OVERWRITES EVERYTHING. How can I handle this?

        # If we have reactive flags, then rhandsontable gets called TWICE... this overwrites the behaviour we entered and screws things up.

        message("\nTesting:\n")
        # input$hot_cascade
        message("input$hot_cascade:")
        if (is.null(input$hot_cascade)) {
            print(NULL)
        } else {
            print(hot_to_r(input$hot_cascade))
        }

        # vCascade
        message("vCascade:")
        print(vCascade)

        # values[["hot_cascade"]]
        # if (exists('values[["hot_cascade"]]')) {
            message('values[["hot_cascade"]]:')
            print(values[["hot_cascade"]])
        # }

        if (is.null(input$hot_cascade) || is.null(vCascade)) {
            # This will pad out the MasterData with NA's and update its name
            vCascade <<- AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = master$data$calib)
            DF = AddNAToMasterData(theBlank = GetBlankMasterDataSet("blank")$calib, theData = master$data$calib)
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
            hot_col(col = "source", type = "autocomplete", strict = FALSE, allowInvalid = TRUE, halign = "htLeft") %>%
            hot_cols(colWidths = c(10,10,5,10,5,20)) %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

### CD4
setHotCD4 <- function(x) values[["hot_cd4"]] = x

# Observe and update data.frame on button press and also when values[["hot_cd4"]] changes

observe({
    # dependency
    input$PREV_editCD4
    if (!is.null(values[["hot_cd4"]]) & !is.null(master$data)) {
        master$data$cd4[2:15] <- values[["hot_cd4"]]$Proportion
        if (input$new_country_name != "") {
            master$data$cd4[[1]] <- input$new_country_name
        }
    }
})

output$hot_cd4 <- renderRHandsontable({
    input$CD4_FLAG

    if (input$NEW_country == TRUE & input$new_country_name != "") {
        vCD4 <<- NULL
        if (is.null(input$hot_cd4) || is.null(vCD4Country)) {
            Proportion <- as.numeric(NA)
            ART <- c(rep("Off ART", 7), rep("On ART", 7))
            Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
            DF = data.frame(ART, Category, Proportion)
            vCD4 <<- DF
            vCD4Country <<- 1L
        } else if (!is.null(input$hot_cd4)) {
            DF = hot_to_r(input$hot_cd4)
        }
    } else {
        vCD4Country <<- NULL
        if (is.null(input$hot_cd4) || is.null(vCD4)) {
            if (isReallyEmpty(master$data$cd4)) {
                Proportion <- as.numeric(NA)
            } else {
                Proportion <- as.numeric(master$data$cd4[2:15])
            }
            ART <- c(rep("Off ART", 7), rep("On ART", 7))
            Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
            DF = data.frame(ART, Category, Proportion)
            vCD4 <<- DF
        } else if (!is.null(input$hot_cd4)) {
            DF = hot_to_r(input$hot_cd4)
        }
    }

    setHotCD4(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "ART", readOnly = TRUE) %>%
            hot_col(col = "Category", readOnly = TRUE) %>%
            hot_col(col = "Proportion", type = "numeric", halign = "htLeft") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

### CD4 2015
setHotCD42015 <- function(x) values[["hot_cd4_2015"]] = x

# Observe and update data.frame on button press and also when values[["hot_cd4"]] changes

observe({
    # dependency
    input$PREV_editCD4
    if (!is.null(values[["hot_cd4_2015"]]) & !is.null(master$data)) {
        master$data$cd4_2015[2:15] <- values[["hot_cd4_2015"]]$Proportion
        if (input$new_country_name != "") {
            master$data$cd4_2015[[1]] <- input$new_country_name
        }
    }
})

output$hot_cd4_2015 <- renderRHandsontable({
    input$CD4_FLAG
    input$copy2010CD4

    if (input$NEW_country == TRUE & input$new_country_name != "") {
        vCD42015 <<- NULL
        if (is.null(input$hot_cd4_2015) || is.null(vCD42015Country)) {
            if (input$copy2010CD4 == TRUE) {
                Proportion <- as.numeric(master$data$cd4[2:15])
            } else {
                Proportion <- as.numeric(NA)
            }
            ART <- c(rep("Off ART", 7), rep("On ART", 7))
            Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
            DF = data.frame(ART, Category, Proportion)
            vCD42015 <<- DF
            vCD42015Country <<- 1L
        } else if (!is.null(input$hot_cd4_2015)) {
            DF = hot_to_r(input$hot_cd4_2015)
        }
    } else {
        vCD42015Country <<- NULL
        if (is.null(input$hot_cd4_2015) || is.null(vCD42015)) {
            if (isReallyEmpty(master$data$cd4_2015)) {
                Proportion <- as.numeric(NA)
            } else {
                Proportion <- as.numeric(master$data$cd4_2015[2:15])
            }
            ART <- c(rep("Off ART", 7), rep("On ART", 7))
            Category <- rep(c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"), 2)
            DF = data.frame(ART, Category, Proportion)
            vCD42015 <<- DF
        } else if (!is.null(input$hot_cd4_2015)) {
            DF = hot_to_r(input$hot_cd4_2015)
        }
    }

    setHotCD42015(DF)
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
    if (!is.null(values[["hot_incidence"]]) & !is.null(master$data)) {
        master$data$incidence <- values[["hot_incidence"]]
    }
})

output$hot_incidence <- renderRHandsontable({
    input$INCIDENCE_FLAG

    if (input$NEW_country == TRUE & input$new_country_name != "") {
        vIncidence <<- NULL
        if (is.null(input$hot_incidence) || is.null(vIncidenceCountry)) {
            theData <- master$data$incidence
            DF = theData[order(theData$type, decreasing = TRUE),]
            vIncidence <<- DF
            vIncidenceCountry <<- 1L
        } else if (!is.null(input$hot_incidence)) {
            DF = hot_to_r(input$hot_incidence)
        }
    } else {
        vIncidenceCountry <<- NULL
        if (is.null(input$hot_incidence) || is.null(vIncidence)) {
            theData <- master$data$incidence
            DF = theData[order(theData$type, decreasing = TRUE),]
            vIncidence <<- DF
        } else if (!is.null(input$hot_incidence)) {
            DF = hot_to_r(input$hot_incidence)
        }
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
    if (!is.null(values[["hot_guidelines"]]) & !is.null(master$data)) {
        master$data$treatment_guidelines[,c("less200", "less250", "less350", "less500", "more500")] <- values[["hot_guidelines"]]$Year
    }
})

output$hot_guidelines <- renderRHandsontable({
    input$GUIDELINES_FLAG

    if (input$NEW_country == TRUE & input$new_country_name != "") {
        vGuidelines <<- NULL
        if (is.null(input$hot_guidelines) || is.null(vGuidelinesCountry)) {
            Year <- as.numeric(NA)
            Threshold <- c("CD4 <200", "CD4 <250", "CD4 <350", "CD4 <500", "CD4 >500")
            DF = data.frame(Threshold, Year)
            vGuidelines <<- DF
            vGuidelinesCountry <<- 1L
        } else if (!is.null(input$hot_guidelines)) {
            DF = hot_to_r(input$hot_guidelines)
        }
    } else {
        vGuidelinesCountry <<- NULL
        if (is.null(input$hot_guidelines) || is.null(vGuidelines)) {
            Year <- as.numeric(master$data$treatment_guidelines[,c("less200", "less250", "less350", "less500", "more500")])
            Threshold <- c("CD4 <200", "CD4 <250", "CD4 <350", "CD4 <500", "CD4 >500")
            DF = data.frame(Threshold, Year)
            vGuidelines <<- DF
        } else if (!is.null(input$hot_guidelines)) {
            DF = hot_to_r(input$hot_guidelines)
        }
    }

    setHotGuidelines(DF)
    rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "Threshold", readOnly = TRUE) %>%
            hot_col(col = "Year", type = "date", dateFormat = "YYYY") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})
