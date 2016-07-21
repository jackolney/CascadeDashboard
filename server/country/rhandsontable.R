# fname = tempfile()

# uncomment lines below if action button is used to commit changes
# values = list()
# setHot = function(x) values[["hot"]] <<- x

# comment lines below if action button is used to commit changes
# values = reactiveValues()
# setHot = function(x) values[["hot"]] = x

# # observe({
# #     input$saveBtn

# #     if (!is.null(values[["hot"]])) {
# #         write.csv(values[["hot"]], fname)
# #         print(fname)
# #     }
# # })

# output$hot = renderRHandsontable({
#     if (!is.null(input$hot)) {
#         DF = hot_to_r(input$hot)
#     } else {
#         DF = MasterData$calib
#     }

#     setHot(DF)
#     rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
#             hot_table(highlightCol = TRUE, highlightRow = TRUE)
# })


####
# MasterData as renderRHandsontable
####

# values = reactiveValues()

# data = reactive({
# 	if (!is.null(input$hot)) {
# 	 	DF = hot_to_r(input$hot)
# 	} else {
# 	 	if (is.null(values[["DF"]]))
# 	   		DF = MasterData$calib
# 	  else
# 	   	DF = values[["DF"]]
# 	}
# 	values[["DF"]] = DF
# 	DF
# })

output$hot <- renderRHandsontable({
	# DF = data()
	DF = MasterData$calib
	if (!is.null(DF))
	 	rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
            hot_col(col = "value", format = '0,0') %>%
	 		hot_col(col = "country", readOnly = TRUE) %>%
	 		hot_col(col = "indicator", readOnly = TRUE) %>%
	 		hot_col(col = "weight", type = "dropdown", source = c("green", "amber", "red"), strict = TRUE, allowInvalid = FALSE, halign = "htLeft", valign = "htMiddle") #%>%
            # hot_col(col = "year", type = "date", format = "%Y", readOnly = TRUE)
})

# on return, update masterData file.

# Need to factorise everything.
