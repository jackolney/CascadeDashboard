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

values = reactiveValues()

data = reactive({
	if (!is.null(input$hot)) {
	 	DF = hot_to_r(input$hot)
	} else {
	 	if (is.null(values[["DF"]]))
	   		DF = MasterData$calib
	  else
	   	DF = values[["DF"]]
	}
	values[["DF"]] = DF
	DF
})

output$hot <- renderRHandsontable({
	DF = data()
	if (!is.null(DF))
	 	rhandsontable(DF, useTypes = TRUE, stretchH = "all")
})
