# Saving input values from setup tab.
# Will eventually be overtaken by knitr output using RMarkdown
saveCascadeData <- function(data) {
    # Grab the Google Sheet
    sheet <- gs_title("WHO-Cascade-Data")
    # Add the data as a new row
    gs_add_row(sheet, input = data)
}
