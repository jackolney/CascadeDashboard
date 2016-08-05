tabItem(tabName = "edit-guidelines",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit Guidelines",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot_guidelines")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please enter the dates of changes in ART guidelines for the new country / region.
                Double clicking on a cell will bring up a calendar, but years can also be entered.
                Values must be supplied for every cell.
                Click 'Back' to return to the previous page"),
            h5("Conditions that must be satisfied for indicator to turn green:"),
            tags$ul(
                tags$li("A year must be entered for every threshold"),
                tags$li("Thresholds must not clash (i.e. <200 cannot be after <350)")
            )
        ),
        bsButton(inputId = "PREV_editGuidelines", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
