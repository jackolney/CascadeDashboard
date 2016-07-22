tabItem(tabName = "edit-cd4",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit CD4 Distribution in 2010",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot_cd4")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please enter any available data on this page relating to distribution of CD4
                counts in among infected persons in 2010. Values must be entered in every cell.
                Click 'Back' to return to the previous page")
        ),
        bsButton(inputId = "PREV_editCD4", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
