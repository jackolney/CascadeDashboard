tabItem(tabName = "edit-guidelines",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit Guidelines",
            collapsible = TRUE,
            collapsed = FALSE,
            "Some means of entering data on the cascade"
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please make a new country. Click the below buttons to edit details of the new country or region. When they turn green, we are good to go.")
        ),
        bsButton(inputId = "PREV_editGuidelines", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
