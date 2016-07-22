tabItem(tabName = "edit-cascade",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit Cascade",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please enter any available data on this page relating to the cascade.
                If data are missing, leave the cells blank and they will be ignored by the model.
                Click 'Back' to return to the previous page")
        ),
        bsButton(inputId = "PREV_editCascade", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
