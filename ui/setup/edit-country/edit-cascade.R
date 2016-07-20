tabItem(tabName = "edit-cascade",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit Cascade",
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
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_edit-cascade", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_edit-cascade" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
