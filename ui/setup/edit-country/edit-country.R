tabItem(tabName = "edit-country",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "New Country / Region",
            collapsible = TRUE,
            collapsed = FALSE,
            "Here will be some stuff about a new country..."
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please make a new country. Click the below buttons to edit details of the new country or region. When they turn green, we are good to go.")
        ),
        bsButton(inputId = "_edit-cascade_",     label = "Cascade Estimates",    style = "danger", size = "small", block = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
        bsButton(inputId = "_edit-cd4_",         label = "CD4 Distribution",     style = "danger", size = "small", block = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
        bsButton(inputId = "_edit-cascade_",     label = "Incidence",            style = "danger", size = "small", block = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
        bsButton(inputId = "_edit-guidelines_",  label = "Treatment Guidelines", style = "danger", size = "small", block = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_edit-country", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_edit-country" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
