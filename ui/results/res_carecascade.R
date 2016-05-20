tabItem(tabName = "care_cascade",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Care Cascade",
            plotOutput('plotCascade')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "The distribution of care between 2015 and 2020",
            "Note, the denominator in all these calculations is # of PLHIV.",
            "These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020)."
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_careCascade", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_careCascade" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
