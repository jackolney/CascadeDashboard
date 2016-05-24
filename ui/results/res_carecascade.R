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
            title = "The Care Cascade between 2015 and 2020",
            "These figures illustrate how the structure of the cascade is likely to change over time
            from 2015 to 2020. This change arises from the parameter values derived during model calibration.
            The bars illustrate the mean value across all simulations and the error bars show the maximum and minimum
            values that arose during calibration."
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
