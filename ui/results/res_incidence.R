tabItem(tabName = "incidence",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "New Infections",
            plotOutput('plotNewInf', height = "500px")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "New Infections",
            "Modelled estimates of changes to the number of new infections between 2015 and 2020.
            The bars illustrate the mean value arising from calibration, and the error bars show 95% confidence intervals."
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_inc", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_inc" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
