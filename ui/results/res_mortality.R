tabItem(tabName = "mortality",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "AIDS Deaths",
            plotOutput('plotAidsDeaths', height = "500px")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "HIV-related Mortality",
            "Modelled estimates of changes to the number of AIDS-deaths between 2015 and 2020.
            Again, the bars illustrate the mean values arising from calibraiton, and the error bars show 95% confidence intervals."
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_mort", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_mort" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
