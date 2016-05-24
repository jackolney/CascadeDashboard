tabItem(tabName = "incidence_mortality",
    column(width = 8,
        tabBox(
            width = NULL,
            height = "600px",
            title = "Results",
            tabPanel("Incidence",
                plotOutput('plotNewInf', height = "500px")
            ),
            tabPanel("AIDS Deaths",
                plotOutput('plotAidsDeaths', height = "500px")
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "New Infections & Mortality",
            h4("New Infections"),
            "The first tab shows the model estimates of changes in incident infections between 2015 and 2020.
            The bars illustrate the mean value arising from calibration, and the error bars illustrate
            the maximum and minimum values resulting from calibration.",
            br(),
            h4("AIDS Deaths"),
            "The second tab shows how AIDS-related deaths are expected to change between 2015 and 2020.
            Again, the bars illustrate the mean values arising from calibraiton, and the error bars the
            maximum and minimum resulting from calibration. As our uncertainty around results increases over time,
            the error bars become larger."
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_incMort", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_incMort" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
