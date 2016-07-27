tabItem(tabName = "edit-incidence",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit New Infections",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot_incidence")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please enter any available data on this page relating to new HIV infections.
                Them model must be specified with an average value, and a lower and upper bound for all years
                between 2010 and 2016. As values are entered, a plot will be populated on the right-hand side.
                Click 'Back' to return to the previous page")
        ),
        bsButton(inputId = "PREV_editIncidence", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome")),
        p(""),
        plotOutput('editIncidencePlot', height = 'auto', width = 'auto')
    )
)
