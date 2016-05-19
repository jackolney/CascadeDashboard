tabItem(tabName = "single_plot",
    column(width = 4,
        box(width = NULL,
            status = "info",
            solidHeader = FALSE,
            selectInput('y', 'Y', VariableNames, selected = "ART")
        )
    ),
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = FALSE,
            plotOutput("plotSingle")
        )
    )
)
