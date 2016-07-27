tabItem(tabName = "opt-best-fit",
    column(width = 8,
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Top 10% 'Best Fitting' Parameter Sets",
            bsModal(id = "seeDataTable_BestFit", title = "Best Fit Calibration Parameters", trigger = "viewData_BESTFIT", size = "large",
                DT::dataTableOutput('bestFitDT', width = "100%")
            ),
            plotOutput('optCalibBestFit', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Detail",
            "This page shows the calibration results of the top 10% of best-fitting parameter sets.
            That is, the top 10% of parameter sets that produced the smallest total error. This line is highlighted
            in red and shows the optimal set of parameters that achieve the interventions.
            Click 'View Parameters' for further details of each parameter.",
            p(""),
            bsButton(inputId = "viewData_BESTFIT",   label = "VIEW PARAMETERS", style = "primary", size = "default", block = TRUE)
        ),
        bsButton(inputId = "PREV_optBestFit", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
