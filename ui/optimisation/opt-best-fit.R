tabItem(tabName = "opt-best-fit",
    column(width = 8,
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Randomly shuffled 10% of Parameter Sets",
            bsModal(id = "seeDataTable_BestFit", title = "Best Fit Calibration Parameters", trigger = "viewData_BESTFIT", size = "large",
                DT::dataTableOutput('bestFitDT', width = "100%")
            ),
            plotOutput('optCalibBestFit', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Intervention Detail",
            "This page shows the calibration results of a randomly sampled 10% of accepted parameter sets.
            That is, a random 10% of parameter sets that produced the smallest total error. These results are highlighted
            in red with all other parameter sets coloured blue.
            Click 'View Parameters' for further details of each parameter.",
            p(""),
            bsButton(inputId = "viewData_BESTFIT",   label = "VIEW PARAMETERS", style = "primary", size = "default", block = TRUE)
        ),
        bsButton(inputId = "PREV_optBestFit", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
