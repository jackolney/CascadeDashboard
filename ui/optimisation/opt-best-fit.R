tabItem(tabName = "opt-best-fit",
    column(width = 8,
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "HIV-Testing",
            # Need a modal
            plotOutput('optCalibBestFit', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Detail",
            "Summary of best fit from model calibration. Illustrate calibration plot and best fitting LINE (red highlight). Also add a modal that describes parameter values."
        ),
        bsButton(inputId = "PREV_optBestFit", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
