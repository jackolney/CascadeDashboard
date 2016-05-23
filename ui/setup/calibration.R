tabItem(tabName = "calibration",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Result",
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('plotCalibration', height = 'auto', width = 'auto')
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Detail",
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('plotCalibrationDetail', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Calibration Control",
            "Hit 'Run Calibration' to stat the calibration, the progress bar will indicate the remaining run-time. Hit 'Accept' if you are content with the calibration figure and 'ghost-values' presented. If you are not happy with either, then please edit the ghost-values and hit 'Repeat'",
            p(""),
            bsButton(inputId = "ADJ_param", label = "Adjust Parameters", style = "primary",  size = "large", block = TRUE, icon = icon("wrench", class = "fa-lg fa-fw", lib = "font-awesome"))
        ),
        box(width = NULL,
            solidHeader = TRUE,
            status = "warning",
            title = "Calibration Detail",
            collapsible = TRUE,
            collapsed = TRUE,
            helpText("Consider the distribution of model error seen in the figure below and adjust the 'maximum tolerated error' accordingly. Also, for higher resolution, increase the number of iterations."),
            selectInput( inputId = "maxError",   label = "Maximum tolerated total error per simulation:", choices = ErrorList, selected = "2"),
            numericInput(inputId = "minResults", label = "Number of simulations required under max error:", value = 100,  min = 0, step = 1,   width = '100%'),
            plotOutput('plotCalibHist', height = 'auto', width = 'auto')
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "REPEAT_calib", label = "Repeat", style = "danger",  size = "large", block = TRUE, icon = icon("repeat", class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "NEXT_calib", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        )
    )
)
