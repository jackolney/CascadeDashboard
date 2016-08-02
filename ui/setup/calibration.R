tabItem(tabName = "calibration",
    column(width = 8,
        box(width = NULL,
            status = "warning",
            title = "Calibration Information",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
                "Calibration begins as soon as the page loads, please wait until the progress bar has
                reached 100%. The model is calibrated by adjusting seven parameters simultaneously, and
                running thousands of simulations to identify parameter sets that best fit the available data.
                Parameter sets are only accepted if the absolute model error is less than the value specified
                in the box entitled 'Simulation Error' (2, by default). Simulations continue until the
                model has 100 (default) parameter sets with an absolute error below the specified value.",
            h4("Plots"),
                "When complete, a bar chart of the cascade in 2015 will be shown below illustrating the
                model's best estimate of the current cascade. Each bar represents the mean result of
                the accepted parameter sets. Additionally, the error bars illustrate the interval within
                which we are confident that 95% of results lie. For reference, any available
                data from 2015 is also shown on the figure. Further detail on changes in the cascade indicators
                over time is shown in the lower figure (fluctuations in certain variables are often the result
                of changes in treatment guidelines).",
            h4("Adjustment"),
                "Clicking the 'Adjust Parameters' button allows the user to adjust individual parameters
                in the model and re-run calibration with certain values fixed. Additionally, the maximum
                tolerated error can be adjusted along with the minimum number of simulations required.
                Clicking 'Repeat' will re-run the calibration and 'Accept' will move to the next page."
        ),
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
            collapsed = TRUE,
            plotOutput('plotCalibrationDetail', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Calibration Control",
            "This is the main model calibration page, where all previously entered data on the cascade
            is brought together with estimates of incidence and changes in treatment guidelines to
            identify a range of pragmatic parameter values that reconcile any uncertainty around the
            data to estimate the current trajectory of care.",
            p(""),
            bsButton(inputId = "ADJ_param", label = "Adjust Parameters", style = "primary",  size = "default", block = TRUE, icon = icon("wrench", class = "fa-lg fa-fw", lib = "font-awesome"))
        ),
        box(width = NULL,
            solidHeader = TRUE,
            status = "warning",
            title = "Simulation Error",
            collapsible = TRUE,
            collapsed = TRUE,
            helpText("
                Adjustments can be made below to the 'maximum tolerated absolute error per simulation'.
                Please use the histogram of model error to visualise the distribution of error across all simulations.
                The maximum tolerated error then adjusts the vertical bar shown on the plot below.
                Additionally, increasing the number of simulations required will increase calibration time."),
            tags$b("Optimise calibration for:"),
            fluidRow(
                column(width = 6,
                    bsButton(inputId = "calib_speed",   label = "Speed",   style = "default",  size = "extra-small", block = TRUE)
                ),
                column(width = 6,
                    bsButton(inputId = "calib_quality", label = "Quality", style = "default",  size = "extra-small", block = TRUE)
                )
            ),
            p(""),
            selectInput(inputId = "maxError", label = "Maximum tolerated total absolute error per simulation:", choices = ErrorList, selected = "2"),
            numericInput(inputId = "minResults", label = "Number of simulations required under max error:", value = 100,  min = 1, max = 1e6, step = 1, width = '100%'),
            plotOutput('plotCalibHist', height = 'auto', width = 'auto')
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "REPEAT_calib", label = "Repeat", style = "danger", size = "large", block = TRUE, icon = icon("repeat", class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "NEXT_calib", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        )
    )
)
