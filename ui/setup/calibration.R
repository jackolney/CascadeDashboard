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
        # Row ONE
        fluidRow(
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Diagnosis Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Rate at which undiagnosed individuals become diagnosed (py^-1).",
                uiOutput("calib_rho_max"),
                uiOutput("calib_rho_min"),
                uiOutput("UI_calib_rho")
            ),
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Proportion Linking to Care",
                collapsible = TRUE,
                collapsed = TRUE,
                "Proportion of diagnosed individuals that link to care immediately.",
                uiOutput("calib_q_max"),
                uiOutput("calib_q_min"),
                uiOutput("UI_calib_q")
            )
        ),
        # Row TWO
        fluidRow(
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "ART Initiation Rate (In Care)",
                collapsible = TRUE,
                collapsed = TRUE,
                "ART initiation rate for diagnosed individuals engaged in pre-ART care (py^-1).",
                uiOutput("calib_gamma_max"),
                uiOutput("calib_gamma_min"),
                uiOutput("UI_calib_gamma")
            ),
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "ART Initiation Rate (Not In Care)",
                collapsible = TRUE,
                collapsed = TRUE,
                "ART initiation rate individuals not currently engaged in pre-ART care. This is also a function of current CD4 count (py^-1).",
                uiOutput("calib_theta_max"),
                uiOutput("calib_theta_min"),
                uiOutput("UI_calib_theta")
            )
        ),
        # Row THREE
        fluidRow(
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Pre-ART Dropout Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Rate at which diagnosed individuals disengage from pre-ART care (py^-1).",
                uiOutput("calib_kappa_max"),
                uiOutput("calib_kappa_min"),
                uiOutput("UI_calib_kappa")
            ),
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "ART Disengagement Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Rate at which individuals on treatment disengage from ART care (py^-1).",
                uiOutput("calib_omega_max"),
                uiOutput("calib_omega_min"),
                uiOutput("UI_calib_omega")
            )
        ),
        # Row FOUR
        fluidRow(
            box(width = 6,
                solidHeader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Proportion Adhering to Treatment",
                collapsible = TRUE,
                collapsed = TRUE,
                "Proportion of individuals initiating ART that adhere to treatment and become virally suppressed.",
                uiOutput("calib_p_max"),
                uiOutput("calib_p_min"),
                uiOutput("UI_calib_p")
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            background = "yellow",
            solidHeader = TRUE,
            title = "Calibration Control",
            "Hit 'Run Calibration' to stat the calibration, the progress bar will indicate the remaining run-time. Hit 'Accept' if you are content with the calibration figure and 'ghost-values' presented. If you are not happy with either, then please edit the ghost-values and hit 'Repeat'",
            p(""),
            bsButton(inputId = "REPEAT_calib", label = "Repeat", style = "danger",  size = "large", block = TRUE, icon = icon("repeat", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "NEXT_calib", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
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
        )
    )
)
