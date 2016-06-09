tabItem(tabName = "parameters",
    column(width = 8,
        # Row ONE
        fluidRow(
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "Diagnosis Rate",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_rho', height = 'auto', width = 'auto'),
                "Rate at which undiagnosed individuals become diagnosed (py^-1).",
                uiOutput("calib_rho_max"),
                uiOutput("calib_rho_min"),
                uiOutput("UI_calib_rho")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "Proportion Linking to Care",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_q', height = 'auto', width = 'auto'),
                "Proportion of diagnosed individuals that link to care immediately.",
                uiOutput("calib_q_max"),
                uiOutput("calib_q_min"),
                uiOutput("UI_calib_q")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "Linkage Rate",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_epsilon', height = 'auto', width = 'auto'),
                "Rate at which diagnosed individuals are linked to care or fail to link.",
                uiOutput("calib_epsilon_max"),
                uiOutput("calib_epsilon_min"),
                uiOutput("UI_calib_epsilon")
            )
        ),
        # Row TWO
        fluidRow(
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "ART Initiation Rate (In Care)",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_gamma', height = 'auto', width = 'auto'),
                "ART initiation rate for diagnosed individuals engaged in pre-ART care (py^-1).",
                uiOutput("calib_gamma_max"),
                uiOutput("calib_gamma_min"),
                uiOutput("UI_calib_gamma")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "ART Initiation Rate (Not In Care)",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_theta', height = 'auto', width = 'auto'),
                "ART initiation rate individuals not currently engaged in pre-ART care. This is also a function of current CD4 count (py^-1).",
                uiOutput("calib_theta_max"),
                uiOutput("calib_theta_min"),
                uiOutput("UI_calib_theta")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "Pre-ART Dropout Rate",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_kappa', height = 'auto', width = 'auto'),
                "Rate at which diagnosed individuals disengage from pre-ART care (py^-1).",
                uiOutput("calib_kappa_max"),
                uiOutput("calib_kappa_min"),
                uiOutput("UI_calib_kappa")
            )
        ),
        # Row FOUR
        fluidRow(
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "ART Disengagement Rate",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_omega', height = 'auto', width = 'auto'),
                "Rate at which individuals on treatment disengage from ART care (py^-1).",
                uiOutput("calib_omega_max"),
                uiOutput("calib_omega_min"),
                uiOutput("UI_calib_omega")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "Proportion Adhering to Treatment",
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotCalibHist_p', height = 'auto', width = 'auto'),
                "Proportion of individuals initiating ART that adhere to treatment and become virally suppressed.",
                uiOutput("calib_p_max"),
                uiOutput("calib_p_min"),
                uiOutput("UI_calib_p")
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("During calibration the model simulates thousands of parameter sets to identify those that best fit the available data on the cascade.
                Based on the settings on the previous page, the model continues running simulations until it accrues a set of 100 (by default) parameter sets
                that each result in a total absolute model error of less than 2 (default).
                On this page is displayed the the maximum and minimum values found across all 100 parameter sets.
                Please check the values to ensure they look correct. If a specific rate or value is known by the user,
                then it can be entered in the relevant box ('Specify exact rate / proportion'), clicking 'Return' and then 'Repeat'
                will run the calibration again but keeping the user-defined parameter constant."),
            bsButton(inputId = "resetParam",     label = "RESET",     style = "danger",  size = "default", block = TRUE)
        ),
        bsButton(inputId = "PREV_param", label = "Return", style = "primary", size = "large", block = TRUE, icon = icon("backward", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
