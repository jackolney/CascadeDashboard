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
                "Proportion of diagnosed individuals that link to care immediately.",
                uiOutput("calib_q_max"),
                uiOutput("calib_q_min"),
                uiOutput("UI_calib_q")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "ART Initiation Rate (In Care)",
                collapsible = TRUE,
                collapsed = FALSE,
                "ART initiation rate for diagnosed individuals engaged in pre-ART care (py^-1).",
                uiOutput("calib_gamma_max"),
                uiOutput("calib_gamma_min"),
                uiOutput("UI_calib_gamma")
            )
        ),
        # Row TWO
        fluidRow(
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "ART Initiation Rate (Not In Care)",
                collapsible = TRUE,
                collapsed = FALSE,
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
                "Rate at which diagnosed individuals disengage from pre-ART care (py^-1).",
                uiOutput("calib_kappa_max"),
                uiOutput("calib_kappa_min"),
                uiOutput("UI_calib_kappa")
            ),
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "ART Disengagement Rate",
                collapsible = TRUE,
                collapsed = FALSE,
                "Rate at which individuals on treatment disengage from ART care (py^-1).",
                uiOutput("calib_omega_max"),
                uiOutput("calib_omega_min"),
                uiOutput("UI_calib_omega")
            )
        ),
        # Row FOUR
        fluidRow(
            box(width = 4,
                solidHeader = TRUE,
                status = "primary",
                title = "Proportion Adhering to Treatment",
                collapsible = TRUE,
                collapsed = FALSE,
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
            helpText("Screw around with parameters here.")
        ),
        bsButton(inputId = "PREV_param", label = "Return", style = "primary", size = "large", block = TRUE, icon = icon("backward", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
