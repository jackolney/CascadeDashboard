tabItem(tabName = "opt-wizard",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Results",
            height = "500px",
            plotOutput('plotCascade_wizard')
        ),
        fluidRow(
            box(
                title = "Incidence",
                height = "300px",
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotNewInf_wizard')
            ),
            box(
                title = "AIDS Deaths",
                height = "300px",
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                plotOutput('plotAidsDeaths_wizard')
            )
        ),
        fluidRow(
            valueBoxOutput("vb_90_wizard"),
            valueBoxOutput("vb_9090_wizard"),
            valueBoxOutput("vb_909090_wizard")
        ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Optimisation Detail",
            "Click on the following buttons to access further details about the optimisation.",
            br(),
            fluidRow(
                column(width = 6,
                    bsButton(inputId = "NEXT_optParam",  label = "Interventions",  size = "large", style = "primary", block = TRUE, icon = icon("gear", class = "fa-lg fa-fw", lib = "font-awesome"))
                ),
                column(width = 6,
                    bsButton(inputId = "NEXT_optCost",   label = "Cost",           size = "large", style = "primary", block = TRUE, icon = icon("usd",  class = "fa-lg fa-fw", lib = "font-awesome"))
                )
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "HIV Testing",
            uiOutput("UI_optW_rho")
        ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Linkage",
            uiOutput("UI_optW_p")
        ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Pre-ART Retention",
            uiOutput("UI_optW_kappa")
        ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "ART Initiation",
            uiOutput("UI_optW_gamma")
        ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "ART Adherence",
            uiOutput("UI_optW_sigma")
        ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "ART Retention",
            uiOutput("UI_optW_omega")
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "optimStart", label = "Start", style = "success", size = "large", block = TRUE, icon = icon("play", class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "optimStop",  label = "Stop",  style = "danger",  size = "large", block = TRUE, icon = icon("stop", class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        ),
        br(),
        bsButton(inputId = "NEXT_optWizard", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome")),
        bsTooltip(id = "NEXT_optWizard", title = "Wait for progress bar to complete before proceeding.", placement = "bottom", trigger = "hover")
    )
)
