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
            plotOutput('plotOptim_result')
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
                "Ignore the below",
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
                "Ignore the below",
                plotOutput('plotAidsDeaths_wizard')
            )
        ),
        fluidRow(
            valueBoxOutput("vb_90_wizard"),
            valueBoxOutput("vb_9090_wizard"),
            valueBoxOutput("vb_909090_wizard")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Control",
            "This is the main intervention page, where all previously selected interventions are
            simulated and results presented."
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "REPEAT_optim", label = "Repeat", style = "danger", size = "large", block = TRUE, icon = icon("repeat", class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "NEXT_optim", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        )
    )
)
