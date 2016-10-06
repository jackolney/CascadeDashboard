tabItem(tabName = "opt-cost",
    column(width = 8,
        shinyjs::useShinyjs(),
        id = "cost-panel",
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            title = "Cost",
            collapsible = TRUE,
            collapsed = FALSE,
            uiOutput("UI_DxUnitCost"),
            uiOutput("UI_LinkageUnitCost"),
            uiOutput("UI_AnnualCareCost"),
            uiOutput("UI_AnnualARTCost")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Cost",
            "Please review and edit the unit costs applied to the model using the sliders in the main
            panel. These costs will be applied to all simulations and will allow the cost of interventions
            to be quantified against a status quo scenario, in the absence of any intervention.",
            p(""),
            bsButton("resetCost", label = "RESET COST", block = TRUE, style = "danger", size = "default")
        ),
        bsButton(inputId = "PREV_optCost", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
