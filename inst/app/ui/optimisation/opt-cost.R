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
            status = "primary",
            solidHeader = TRUE,
            title = "Intervention Cost",
            "Please review and edit the unit costs applied to the model using the sliders in the main
            panel. These costs will be applied to all simulations and will allow the cost of interventions
            to be quantified against a status quo scenario, in the absence of any intervention.",
            p(""),
            bsButton("resetCost", label = "RESET COST", block = TRUE, style = "danger", size = "default")
        ),
        box(width = NULL,
            status = "danger",
            solidHeader = TRUE,
            title = "Cost Adjustment",
            collapsible = TRUE,
            collapsed = FALSE,
            "The model does not capture HIV-negative individuals and assigns a fixed cost for the 'diagnosis' of one individual.
            This does not reflect the cost associated with testing HIV-negative individuals, nor does it reflect the increasing
            difficulty of locating infected individuals as testing coverage nears 100%.",
            p(""),
            "To account for this, the unit cost of 'diagnosis' is scaled up by estimating the probability of testing a positive
            individual given the size of the undiagnosed (not in care) population. This calculation uses an estimate of population
            size in 2015 and the assumption that testing occurs at random. The value below represents an estimate of the population
            size in 2015 from Spectrum. Please enter or update the estimate. If not estimate is available leave the cell blank and
            the cost of diagnosis will be scaled by 5, unless the checkbox is unticked.",
            br(),
            br(),
            checkboxInput(inputId = "adjustCost", label = tags$strong("Scale diagnosis cost to account for undiagnosed"), value = TRUE, width = NULL),
            uiOutput("UI_AdjustCostValue")
        ),
        bsButton(inputId = "PREV_optCost", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
