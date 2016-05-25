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
            sliderInput(inputId = 'userDxUnitCost',        label = 'Unit cost of diagnosing a patient (USD):',                min = 0, max = 100, value = 10,  step = 1),
            sliderInput(inputId = 'userLinkageUnitCost',   label = 'Unit cost of linking a patient to care (USD):',           min = 0, max = 100, value = 40,  step = 1),
            sliderInput(inputId = 'userAnnualCareUnit',    label = 'Annual cost of keeping a patient in pre-ART care (USD):', min = 0, max = 100, value = 40,  step = 1),
            sliderInput(inputId = 'userAnnualARTUnitCost', label = 'Annual cost of ART (USD):',                               min = 0, max = 500, value = 367, step = 1)
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
