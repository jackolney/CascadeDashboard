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
            title = "Help Panel",
            "Review or edit the unit costs using the sliders on the left.",
            helpText("These costs will be applied to all runs within the optimisation section of this site. Click 'Return' and then run the optimisation to continue."),
            bsButton("resetCost", label = "RESET COST", block = TRUE, style = "danger")
        ),
        bsButton(inputId = "PREV_optCost", label = "Return", style = "primary", size = "large", block = TRUE, icon = icon("backward", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
