tabItem(tabName = "opt-wizard",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Results",
            "The figure below illustrates the different combinations of interventions that achieve the
            viral suppression value in 2020 set by the slider on the right hand side
            (# virally suppressed / # on ART). This figure was calculated by by looking at each
            intervention in turn, and calculating the percentage increase in a particular rate
            (e.g. the testing rate) for each simulation, before taking the average across all
            simulations to arrive at the average percentage increase in each aspect of care.
            The figure shows for a given viral suppression value, which interventions were used and
            to what degree they were used. For further details, see the 'Strategy' section.",
            p(""),
            plotOutput('plotOptim_result', height = 'auto', width = 'auto'),
            bsModal(id = "optimDTmodalID", title = "Result Table (showing 90-90-90 targets)", trigger = "optData", size = "large",
                DT::dataTableOutput('optimDTmodal', width = "100%")
            )
        ),
        box(width = NULL,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Strategy",

            "The results of the optimisation indicate thousands of potential ways to improve care,
            either using interventions individually or in combination. Our simulations find that in
            order to achieve the level of viral suppression selected by the slider (right) by 2020,
            then over the next five years on average a number of changes must occur,
            these changes are described below:",
            p(""),
            tags$em("Please note that values below are absolute and not to be interpretted as the
                additional number of tests, initiations relative to a baseline scenario."),
            p(""),
            DT::dataTableOutput('optimDTout', width = "100%")
        ),
        box(width = NULL,
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Cost vs. Impact",
            selectInput(inputId = "userStratPoint",
                label = "Select intervention to stratify results by:",
                choices = InterventionList,
                selected = "Initiation"),
            plotOutput('plotOptim_CostImpact',
                dblclick = "plotOptim_CostImpact_dblclick",
                brush = brushOpts(
                    id = "plotOptim_CostImpact_brush",
                    clip = TRUE,
                    resetOnNew = TRUE
                ),
                height = 'auto',
                width = 'auto'
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Control",
            "This is the main intervention page, where all previously selected interventions are
            simulated and results presented.",
            p(""),
            sliderInput(inputId = "opt_VS_cutoff",
                label = "Only show interventions achieving viral suppression by 2020 of (%):",
                min = 0,
                max = 100,
                value = 50,
                step = 1,
                round = FALSE,
                ticks = TRUE,
                animate = FALSE,
                width = '100%',
                sep = ","),
            bsButton(inputId = "optData",
                        label = "View Results",
                        type = "action",
                        style = "primary",
                        size = "default",
                        block = TRUE,
                        icon = icon("database", class = "fa-lg fa-fw", lib = "font-awesome"))
        ),
        bsAlert(anchorId = "opt_VS_cutoff_alert"),
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
