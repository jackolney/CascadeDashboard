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
            viral suppression value set by the slider on the right hand side (# virally suppressed
            / # on ART) by 2020. This figure was calculated by looking at the number of times each
            intervention achieved the viral suppression value on the right, and calculating for each
            simulation, the increase in a particular rate (e.g. the testing rate) relative to the
            baseline rate, then multiplying this proportional increase by the viral suppression value achieved,
            then penalising this intervention by the reciprocal of the number of active interventions.
            This means that weak interventions producing a large impact because of the presence of
            other active interventions are penalised in favour of powerful individual interventions.
            The central aim is to convey quickly to the reader, which interventions provide a large
            impact. For further details, see the 'Strategy' section.",
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
            tags$em("Please note that values below may indicate decreases in some aspects of care,
                this is correct, and explained by specific changes in care having an in-direct impact
                on reducing incidence, thereby reducing the total population of infected individuals."),
            p(""),
            DT::dataTableOutput('optimDTout', width = "100%"),
            p(""),
            tags$h3("UNAIDS 90-90-90 Targets by 2020"),
            "Given the changes listed in the above table, countries can expect to be well on the way
            to achieving the UNAIDS 90-90-90 targets by 2020. Expected values for each indicator are
            listed below, along with the total additional cost of changes to care.",
            p(""),
            fluidRow(
                column(width = 4,
                    valueBoxOutput(outputId = "vbOptim_909090_1", width = "100%")
                ),
                column(width = 4,
                    valueBoxOutput(outputId = "vbOptim_909090_2", width = "100%")
                ),
                column(width = 4,
                    valueBoxOutput(outputId = "vbOptim_909090_3", width = "100%")
                )
            ),
            tags$div(valueBoxOutput(outputId = "vbOptim_COST", width = "100%"), style = "width: 45%; margin: auto;")
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
