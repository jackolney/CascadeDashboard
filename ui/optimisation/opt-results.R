tabItem(tabName = "opt-results",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Results",
            plotOutput('plotOptim_result', height = 'auto', width = 'auto'),
            p(""),
            "The figure below illustrates the various changes that must be made to care to achieve
            the viral suppression value set by the slider on the right hand side (# virally suppressed
            / # on ART) by 2020. This figure was calculated by taking the mean value of each parameter
            that achieved a viral suppression level above the value specified, then simulating the
            changes that must be made to each stage of care relative to a baseline scenario in the
            absence of any interventions. Values in this figure are enumerated below.",
            p(""),
            tags$em("Please note that negative values may be shown in the figure above. This is correct,
            and occurs because any changes made to care at any point in the cascade result in
            downstream deviations from the status quo, as all stages are linked. Additionally, certain
            changes may result in reductions in future incidence, further compounding the alterations
            visualised."),
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

            tags$div(valueBoxOutput(outputId = "vbOptim_COST",       width = "100%"), style = "width: 50%; margin: auto;"),
            fluidRow(
                column(width = 4,
                    infoBoxOutput(outputId = "vbOptim_testing",      width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vbOptim_linkage",      width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vbOptim_preRetention", width = "100%")
                )
            ),
            fluidRow(
                column(width = 4,
                    infoBoxOutput(outputId = "vbOptim_initiation",   width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vbOptim_adherence",    width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vbOptim_retention",    width = "100%")
                )
            ),
            fluidRow(
                column(width = 4,
                    valueBoxOutput(outputId = "vbOptim_909090_1",    width = "100%")
                ),
                column(width = 4,
                    valueBoxOutput(outputId = "vbOptim_909090_2",    width = "100%")
                ),
                column(width = 4,
                    valueBoxOutput(outputId = "vbOptim_909090_3",    width = "100%")
                )
            )


            # "The results of the optimisation indicate thousands of potential ways to improve care,
            # either using interventions individually or in combination. Our simulations find that in
            # order to achieve the level of viral suppression selected by the slider (right) by 2020,
            # then over the next five years on average a number of changes must occur,
            # these changes are described below:",
            # p(""),
            # tags$em("Please note that values below may indicate decreases in some aspects of care,
            #     this is correct, and explained by specific changes in care having an in-direct impact
            #     on reducing incidence, thereby reducing the total population of infected individuals."),
            # p(""),
            # DT::dataTableOutput('optimDTout', width = "100%"),
            # p(""),
            # tags$h3("UNAIDS 90-90-90 Targets by 2020"),
            # "Given the changes listed in the above table, countries can expect to be well on the way
            # to achieving the UNAIDS 90-90-90 targets by 2020. Expected values for each indicator are
            # listed below, along with the total additional cost of changes to care.",
            # p(""),
            # fluidRow(
            #     column(width = 4,
            #         valueBoxOutput(outputId = "vbOptim_909090_1", width = "100%")
            #     ),
            #     column(width = 4,
            #         valueBoxOutput(outputId = "vbOptim_909090_2", width = "100%")
            #     ),
            #     column(width = 4,
            #         valueBoxOutput(outputId = "vbOptim_909090_3", width = "100%")
            #     )
            # ),
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
            title = "Intervention Results",
            "When this page renders, figures summarising the results of the intervention simulations
            will be displayed in both figure and table form. Adjusting the slider below will cause all
            figures and tables to refresh and results will be a subset of the value selected.
            Hit 'Next' to identify the optimal route to achieving 90-90-90 by 2020.",
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
                bsButton(inputId = "PREV_optim", label = "Back", style = "danger", size = "large", block = TRUE, icon = icon("arrow-left", class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_optim" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
