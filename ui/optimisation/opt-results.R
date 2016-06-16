tabItem(tabName = "opt-results",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Results",
            tags$div(valueBoxOutput(outputId = "vbOptim_cutoff",     width = "100%"), style = "width: 50%; margin: auto;"),
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
            ),
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
            tags$em("Please note that negative values may be shown in the boxes above. This is correct,
                        and occurs because some interventions act to reduce losses from care relative to the status quo.
                        Additionally, certain changes may result in downstream deviations from the status quo,
                        causing reductions in future incidence, that further compound the alterations made to care.")
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
            "When this page renders, boxes summarising the results of the intervention simulations
            will be displayed. Adjusting the slider below will cause all values to update to illustrate
            the subset of results achieving the viral suppression value selected on the slider.
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
