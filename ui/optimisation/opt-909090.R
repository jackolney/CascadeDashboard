tabItem(tabName = "opt-909090",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "UNAIDS 90-90-90",
            fluidRow(
                column(width = 4,
                    valueBoxOutput(outputId = "VB_909090_1",     width = "100%"),
                    valueBoxOutput(outputId = "VB_cum_909090_1", width = "100%"),
                    uiOutput("UI_909090_1_slider")
                ),
                column(width = 4,
                    valueBoxOutput(outputId = "VB_909090_2",     width = "100%"),
                    valueBoxOutput(outputId = "VB_cum_909090_2", width = "100%"),
                    uiOutput("UI_909090_2_slider")
                ),
                column(width = 4,
                    valueBoxOutput(outputId = "VB_909090_3",     width = "100%"),
                    valueBoxOutput(outputId = "VB_cum_909090_3", width = "100%"),
                    uiOutput("UI_909090_3_slider")
                )
            ),
            bsModal(id = "optimDT909090modalID", title = "Result Table", trigger = "optData909090", size = "large",
                DT::dataTableOutput('optimDT909090modal', width = "100%")
            )
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "How to get there?",
            # tags$div(valueBoxOutput(outputId = "vb909090_COST", width = "100%"), style = "width: 45%; margin: auto;"),
            # COST
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "vb909090_COST_BASE",        width = "100%" )
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "vb909090_COST_NEW",         width = "100%" )
                )
            ),

            # PREV
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_prevention_BASE",   width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_prevention",        width = "100%" )
                )
            ),

            # TEST
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_testing_BASE",      width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_testing",           width = "100%" )
                )
            ),

            # LINK
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_linkage_BASE",      width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_linkage",           width = "100%" )
                )
            ),

            # PRE-RETENTION
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_preRetention_BASE", width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_preRetention",      width = "100%" )
                )
            ),

            # INITIATION
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_initiation_BASE",   width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_initiation",        width = "100%" )
                )
            ),

            # RETENTION
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_retention_BASE",    width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_retention",         width = "100%" )
                )
            ),

            # ADHERENCE
            fluidRow(
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_adherence_BASE",    width = "100%" )
                ),
                column(width = 6,
                    infoBoxOutput(outputId = "vb909090_adherence",         width = "100%" )
                )
            ),
            tags$em("
                The average cost and changes to care required to achieve the
                90-90-90 targets are displayed above.
                On the left - in red - illustrates that changes that would happen
                to care in the absence of any interventions i.e. the 'baseline'.
                On the right - in green - illustrates the additional changes
                required to achieve 90-90-90 through implementing interventions.
                These values illustrate the per year changes to care and are
                calculated from simulating all combinations of interventions and
                assessing the outcomes relative to a baseline scenario in the absence of any
                interventions. We then calculate the changes in each indicator between
                baseline and intervention scenario, repeat this calculation for
                all best-fitting parameter sets derived from our calibration,
                and present the average across all simulations as per year
                values together with 95% confidence intervals. If a box displays
                zero, then no changes are required to achieve 90-90-90.")
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Cost-Effectiveness Frontier",
            plotOutput('plotFrontier',
                dblclick = "plotFrontier_dblclick",
                brush = brushOpts(
                    id = "plotFrontier_brush",
                    clip = TRUE,
                    resetOnNew = TRUE
                ),
                height = 'auto',
                width = 'auto'
            ),
            tags$em("
                The figure above illustrates the calculations behind the values above.
                Each blue dot represents an individual simulation in terms of its
                impact and cost relative to a baseline scenario. By simulating
                all possible combinations of interventions and all best-fitting
                parameter sets we produce a 'cloud' of potential interventions
                that will bring about improvements to patient outcomes. We select
                the simulations that achieve 73% viral suppression by 2020 (the
                end point of 90-90-90, 0.9^3), and calculate the cost-frontier
                (the curve depicting maximum impact for minimum cost). We then
                interpolate between interventions to identify the changes that
                must be made to care to achieve the 90-90-90 targets. The
                vertical line illustrates 73% viral suppression by 2020; the
                lines in red illustrate frontiers that achieve 73% suppression,
                while those in grey denote frontiers that do not.")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Intervention Results",
                "This page illustrates the changes that could be made to care that
                may permit the achievement of the UNAIDS 90-90-90 targets by 2020.
                We simulate a range of interventions and use interpolation across
                multiple scenarios to calculate the average changes to care. Hit
                'View Results' to view individual simulation results. Hitting
                'Next' takes you to the report generation page.",
            p(""),
            bsButton(inputId = "resetTarget",
                        label = "RESET TARGETS",
                        style = "danger",
                        block = TRUE,
                        size = "default",
                        icon = icon("bullseye", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "optData909090",
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
                bsButton(inputId = "PREV_opt909090", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_opt909090" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        ),
        p(""),
        plotOutput('plotChanges')
    )
)
