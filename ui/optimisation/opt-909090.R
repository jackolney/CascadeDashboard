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
                    valueBox(value = scales::percent(0.9), subtitle = "Diagnosed", color = "gray", width = NULL, icon = icon("check", lib = "font-awesome"))
                ),
                column(width = 4,
                    valueBox(value = scales::percent(0.9), subtitle = "On Treatment", color = "gray", width = NULL, icon = icon("check", lib = "font-awesome"))
                ),
                column(width = 4,
                    valueBox(value = scales::percent(0.9), subtitle = "Virally Suppressed", color = "gray", width = NULL, icon = icon("check", lib = "font-awesome"))
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
            tags$div(valueBoxOutput(outputId = "vb909090_COST", width = "100%"), style = "width: 45%; margin: auto;"),
            fluidRow(
                column(width = 4,
                    infoBoxOutput(outputId = "vb909090_testing",      width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vb909090_linkage",      width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vb909090_preRetention", width = "100%")
                )
            ),
            fluidRow(
                column(width = 4,
                    infoBoxOutput(outputId = "vb909090_initiation",   width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vb909090_adherence",    width = "100%")
                ),
                column(width = 4,
                    infoBoxOutput(outputId = "vb909090_retention",    width = "100%")
                )
            ),
            tags$em("If all three targets can be achieved or exceeded by 2020, then the cheapest route to 90-90-90
                        will be displayed above. The total additional cost of care to achieve these goals between 2015
                        and 2020 will be shown above along with the changes that must be made to care. In the event
                        that all three targets are not able to be achieved through implementing interventions, then
                        the simulation producing the closest result will be displayed, along with its cost and
                        required changes to care. Boxes are color coded to illustrate the magnitude of changes to be made.
                        Green boxes illustrate the the two largest absolute changes to care, orange denotes the next
                        largest two absolute changes to care and red denotes the two smallest absolute changes to care.")
        ),
        box(width = NULL,
            status = "warning",
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
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Results",
            "This page shows the quickest route to achieving the UNAIDS 90-90-90 targets by 2020.
            For each goal that can be achieved, the box will be green and a tick will be displayed.
            Any goals that fail to be achieved will be denoted by a cross. Hit 'View Results' to
            view the selected simulation and othe nearby solutions. Hitting 'Next' takes you to the
            report generation page.",
            p(""),
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
        )
    )
)
