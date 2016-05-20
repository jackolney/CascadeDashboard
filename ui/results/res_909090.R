tabItem(tabName = "_909090",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "UNAIDS 90-90-90 by 2020",
            plotOutput('plot909090', height = "500px")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "UNAIDS 90-90-90",
            "By 2020, this is what the model predicts will be achieved in comparison to the UNAIDS goals of 90% diagnosed,
                90% on treatment and 90% virally suppressed. If you would like to see what changes can be made to resolve any
                inefficiencies in care, then click on the 'Optimisation' tab."
        ),
        valueBoxOutput("vb_90",     width = NULL),
        valueBoxOutput("vb_9090",   width = NULL),
        valueBoxOutput("vb_909090", width = NULL),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_909090", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_909090" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
