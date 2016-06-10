tabItem(tabName = "_909090",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "UNAIDS 90-90-90 by 2020",
            plotOutput('plot909090', height = "500px")
        ),
        valueBoxOutput("vb_90",     width = 4),
        valueBoxOutput("vb_9090",   width = 4),
        valueBoxOutput("vb_909090", width = 4)
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "UNAIDS 90-90-90",
            "From the predicted distribution of care in 2020, we can visualise the achievement of the
            UNAIDS 90-90-90 targets. In 2014, UNAIDS set the target of 90% diagnosis, 90% treatment
            coverage and 90% viral suppression to be achieved by 2020. The 90% goal is illustrated by
            the horizontal line in the figure. The values of each bar are also shown in the coloured
            boxes below. Click 'Next' to continue."
        ),
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
