tabItem(tabName = "plhiv",
    column(width = 8,
        shinyjs::useShinyjs(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People Living with HIV",
            collapsible = TRUE,
            collapsed = FALSE,
            # background = "blue",
            div(img(src = "si-indicators/plhiv.png", height = '30%', width = '30%'), style = "text-align: center;"),
            br(),
            bsModal(id = "seeDataTable_PLHIV", title = "Data Table", trigger = "viewData_PLHIV", size = "large",
                DT::dataTableOutput('dataTable_PLHIV', width = "100%")
            ),
            id = 'plhiv_panel',
            uiOutput(outputId = "UI_uPLHIV"),
            uiOutput(outputId = "UI_uPLHIV_source"),
            uiOutput(outputId = "UI_uPLHIV_year")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please fill in the boxes with details regarding each of the strategic information indicators,
                then select the source of the data from the drop-down menu below.
                Once entered hit 'Next' to proceed. For further details please see:"),
            a(href = "http://who.int/hiv/pub/guidelines/strategic-information-guidelines/en/", "WHO - Consolidated Strategic Information Guidelines for HIV in the Health Sector.", target = "_blank"),
            p(""),
            bsButton(inputId = "viewData_PLHIV",   label = "VIEW DATA", style = "primary", size = "default", block = TRUE),
            bsButton(inputId = "resetPLHIV", label = "RESET",     style = "danger",  size = "default", block = TRUE)
        ),
        bsAlert(anchorId = "uPLHIV_ALERT_green"),
        bsAlert(anchorId = "uPLHIV_ALERT_amber"),
        bsAlert(anchorId = "uPLHIV_ALERT_red"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_plhiv", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_plhiv" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
