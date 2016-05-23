tabItem(tabName = "diagnosis",
    column(width = 8,
        shinyjs::useShinyjs(),
        div(img(src = "si-indicators/diagnosed.png", height = '30%', width = '30%'), style="text-align: center;"),
        br(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People Diagnosed with HIV",
            collapsible = TRUE,
            collapsed = FALSE,
            bsModal(id = "seeDataTable_DIAG", title = "Data Table", trigger = "viewData_DIAG", size = "large",
                DT::dataTableOutput('dataTable_DIAG', width = "100%")
            ),
            id = 'diag_panel',
            uiOutput(outputId = "UI_uDIAG"),
            uiOutput(outputId = "UI_uDIAG_source"),
            uiOutput(outputId = "UI_uDIAG_year")
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
            bsButton(inputId = "viewData_DIAG", label = "VIEW DATA", style = "primary", size = "default", block = TRUE),
            bsButton(inputId = "resetDIAG",     label = "RESET",     style = "danger",  size = "default", block = TRUE)
        ),
        bsAlert(anchorId = "uDIAG_ALERT_green"),
        bsAlert(anchorId = "uDIAG_ALERT_amber"),
        bsAlert(anchorId = "uDIAG_ALERT_red"),
        bsAlert(anchorId = "uDIAG_ALERT"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_diag", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_diag" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
