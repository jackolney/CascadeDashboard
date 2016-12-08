tabItem(tabName = "suppression",
    column(width = 8,
        shinyjs::useShinyjs(),
        div(img(src = "si-indicators/suppression.png", height = '30%', width = '30%'), style="text-align: center;"),
        br(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People Virally Suppressed on ART",
            collapsible = TRUE,
            collapsed = FALSE,
            bsModal(id = "seeDataTable_VIRAL", title = "Data Table", trigger = "viewData_VIRAL", size = "large",
                DT::dataTableOutput('dataTable_VIRAL', width = "100%")
            ),
            id = 'viral_panel',
            uiOutput(outputId = "UI_uVIRAL"),
            uiOutput(outputId = "UI_uVIRAL_source"),
            uiOutput(outputId = "UI_uVIRAL_year")
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
            bsButton(inputId = "viewData_VIRAL", label = "VIEW DATA", style = "primary", size = "default", block = TRUE),
            bsButton(inputId = "resetVIRAL",     label = "RESET",     style = "danger",  size = "default", block = TRUE)
        ),
        bsAlert(anchorId = "uVIRAL_ALERT_green"),
        bsAlert(anchorId = "uVIRAL_ALERT_amber"),
        bsAlert(anchorId = "uVIRAL_ALERT_red"),
        bsAlert(anchorId = "uVIRAL_ALERT"),
        uiOutput(outputId = "UI_uVIRAL_national"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_viral", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_viral" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
