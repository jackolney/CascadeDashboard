tabItem(tabName = "data-review",
    column(width = 8,
        shinyjs::useShinyjs(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Cascade Data Review",
            collapsible = TRUE,
            collapsed = FALSE,
            bsModal(id = "seeDataTable_DATA", title = "Data Table", trigger = "viewData_DATA", size = "large",
                DT::dataTableOutput('dataTable_DATA', width = "100%")
            ),
            plotOutput('plotData', height = 'auto', width = 'auto')
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "CD4 Distribution in 2010",
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('plotCD4', height = 'auto', width = 'auto')
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "New Infections",
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('plotIncidence', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please review the data that will be used to calibrate
                the model. If you are happy with the data presented click
                'Calibrate' to begin model calibration.")
            # bsButton(inputId = "viewData_DATA",   label = "VIEW DATA",  style = "primary", size = "default", block = TRUE),
            # bsButton(inputId = "resetDATA",       label = "RESET",      style = "danger",  size = "default", block = TRUE)
        ),
        # box(width = NULL,
        #     status = "danger",
        #     title = "Treatment Guidelines",
        #     collapsible = TRUE,
        #     collapsed = TRUE,
        #     solidHeader = TRUE,
        #     uiOutput("UI_l200_tx"),
        #     uiOutput("UI_l250_tx"),
        #     uiOutput("UI_l350_tx"),
        #     uiOutput("UI_l500_tx"),
        #     uiOutput("UI_m500_tx"),
        #     bsButton(inputId = "resetTxGuidelines", label = "RESET", style = "danger", size = "default", block = TRUE)
        # ),
        # bsAlert(anchorId = "tx_l200_alert_l250"),
        # bsAlert(anchorId = "tx_l200_alert_l350"),
        # bsAlert(anchorId = "tx_l200_alert_l500"),
        # bsAlert(anchorId = "tx_l200_alert_m500"),
        # bsAlert(anchorId = "tx_l250_alert_l250"),
        # bsAlert(anchorId = "tx_l250_alert_l350"),
        # bsAlert(anchorId = "tx_l250_alert_l500"),
        # bsAlert(anchorId = "tx_l250_alert_m500"),
        # bsAlert(anchorId = "tx_l350_alert_l250"),
        # bsAlert(anchorId = "tx_l350_alert_l350"),
        # bsAlert(anchorId = "tx_l350_alert_l500"),
        # bsAlert(anchorId = "tx_l350_alert_m500"),
        # bsAlert(anchorId = "tx_l500_alert_l250"),
        # bsAlert(anchorId = "tx_l500_alert_l350"),
        # bsAlert(anchorId = "tx_l500_alert_l500"),
        # bsAlert(anchorId = "tx_l500_alert_m500"),
        # bsAlert(anchorId = "tx_m500_alert_l250"),
        # bsAlert(anchorId = "tx_m500_alert_l350"),
        # bsAlert(anchorId = "tx_m500_alert_l500"),
        # bsAlert(anchorId = "tx_m500_alert_m500"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_data", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "CALIB_data", label = "Calibrate", style = "success",  size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        )
    )
)
