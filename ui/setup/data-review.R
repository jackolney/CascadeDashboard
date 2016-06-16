tabItem(tabName = "data-review",
    column(width = 8,
        shinyjs::useShinyjs(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Data Review",
            collapsible = TRUE,
            collapsed = FALSE,
            bsModal(id = "seeDataTable_DATA", title = "Data Table", trigger = "viewData_DATA", size = "large",
                DT::dataTableOutput('dataTable_DATA', width = "100%")
            ),
            plotOutput('plotData', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please review the data that will be used to calibrate the model.
                This data has already been pre-loaded from various sources. Hit 'VIEW DATA',
                to view each data point in detail. If you have additional data click 'Enter Data' to
                begin adding new data for calibration, but if you are happy with the data presented
                click 'Calibrate' to begin model calibration."),
            bsButton(inputId = "viewData_DATA",   label = "VIEW DATA", style = "primary", size = "default", block = TRUE)
        ),
        box(width = NULL,
            status = "danger",
            title = "Treatment Guidelines",
            collapsible = TRUE,
            collapsed = TRUE,
            solidHeader = TRUE,
            uiOutput("UI_l200_tx"),
            uiOutput("UI_l250_tx"),
            uiOutput("UI_l350_tx"),
            uiOutput("UI_l500_tx"),
            uiOutput("UI_m500_tx"),
            bsButton(inputId = "resetTxGuidelines", label = "RESET", style = "danger", size = "default", block = TRUE)
        ),
        bsAlert(anchorId = "tx_l200_alert_l250"),
        bsAlert(anchorId = "tx_l200_alert_l350"),
        bsAlert(anchorId = "tx_l200_alert_l500"),
        bsAlert(anchorId = "tx_l200_alert_m500"),
        bsAlert(anchorId = "tx_l250_alert_l250"),
        bsAlert(anchorId = "tx_l250_alert_l350"),
        bsAlert(anchorId = "tx_l250_alert_l500"),
        bsAlert(anchorId = "tx_l250_alert_m500"),
        bsAlert(anchorId = "tx_l350_alert_l250"),
        bsAlert(anchorId = "tx_l350_alert_l350"),
        bsAlert(anchorId = "tx_l350_alert_l500"),
        bsAlert(anchorId = "tx_l350_alert_m500"),
        bsAlert(anchorId = "tx_l500_alert_l250"),
        bsAlert(anchorId = "tx_l500_alert_l350"),
        bsAlert(anchorId = "tx_l500_alert_l500"),
        bsAlert(anchorId = "tx_l500_alert_m500"),
        bsAlert(anchorId = "tx_m500_alert_l250"),
        bsAlert(anchorId = "tx_m500_alert_l350"),
        bsAlert(anchorId = "tx_m500_alert_l500"),
        bsAlert(anchorId = "tx_m500_alert_m500"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "NEXT_data", label = "Enter Data", style = "danger",  size = "large", block = TRUE, icon = icon("database",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "CALIB_data", label = "Calibrate", style = "success",  size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        )
    )
)
