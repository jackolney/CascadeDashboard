tabItem(tabName = "data-review",
    column(width = 8,
        shinyjs::useShinyjs(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Data Review",
            collapsible = TRUE,
            collapsed = FALSE,
            "This page will have a plot (here) that illustrates the currently loaded data in the model,
            from (MasterData). The user can review it and pull down a modal with all the values printed.
            The user will then be able to click either 'ENTER DATA' or 'CALIBRATE' and be taken to the
            relevant pages.",
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
