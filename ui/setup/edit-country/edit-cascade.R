tabItem(tabName = "edit-cascade",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit Cascade",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot_cascade")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please enter any available data on this page relating to the cascade.
                If data are missing, leave the cells blank and they will be ignored by the model.
                Expand and consult the table below for information regarding how to weight each data point.
                Click 'Back' to return to the previous page"),
            h5("Conditions that must be satisfied for indicator to turn green:"),
            tags$ul(
                tags$li("Value must be entered in 2010 for 'PLHIV'"),
                tags$li("Value must be entered in 2010 for 'PLHIV on ART'"),
                tags$li("All values entered must be accompanied by a weight")
            ),
            a(href = "http://who.int/hiv/pub/guidelines/strategic-information-guidelines/en/", "WHO - Consolidated Strategic Information Guidelines for HIV in the Health Sector", target = "_blank")
        ),
        box(width = NULL,
            status = "danger",
            title = "Data Weight Table",
            collapsible = TRUE,
            collapsed = TRUE,
            solidHeader = TRUE,
            tableOutput('weightTable')
        ),
        bsButton(inputId = "PREV_editCascade", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome")),
        plotOutput('editCascadePlot', height = 'auto', width = 'auto')
    )
)
