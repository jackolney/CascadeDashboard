tabItem(tabName = "edit-cd4",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit CD4 Distribution in 2010",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot_cd4")
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Edit CD4 Distribution in 2015",
            collapsible = TRUE,
            collapsed = FALSE,
            rHandsontableOutput("hot_cd4_2015")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please enter any available data on this page relating to distribution of CD4
                counts in among infected persons in 2010. Values must be entered in every cell.
                The check function on the previous page will not turn green until values for 'off' and
                'on' ART distributions each sum to one.
                Click 'Back' to return to the previous page"),
            tags$div(checkboxInput(inputId = "copy2010CD4", label = "Use 2010 CD4 distribution in 2015?", value = FALSE, width = NULL), style = "width: 60%; margin: auto; font-weight: bold; font-size: 110%;"),
            h5("Conditions that must be satisfied for indicator to turn green:"),
            tags$ul(
                tags$li("A value must be entered in every cell"),
                tags$li("All values across 'Off ART' must sum to 1"),
                tags$li("All values across 'On ART' must sum to 1")
            )
        ),
        bsButton(inputId = "PREV_editCD4", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome")),
        p(""),
        plotOutput('editCD4Plot',     height = 'auto', width = 'auto'),
        plotOutput('editCD42015Plot', height = 'auto', width = 'auto')
    )
)
