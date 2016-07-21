tabItem(tabName = "country",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Map",
            collapsible = TRUE,
            collapsed = FALSE,
            leafletOutput("countryMap", width = "100%", height = 500)
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Select Country",
            selectInput("selectCountry",
                label = NULL,
                choices = CountryList,
                selected = "Brazil"),
            bsButton(inputId = "NEW_country",
                label = "New Country / Region",
                style = "success",
                type = "toggle",
                value = FALSE,
                size = "small",
                block = TRUE,
                icon = icon("plus", class = "fa-lg fa-fw", lib = "font-awesome")),
            conditionalPanel(
                condition = "NEW_country.className == 'btn sbs-toggle-button btn-success btn-sm btn-block shiny-bound-input active'",
                textInput(inputId = "new_country_name", label = "", value = "", width = NULL, placeholder = "Enter name...")
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please click a country on the map, or select one from the drop-down menu below. Hit 'Reset Map' to reset map zoom. 'Mission Control' provides additional information on available data from each country."),
            bsButton("resetMap", label = "RESET MAP", style = "danger", block = TRUE, size = "default")
        ),
        box(width = NULL,
            status = "danger",
            title = "Mission Control",
            collapsible = TRUE,
            collapsed = FALSE,
            solidHeader = TRUE,
            bsButton(inputId = "CASCADE_FLAG",     label = "Cascade Estimates",    style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "CD4_FLAG",         label = "CD4 Distribution",     style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "INCIDENCE_FLAG",   label = "Incidence",            style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "GUIDELINES_FLAG",  label = "Treatment Guidelines", style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        ),
        bsAlert(anchorId = "_PROCEED_"),
        bsAlert(anchorId = "_DONOTPROCEED_"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_country", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_country" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
