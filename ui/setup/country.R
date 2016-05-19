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
                selected = "Brazil")
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
        # Perhaps a collapsible 'master check' warning panel, like at mission control. Call it "mission control".
        # It will contain a narrow and tall list of all the imported csv files, and will have deactivated buttons that will change colour depending on what gets loaded.
        # When data loaded, the icon changes?
        # SUPER AWESOME.
        box(width = NULL,
            status = "danger",
            # background = "black",
            title = "Mission Control",
            collapsible = TRUE,
            collapsed = FALSE,
            solidHeader = TRUE,
            bsButton(inputId = "_Incidence_FLAG_",  label = "Incidence (Spectrum)",        style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_CD4_FLAG_",        label = "CD4 Distribution (Spectrum)", style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_Treatment_FLAG_",  label = "Treatment Guidelines",        style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_PLHIV_FLAG_",      label = "PLHIV Estimates (Spectrum)",  style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_ART_FLAG_",        label = "ART Estimates (Spectrum)",    style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_Additional_FLAG_", label = "Additional Estimates",        style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_Rates_FLAG_",      label = "Rates",                       style = "danger", size = "small", block = TRUE, disabled = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "_PROCEED_",         label = "DO NOT PROCEED",              style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
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
