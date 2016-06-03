tabItem(tabName = "opt-909090",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "UNAIDS 90-90-90",
            # Allow the below to be dynamic: if they can be achieved, make them have a tick, else a cross.
            fluidRow(
                column(width = 4,
                    valueBox(scales::percent(0.9), "Diagnosed", color = 'red',icon = icon("check"), width = NULL)
                ),
                column(width = 4,
                    valueBox(scales::percent(0.9), "On Treatment", color = 'orange',icon = icon("check"), width = NULL)
                ),
                column(width = 4,
                    valueBox(scales::percent(0.9), "Virally Suppressed", color = 'green',icon = icon("check"), width = NULL)
                )
            )
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "How to get there?",
            # Need an algorithm that will identify the cheapest strategy to achieving the 90-90-90,
            # BUT in the event that it cant be done, we trigger the crosses above, AND then pick the route that gets us CLOSEST (absolute)
            tags$div(
                valueBox(scales::dollar(10), "Cost", color = "green", icon = icon("usd"), width = NULL),
                style = "width: 50%; margin: auto;"
            ),
            fluidRow(
                column(width = 4,
                    infoBox(title = "Linkage", 10 * 2, color = "orange", subtitle = "hello hello hello", width = NULL, fill = TRUE)
                ),
                column(width = 4,
                    infoBox(title = "Linkage", 10 * 2, color = "orange", subtitle = "hello hello hello", width = NULL, fill = TRUE)
                ),
                column(width = 4,
                    infoBox(title = "Linkage", 10 * 2, color = "orange", subtitle = "hello hello hello", width = NULL, fill = TRUE)
                )
            ),
            fluidRow(
                column(width = 4,
                    infoBox(title = "Linkage", 10 * 2, color = "orange", subtitle = "hello hello hello", width = NULL, fill = TRUE)
                ),
                column(width = 4,
                    infoBox(title = "Linkage", 10 * 2, color = "orange", subtitle = "hello hello hello", width = NULL, fill = TRUE)
                ),
                column(width = 4,
                    infoBox(title = "Linkage", 10 * 2, color = "orange", subtitle = "hello hello hello", width = NULL, fill = TRUE)
                )
            ),
            # NEED A DATATABLE HERE SHOWING THE SAME AS BEFORE BUT IN THE HOW DO WE GET TO 90-90-90 SENSE.
            # What about info boxes instead?
            "The results of the optimisation indicate thousands of potential ways to improve care,
            either using interventions individually or in combination. Our simulations find that in
            order to achieve the level of viral suppression selected by the slider (right) by 2020,
            then over the next five years on average a number of changes must occur,
            these changes are described below:",
            p(""),
            tags$em("Please note that values below may indicate decreases in some aspects of care,
                this is correct, and explained by specific changes in care having an in-direct impact
                on reducing incidence, thereby reducing the total population of infected individuals.")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Control",
            "This is the main intervention page, where all previously selected interventions are
            simulated and results presented.",
            p(""),
            bsButton(inputId = "optData",
                        label = "View Results",
                        type = "action",
                        style = "primary",
                        size = "default",
                        block = TRUE,
                        icon = icon("database", class = "fa-lg fa-fw", lib = "font-awesome"))
        ),
        bsAlert(anchorId = "opt_VS_cutoff_alert"),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_opt909090", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_opt909090" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
