tabItem(tabName = "powers_cascade",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Power's Cascade",
            plotOutput('plotPowersCascade')
        )
    ),
    column(width = 4,
        box(width = NULL,
            background = "yellow",
            solidHeader = TRUE,
            title = "The distribution of care between 2015 and 2020.",
            "These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020).",
            "Note, this figure is identical to the one found on the previous page,
                except here we illustrate the discrete stages of care. Figures are based on those found in Powers et al. (2015).",
            tags$ol(
                tags$li("# persons undiagnosed"),
                tags$li("# persons diagnosed and not in care"),
                tags$li("# persons diagnosed, in care, not on ART"),
                tags$li("# persons diagnosed, in care, on ART, but not adhering and not virally suppressed"),
                tags$li("# persons diagnosed, in care, on ART, adhering, but not virally suppressed"),
                tags$li("# persons diagnosed, in care, on ART, virally suppressed"),
                tags$li("# persons diagnosed, not in care, dropped out of ART.")
            )
        ),
        bsButton(inputId = "NEXT_powersCascade", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
