tabItem(tabName = "session",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Session Info",
            verbatimTextOutput("console")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            "This is a diagnostic zone.
            Users are not required to access this area."
        )
    )
)
