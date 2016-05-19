tabItem(tabName = "opt-budget",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            DT::dataTableOutput('optBudgetTable', width = "100%")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Budget",
            helpText("Please enter a health care budget value in the box below to see a subset of results that do not exceed that value."),
            numericInput("userBudget","Enter budget to subset results (2013 USD):", value = 0, min = 0, step = 1e+04),
            tags$b("Select output as 90-90-90 indicators or DALYs:"),
            p(" "),
            bsButton("showBudget909090", label = "90-90-90", style = "success", block = TRUE, size = "large"),
            p(" "),
            bsButton("showBudgetDALYs", label = "DALYs", style = "danger", block = TRUE, size = "large")
        )
    )
)
