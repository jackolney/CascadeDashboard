dashboardSidebar(
    sideBarMini = TRUE,
    sidebarMenu(
        id = "sideBar",
        menuItem("Introduction", tabName = "introduction", icon = icon("home", class = "fa-lg fa-fw", lib = "font-awesome")),
        menuItem("Country", icon = icon("globe", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Country",                tabName = "country"),
            menuSubItem("Edit Cascade",           tabName = "edit-cascade"),
            menuSubItem("Edit CD4",               tabName = "edit-cd4"),
            menuSubItem("Edit Incidence",         tabName = "edit-incidence"),
            menuSubItem("Edit Guidelines",        tabName = "edit-guidelines")
        ),
        menuItem("Setup", icon = icon("cogs", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Data Review",            tabName = "data-review"),
            menuSubItem("Calibration",            tabName = "calibration"),
            menuSubItem("Parameters",             tabName = "parameters")
        ),
        menuItem("Results", icon = icon("line-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("The Care Cascade",       tabName = "your_cascade"),
            menuSubItem("Cascade Projection",     tabName = "care_cascade"),
            menuSubItem("The Power's Cascade",    tabName = "powers_cascade"),
            menuSubItem("UNAIDS 90-90-90",        tabName = "_909090"),
            menuSubItem("Incidence",              tabName = "incidence"),
            menuSubItem("Mortality",              tabName = "mortality")
        ),
        menuItem("Optimisation", icon = icon("pie-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Introduction",           tabName = "opt-intro"),
            menuSubItem("Intervention Detail",    tabName = "opt-parameter"),
            menuSubItem("Intervention Cost",      tabName = "opt-cost"),
            menuSubItem("'Best Fit'",             tabName = "opt-best-fit"),
            menuSubItem("UNAIDS 90-90-90",        tabName = "opt-909090")
        ),
        menuItem("More", icon = icon("question", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Create Report",          tabName = "report"),
            menuSubItem("Model Document",         tabName = "model_document"),
            menuSubItem("Diagnostics",            tabName = "session"),
            menuSubItem("Single Plot",            tabName = "single_plot"),
            menuSubItem("All Plots",              tabName = "all_plots")
        )
    )
)
