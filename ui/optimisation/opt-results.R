InterventionList <- c("Rho","Q","Kappa","Gamma","Sigma","Omega")

tabItem(tabName = "opt-results"
    # column(width = 8,
    #     bsModal(id = "opt909090TableModal", title = "Result Table (showing 90-90-90 targets)", trigger = "showOpt909090Table", size = "large",
    #         DT::dataTableOutput('opt909090Table', width = "100%")
    #     ),
    #     bsModal(id = "opt909090TableBrushedModal", title = "Selected Result Table (showing 90-90-90 targets)", trigger = "showOpt909090BrushedTable", size = "large",
    #         DT::dataTableOutput('opt909090TableBrushed', width = "100%")
    #     ),
    #     bsModal(id = "optDALYsTableModal", title = "Result Table", trigger = "showOptDALYsTable", size = "large",
    #         DT::dataTableOutput('optDALYsTable', width = "100%")
    #     ),
    #     bsModal(id = "optDALYsTableBrushedModal", title = "Selected Result Table", trigger = "showOptDALYsBrushedTable", size = "large",
    #         DT::dataTableOutput('optDALYsTableBrushed', width = "100%")
    #     ),
    #     bsModal(id = "optDALYs909090TableModal", title = "Result Table (results achieving 90-90-90 targets)", trigger = "showOptDALYs909090Table", size = "large",
    #         DT::dataTableOutput('optDALYs909090Table', width = "100%")
    #     ),
    #     bsModal(id = "optDALYs909090TableBrushedModal", title = "Selected Result Table (results achieving 90-90-90 targets)", trigger = "showOptDALYs909090BrushedTable", size = "large",
    #         DT::dataTableOutput('optDALYs909090TableBrushed', width = "100%")
    #     ),
    #     bsCollapse(id = 'optCollapse', open = NULL,
    #         bsCollapsePanel("Plot 90-90-90",
    #             plotOutput('plotOpt909090',
    #                 dblclick = "plotOpt909090_dblclick",
    #                 brush = brushOpts(
    #                     id = "plotOpt909090_brush",
    #                     clip = TRUE,
    #                     resetOnNew = TRUE
    #                     )
    #                 ),
    #             style = "success"
    #         ),
    #         bsCollapsePanel("Plot DALYs",
    #             plotOutput('plotOptDALYs',
    #                 dblclick = "plotOptDALYs_dblclick",
    #                 brush = brushOpts(
    #                     id = "plotOptDALYs_brush",
    #                     clip = TRUE,
    #                     resetOnNew = TRUE
    #                     )
    #                 ),
    #             style = "danger"
    #         ),
    #         bsCollapsePanel("Plot DALYs (90-90-90)",
    #             plotOutput('plotOptDALYs909090',
    #                 dblclick = "plotOptDALYs909090_dblclick",
    #                 brush = brushOpts(
    #                     id = "plotOptDALYs909090_brush",
    #                     clip = TRUE,
    #                     resetOnNew = TRUE
    #                     )
    #                 ),
    #             style = "info"
    #         )
    #     )
    # ),
    # column(width = 4,
    #     box(width = NULL,
    #         status = "primary",
    #         solidHeader = TRUE,
    #         title = "Results",
    #         helpText("This section is still under active development."),
    #         p("The results of the optimisation simulation are shown in the plot to the right. Hit 'Show Result Table' to view all data points and corresponding parameter values.
    #             Zoom in on data points by drawing a box on the plot with the mouse and double clicking. To view the details of a specific point, draw a box with the mouse over the point and
    #             hit 'Show Selected Result Table'"),
    #         p(" "),
    #         selectInput("userStratPoint","Select parameter to stratify results by:", InterventionList, selected = "Rho"),
    #         p(" "),
    #         tags$b("Viral suppression against cost:"),
    #         p(" "),
    #         bsButton("showOpt909090Plot", label = "", style = "success", block = TRUE, size = "large", icon = icon("bar-chart", class = "fa-2x fa-fw", lib = "font-awesome")),
    #         p(" "),
    #         bsButton("showOpt909090Table", label = "Show Result Table", style = "primary", block = TRUE),
    #         p(" "),
    #         bsButton("showOpt909090BrushedTable", label = "Show Selected Result Table", style = "primary", block = TRUE),
    #         p(" "), br(),
    #         tags$b("DALYs averted against cost:"),
    #         p(" "),
    #         bsButton("showOptDALYsPlot", label = "", style = "danger", block = TRUE, size = "large", icon = icon("bar-chart", class = "fa-2x fa-fw", lib = "font-awesome")),
    #         p(" "),
    #         bsButton("showOptDALYsTable", label = "Show Result Table", style = "primary", block = TRUE),
    #         p(" "),
    #         bsButton("showOptDALYsBrushedTable", label = "Show Selected Result Table", style = "primary", block = TRUE),
    #         p(" "), br(),
    #         tags$b("DALYs averted against cost (for subset achieving 90-90-90):"),
    #         p(" "),
    #         bsButton("showOptDALYs909090Plot", label = "", style = "info", block = TRUE, size = "large", icon = icon("bar-chart", class = "fa-2x fa-fw", lib = "font-awesome")),
    #         p(" "),
    #         bsButton("showOptDALYs909090Table", label = "Show Result Table", style = "primary", block = TRUE),
    #         p(" "),
    #         bsButton("showOptDALYs909090BrushedTable", label = "Show Selected Result Table", style = "primary", block = TRUE)
    #     ),
    #     bsButton(inputId = "wizardOpt_4", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    # )
)
