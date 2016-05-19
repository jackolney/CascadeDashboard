tabItem(tabName = "report",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = tags$span(tagList(icon("file-pdf-o", class = "fa-lg fa-fw", lib = "font-awesome"), "Report Generation")),
            "Generate a report detailing all data entered into the site, all simulations run, and all results generated.
            The report is a pdf compiled using LaTeX, and contains all important information regarding the model. Please contact me with any bugs or issues at:",
            HTML('<a href="mailto:jack.olney11@imperial.ac.uk">jack.olney11@imperial.ac.uk</a>'),
            img(src = "report-sample.png", height = '100%', width = '100%')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Caution: report generation can take up to a minute."),
            "Click 'Create Report' to generate and download a pdf report from your simulations."
        ),
        downloadButton(outputId = 'downloadReport', label = "CREATE REPORT", class = "btn btn-success btn-lg btn-block")
    )
)
