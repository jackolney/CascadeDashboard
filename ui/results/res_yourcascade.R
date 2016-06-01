tabItem(tabName = "your_cascade",
    column(width = 8,
        # h1(textOutput('CountryName')),
        tabBox(
            width = NULL,
            height = "600px",
            selected = "PLHIV",
            side = "left",

            tabPanel(title = "PLHIV",
                tags$b("Mean number of PLHIV:"),
                uiOutput("outPLHIV"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outPLHIV_perc"),
                p(""),
                plotOutput('plotValidation_PLHIV',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Diagnosed",
                tags$b("Mean of PLHIV who have been diagnosed:"),
                uiOutput("outDIAG"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outDIAG_perc"),
                p(""),
                plotOutput('plotValidation_DIAG',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Care",
                tags$b("Mean of PLHIV in HIV care (including ART):"),
                uiOutput("outCARE"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outCARE_perc"),
                p(""),
                plotOutput('plotValidation_CARE',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Treatment",
                tags$b("Mean of PLHIV in HIV care and on ART:"),
                uiOutput("outART"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outART_perc"),
                p(""),
                plotOutput('plotValidation_ART',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Suppression",
                tags$b("Mean of PLHIV in HIV care, on ART and virally suppressed:"),
                uiOutput("outSUPP"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outSUPP_perc"),
                p(""),
                plotOutput('plotValidation_SUPP',
                    height = 'auto',
                    width = 'auto')
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Care Cascade",
            "This page shows the breakdown of the care cascade in 2015, resulting from the calibration
            of the model on the previous page. Each tab will explain the values behind each bar of the cascade,
            including the range of plausible values arising from calibration.
            Clicking 'Next' will advance to the next page."
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_yourCascade", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_yourCascade" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
