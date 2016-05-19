tabItem(tabName = "setup",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Model Setup",
            collapsible = TRUE,
            collapsed = FALSE,
            shinyjs::useShinyjs(),
            id = "setup-panel",
            helpText("Select country and fill in boxes to specify the initial values of the model.
                The boxes correlate with indicators in the Consolidated Information Guidelines (shown below), with numbers in brackets corresponding to numbers on the indicator figure."),
            img(src = "WHOGuidelinesCascade.png", height = '100%', width = '100%')
        ),
        fluidRow(
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                selectInput("userCountry", "Country:", CountryList, selected = "Brazil")
            ),
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                numericInput("userPLHIV","Number of PLHIV (1):", value = 1e+6, min = 0, width = '100%')
            )
        ),
        fluidRow(
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                numericInput("userDx","Number of PLHIV who have been diagnosed (4):", value = 0, min = 0, width = '100%')
            ),
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                numericInput("userCare","Number of PLHIV in HIV care (including ART) (5):", value = 0, min = 0)
            )
        ),
        fluidRow(
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                numericInput("userTx","Number of PLHIV in HIV care and on ART (6):", value = 0, min = 0)
            ),
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                numericInput("userVs","Number of PLHIV in HIV care, on ART and virally suppressed (8):", value = 0, min = 0)
            )
        ),
        fluidRow(
            box(width = 6,
                height = '100%',
                background = "yellow",
                solidHeader = TRUE,
                numericInput("userLtfu","Number of PLHIV who dropped out of ART care:", value = 0, min = 0)
            ),
            # em(h5("If this value is known please enter it, otherwise leave it blank:")),
            box(width = 6,
                height = '100%',
                background = "orange",
                solidHeader = TRUE,
                numericInput("userRetArt12mths","Percentage of PLHIV retained and on ART after 12 months (7):", value = 0, min = 0, max = 1, step = 0.01)
            )
        ),
        box(width = NULL,
                background = "yellow",
                solidHeader = TRUE,
                h4("ART Initiation Threshold"),
                p("Please check the box corresponding to the correct treatment threshold. Boxes are reactive and nearby boxes will adjust to the selection made. Please note that unchecking all boxes will result in ART being witheld for all individuals."),
                checkboxInput("userART_All", "Immediate ART", value = TRUE),
                checkboxInput("userART_500", "CD4 <500", value = TRUE),
                checkboxInput("userART_350", "CD4 <350", value = TRUE),
                checkboxInput("userART_200", "CD4 <200", value = TRUE)
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please fill in all boxes with relevant data, then hit 'SAVE' and wait for the confirmation below.
                Hit 'RESET' to reset all values to zero, and hit 'DEMO' for a random set of values to be generated.
                Unchecking the 'HIV incidence' checkbox prevents any new infections occurring in the model."),
            checkboxInput("incidenceInput","HIV Incidence", value = TRUE, width = "100%"),
            bsButton("saveInput", label = "SAVE", style = "success", block = TRUE, size = "default"),
            p(" "),
            bsButton("resetInput", label = "RESET", style = "danger", block = TRUE, size = "default"),
            p(" "),
            bsButton("demoInput", label = "DEMO", style = "primary", block = TRUE, size = "default"),
            bsTooltip(id = "demoInput", title = "Populate model with best estimates from Kenya.", placement = "left", trigger = "hover"),
            p(" "),
            helpText("Console output:"),
            textOutput('saveText'),
            textOutput('warningText'),
            textOutput('warningCD4Text')
        ),
        bsButton(inputId = "wizardCalibration", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
