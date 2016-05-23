tabItem(tabName = "introduction",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Introduction",
            p("With the release of the Consolidated Information Guidelines for HIV by the World Health Organization (WHO)
            in May 2015, a set of indicators have been agreed upon, based on the cascade of HIV services relating to impact in
            terms of HIV incidence and mortality (see below). These guidelines provide a framework for countries to assess the current state
            of care and identify any immediate deficiencies and bottlenecks preventing patients from progressing to treatment."),
            p("Furthermore, as we pass the Millennium Development Goals of 2015 and focus attention on the UNAIDS 90-90-90 targets for 2020,
            countries will be keen to understand whether they are on the right trajectory to achieve these goals. For this purpose, data from
            countries can be input into a mathematical model and used to estimate future incidence, AIDS-deaths and the ascertainment of the 90-90-90 goals."),
            img(src = "WHO-Guidelines-Front-Crop.png", height = '100%', width = '100%')
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Aims",
            collapsible = TRUE,
            collapsed = TRUE,
            p("This webpage contains an interactive model that allows data to be entered, parameters to be altered and results to be presented in real-time.
                No specialist software is required as all calculations are completed on a remote server, results are then returned and displayed, along with all visualisations, in the browser."),
            p("The overarching aims of this model are to:"),
            tags$ol(
                tags$li("Provide a simple intuitive tool to document and analyse the current state of HIV care. This mathematical model is able to handle a wide range
                    of data, characterise individuals into discrete care categories, and by building upon a set of assumptions regarding HIV-transmission, duration of
                    infection and progression through care and death, the model can project changes in incidence, mortality and care until 2020."),
                tags$li("The model is country-specific. During setup, when a new country is selected from the drop down list,
                    the model adjusts its incidence estimates based on data from the Spectrum software used by UNAIDS."),
                tags$li("The model can be used to assess the current state of care and ascertain gaps that require immediate attention. Furthermore, the model can identify what
                    future changes need to be made, that differ from what countries have done so far, to be on track to achieve the 90-90-90 goals set out by UNAIDS."),
                tags$li("Non-specific interventions can be simulated that broadly illustrate the changes that can be made to care, along with with the costs of doing so,
                    to identify the most cost-effective strategy for reconciling any deficiencies in care (still in development)."),
                tags$li("It is hoped that this model will help countries prioritise strategies and estimate the costs required to achieve future targets.")
            )
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "The Model",
            collapsible = TRUE,
            collapsed = TRUE,
            p("The simplified structure of the model is shown below. State compartments do not exactly correlate with the indicators in Consolidated Indicator Guidelines,
                as compartments in the model must be discrete and exhaustive, while the indicators listed are not all discrete; for example, 'Knowing HIV status' includes all patients who are in care,
                on treatment, virally suppressed and lost from care as long as they are diagnosed. However, the model is able to reconcile this by taking individual indicators and separating them into their
                components to specify the initial conditions for simulations."),
            helpText("More details on the model can be found in the following pages, along with a detailed description under the 'more' table and 'Model Document'."),
            img(src = "ModelSimple.png", height = '100%', width = '100%'),
            h3("Outcomes"),
            tags$ol(
                tags$li("Predict achievement of UNAIDS 90-90-90 targets in 2020."),
                tags$li("Identify strategies to achieve 90-90-90 targets at minimal cost."),
                tags$li("Illustrate the change in distribution of care between 2015 and 2020."),
                tags$li("Predict changes in new infections and AIDS deaths between 2015 and 2020.")
                )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            # background = "yellow",
            solidHeader = TRUE,
            title = "Quick Start",
            helpText("If you want to skip the introduction and get modelling, jump to 'setup' and click on 'DEMO'.
                Data entered can be saved by clicking 'SAVE' on any relevant page. A pdf containing details about
                the entire model can be found in the 'more' tab along with links to spreadsheets containing data used in the model."),
            h5("Contributors"),
            tags$i("Jack J Olney, Jeffrey W Eaton, Ellen McRobie & Timothy B Hallett")
        ),
        bsAlert(anchorId = "startAlert"),
        bsButton(inputId = "NEXT_intro", label = "Start Wizard", style = "success", size = "large", block = TRUE, icon = icon("magic", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
