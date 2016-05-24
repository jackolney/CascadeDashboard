tabItem(tabName = "introduction",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Introduction",
            "With the release of the Consolidated Information Guidelines for HIV by the World Health Organization (WHO)
            in May 2015, a set of indicators have been agreed upon, based on the cascade of HIV services relating to impact in
            terms of HIV incidence and mortality (see below). These guidelines provide a framework for countries to assess the current state
            of care and identify any immediate deficiencies and bottlenecks preventing patients from progressing to treatment.",
            "Furthermore, as we pass the Millennium Development Goals of 2015 and focus attention on the UNAIDS 90-90-90 targets for 2020,
            countries will be keen to understand whether they are on the right trajectory to achieve these goals. For this purpose, data from
            countries can be input into a mathematical model and used to estimate future incidence, AIDS-deaths and the ascertainment of the 90-90-90 goals.",
            p(""),
            img(src = "WHO-Guidelines-Front-Crop.png", height = '100%', width = '100%')
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Aims",
            collapsible = TRUE,
            collapsed = TRUE,
            "This site contains an interactive model that allows data on the cascade to be entered,
            model parameters to be adjusted, results to be generated in real-time. parameters to be
            altered and results to be presented in real-time.
                No specialist software is required as all calculations and simulations are completed
                on a remote server, results are then returned and displayed, along with all visualisations,
                in the browser.",
            "The aims of this model are to:",
            tags$ol(
                tags$li("Provide a simple intuitive tool to document and analyse the current state of HIV care.
                    This tool utilises a mathematical model to characterise individuals into discrete care categories,
                    and by building upon a set of assumptions regarding HIV-transmission, duration of
                    infection and progression through care and death, can project changes in incidence, mortality and care until 2020."),
                tags$li("The model is country-specific and can be used to generate national estimates of care cascade for over 30 countries.
                    During setup, when a new country is selected, the model loads a country-specific
                    data set containing incidence estimates and treatment guidelines, these data can
                    then be updated by the user before the model automatically calibrates to all available
                    data on the cascade to produce estimates of changes in the trajectory of care until 2020."),
                tags$li("The model can be used to assess the current state of care, estimate changes
                    over time, and identify any gaps that require attention. Furthermore, the model
                    can illustrate what changes need to be made, that differ from what countries have
                    done before, to remain on-track to achieve the UNAIDS 90-90-90 goals by 2020."),
                tags$li("Non-specific interventions can be simulated that broadly illustrate the changes
                    countries can make to care, along with with the costs of doing so, to show the
                    most cost-effective strategies for reconciling deficiencies in care."),
                tags$li("It is hoped that this model will help countries prioritise strategies to strengthen care,
                    estimate the costs of doing so, and encourage the regular collection of data representing
                    the cascade of care.")
            )
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "The Model",
            collapsible = TRUE,
            collapsed = TRUE,
            "A simplified structure of the model is shown below. It should be noted that, state compartments
                do not exactly correlate with the indicators in Consolidated Indicator Guidelines. This
                is because compartments in the model must be mutually exclusive, while the indicators
                listed in the WHO Guidelines are not all discrete; for example, 'Knowing HIV status'
                includes all patients who are in care, on treatment, virally suppressed, and lost from
                care as long as they are diagnosed. However, the model is able to reconcile this by
                taking individual indicators and separating them into their components to specify the
                initial conditions for simulations.",
            helpText("Further details on the model can be found in the supporting documentation, found
                on the 'Model Document' page, accessible by clicking the '?' symbol on the left-hand
                side of the screen."),
            img(src = "ModelSimple.png", height = '100%', width = '100%')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Quick Start",
            helpText("If you want to skip the introduction and get modelling, click 'Start Wizard' to
                jump right into the model. Data on the cascade can be entered on the following pages,
                allowing the model to calibrate to the latest estimates of the cascade. After calibration,
                the model will show its predictions of the changes to care over time, and can be used to
                identify interventions to improve care and achieve the UNAIDS 90-90-90 targets by 2020.
                Finally, a pdf report can be generated containing all inputs and outputs of this modelling tool."),
            h5("Contributors"),
            tags$i("Jack J Olney, Jeffrey W Eaton, Ellen McRobie & Timothy B Hallett")
        ),
        bsAlert(anchorId = "startAlert"),
        bsButton(inputId = "NEXT_intro", label = "Start Wizard", style = "success", size = "large", block = TRUE, icon = icon("magic", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
