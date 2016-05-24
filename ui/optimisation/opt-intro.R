tabItem(tabName = "opt-intro",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Interventions Targetting the Care Cascade",
            "The structure of the model allows us to implement and assess the impact of six broad
            interventions acting on various elements of the cascade. The interventions are described
            below and can be implemented on the following pages.",

            tags$h3("HIV Testing"),
            "We simulate a broad HIV-testing intervention, that involves increasing the rate at which
            undiagnosed individuals become diagnosed. As the model does not account for HIV-negative
            individuals this rate cannot be interpretted as an 'HIV-testing rate', as the true HIV-testing
            rate would depend on the proportion of postive undiagnosed individuals in the population
            and would almost certainly be higher. However, this intervention will be able to identify
            how many more individuals need to be diagnosed per year.",

            tags$h3("Linkage"),
            "We simulate a linkage intervention that adjusts the proportion of diagnosed individuals
            that link to care. After diagnosis, a proportion (q) of infected individuals link to pre-ART
            care at a rate (epsilon) determined during calibration. However, 1-q individuals fail to
            link to care and are lost to follow-up; although, all individuals have the propensity to
            seek care at a rate determined by their current health state. As an individual's CD4 count
            declines, they seek care at a higher rate.",

            tags$h3("Pre-ART Retention"),
            "During calibration, a rate of loss from pre-ART care for diagnosed individuals is derived
            (kappa). We simulate a pre-ART retention intervention that reduces pre-ART loss by lowering
            the pre-ART loss rate, kappa. This keep patients engaged in pre-ART care and increases the
            likelihood that they will initiate ART.",

            tags$h3("ART Initiation"),
            "Our ART initiation intervention involves increasing the rate at which diagnosed individuals
            engaged in pre-ART care start treatment. This rate is initially derived during calibration,
            but is also dependent upon country specific treatment guidelines. For example, an individual
            will not initiate ART with a CD4 count of >500 if the national guidelines only suggest ART
            for persons with CD4 <350.",

            tags$h3("Adherence"),
            "Upon initiating ART, a proportion (p) of patients adhere to treatment and become virally
            suppressed. However, 1-p individuals do not adhere to treatment and HIV progresses as if
            they were treatment naive. We implement an adherence intervention by allowing non-adherent
            persons on treatment to start adhereing at some rate, sigma. Prior to the intervention,
            this rate is zero, and no individuals progress from non-adherent to adherent states.",

            tags$h3("ART Retention"),
            "Finally, we simulate an intervention that reduces loss to follow-up for persons on treatment.
            During calibration we derive a baseline rate at which individuals are lost from ART care,
            omega. We simulate an intervention by reducing this rate of loss from care. Additionally,
            this rate applies to both adherent and non-adherent persons on ART."
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            "With a calibrated model of the cascade, we can project forward until 2020, to estimate
            changes to the distribution of individuals in care. This allows us to ascertain whether
            the UNAIDS 90-90-90 targets will be met, along with estimating changes in HIV incidence
            and AIDS-related mortality. Additionally, this calibrated model allows us to explore the
            impact and cost of a range of hypothetical interventions acting at various stages of care.
            Due to the simplistic nature of the model, however, we can only test broad categories of
            interventions. Potential interventions that can be simulated are listed in the main panel.",
            helpText("Please hit 'Next' to proceed to optimisation.
                Further page options are available from the sidebar.")
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "PREV_optIntro", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                HTML('<button id="NEXT_optIntro" type="button" class="btn action-button btn-success btn-lg btn-block"> Next <i class="fa fa-arrow-right fa-lg fa-fw"></i> </button>')
            )
        )
    )
)
