tabItem(tabName = "opt-parameter",
    column(width = 8,
        shinyjs::useShinyjs(),
        id = "optimisation-panel",
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "HIV-Testing",
            helpText("By adjusting the rate below, we define the maximum impact that a potential
                testing intervention can achieve. This is shown in the box on the right hand side.
                All values shown are averaged between 2015 and 2020."),
            uiOutput("UI_opt_rho_MAX"),
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "ib_testing_baseline", width = NULL)
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "ib_testing_intervention", width = NULL)
                )
            )
        ),
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Linkage",
            helpText("By adjusting the rate below, we define the maximum impact that a potential
                linkage intervention can achieve. This is shown in the box on the right hand side.
                All values shown are averaged between 2015 and 2020."),
            uiOutput("UI_opt_q_MAX"),
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "ib_linkage_baseline", width = NULL)
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "ib_linkage_intervention", width = NULL)
                )
            )
        ),
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Pre-ART Retention",
            helpText("By adjusting the rate below, we define the maximum impact that a potential
                pre-ART retention intervention can achieve. This is shown in the box on the right hand side.
                All values shown are averaged between 2015 and 2020."),
            uiOutput("UI_opt_kappa_MAX"),
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "ib_preRetention_baseline", width = NULL)
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "ib_preRetention_intervention", width = NULL)
                )
            )
        ),
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Treatment Initiation",
            helpText("By adjusting the rate below, we define the maximum impact that a potential
                ART initiation intervention can achieve. This is shown in the box on the right hand side.
                All values shown are averaged between 2015 and 2020."),
            uiOutput("UI_opt_gamma_MAX"),
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "ib_initiation_baseline", width = NULL)
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "ib_initiation_intervention", width = NULL)
                )
            )
        ),
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Adherence",
            helpText("By adjusting the rate below, we define the maximum impact that a potential
                adherence intervention can achieve. This is shown in the box on the right hand side.
                All values shown are averaged between 2015 and 2020."),
            uiOutput("UI_opt_sigma_MAX"),
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "ib_adherence_baseline", width = NULL)
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "ib_adherence_intervention", width = NULL)
                )
            )
        ),
        box(width = NULL,
            height = '100%',
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "ART Retention",
            helpText("By adjusting the rate below, we define the maximum impact that a potential
                ART retention intervention can achieve. This is shown in the box on the right hand side.
                All values shown are averaged between 2015 and 2020."),
            uiOutput("UI_opt_omega_MAX"),
            fluidRow(
                column(width = 6,
                    valueBoxOutput(outputId = "ib_retention_baseline", width = NULL)
                ),
                column(width = 6,
                    valueBoxOutput(outputId = "ib_retention_intervention", width = NULL)
                )
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Intervention Detail",
            "The model allows up to six non-specific interventions targetting different aspects of
            the cascade to be tested simultaneously. Click '+' to expand details of a particular
            intervention, and adjust its settings. By default, four uniformally distributed
            permutations of each intervention are simulated by the model.",
            p(""),
            bsButton(inputId = "resetInterventions", label = "RESET INTERVENTIONS", block = TRUE, style = "danger")
        ),
        bsButton(inputId = "PREV_optParam", label = "Back", style = "danger",  size = "large", block = TRUE, icon = icon("arrow-left",  class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
