# Budget Table
output$optBudgetTable <- DT::renderDataTable({
    # Re-render upon button press
    input$showBudget909090
    input$showBudgetDALYs

    if (Budget$Switch == "the909090") {
        theTable <- filter(Result_909090,Cost <= input$userBudget)
        return(datatable(theTable,
            extensions = 'TableTools',
            options = list(
            dom = 'T<"clear">lfrtip',
            tableTools = list(sSwfPath = copySWF('www')),
            order = list(list(4, 'desc')), autoWidth = TRUE, pageLength = 100)) %>%
            formatRound("90",3) %>%
            formatRound("90-90",3) %>%
            formatRound("90-90-90",3) %>%
            formatRound("VS",3) %>%
            formatRound("Rho",3) %>%
            formatRound("Q",3) %>%
            formatRound("Kappa",3) %>%
            formatRound("Gamma",3) %>%
            formatRound("Sigma",3) %>%
            formatRound("Omega",3) %>%
            formatCurrency("Cost",'$')
            )
        }

    if (Budget$Switch == "DALYs") {
        theTable <- filter(Result_DALYs,Cost <= input$userBudget)
        return(datatable(theTable,
            extensions = 'TableTools',
            options = list(
            dom = 'T<"clear">lfrtip',
            tableTools = list(sSwfPath = copySWF('www')),
            order = list(list(1, 'desc')), autoWidth = TRUE, pageLength = 100)) %>%
            formatRound("Rho",3) %>%
            formatRound("Q",3) %>%
            formatRound("Kappa",3) %>%
            formatRound("Gamma",3) %>%
            formatRound("Sigma",3) %>%
            formatRound("Omega",3) %>%
            formatCurrency("DALYs",'') %>%
            formatCurrency("Cost",'$')
            )
        }
    }
)
