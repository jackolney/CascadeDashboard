# Parameter table
output$parameterTable <- DT::renderDataTable({
    # rely on button press
    input$viewParameterTable

    theP <- GetParameters()
    theParameters <- c(theP[["Rho"]],theP[["Epsilon"]],theP[["Kappa"]],theP[["Gamma"]],theP[["Theta"]],theP[["Omega"]],theP[["Nu_1"]],theP[["Nu_2"]],theP[["Nu_3"]],theP[["Nu_4"]],theP[["Nu_5"]],theP[["Nu_6"]],theP[["p"]],theP[["s_1"]],theP[["s_2"]],theP[["s_3"]],theP[["s_4"]],theP[["s_5"]],theP[["s_6"]],theP[["s_7"]],theP[["Sigma"]],theP[["Delta_1"]],theP[["Delta_2"]],theP[["Delta_3"]],theP[["Delta_4"]],theP[["Delta_5"]],theP[["Alpha_1"]],theP[["Alpha_2"]],theP[["Alpha_3"]],theP[["Alpha_4"]],theP[["Alpha_5"]],theP[["Alpha_6"]],theP[["Alpha_7"]],theP[["Tau_1"]],theP[["Tau_2"]],theP[["Tau_3"]],theP[["Tau_4"]],theP[["Tau_5"]],theP[["Tau_6"]],theP[["Tau_7"]],theP[["Mu"]],theP[["Iota_1"]],theP[["Iota_2"]],theP[["Iota_3"]],theP[["Iota_4"]],theP[["Iota_5"]],theP[["Iota_6"]],theP[["Iota_7"]])
    ParameterNames <- c("Rho","Epsilon","Kappa","Gamma","Theta","Omega","Nu_1","Nu_2","Nu_3","Nu_4","Nu_5","Nu_6","p","s_1","s_2","s_3","s_4","s_5","s_6","s_7","Sigma","Delta_1","Delta_2","Delta_3","Delta_4","Delta_5","Alpha_1","Alpha_2","Alpha_3","Alpha_4","Alpha_5","Alpha_6","Alpha_7","Tau_1","Tau_2","Tau_3","Tau_4","Tau_5","Tau_6","Tau_7","Mu","Iota_1","Iota_2","Iota_3","Iota_4","Iota_5","Iota_6","Iota_7")
    rows <- length(ParameterNames)
    tbl <- matrix(theParameters,rows, ncol = 2)
    tbl[,1] <- ParameterNames
    colnames(tbl) <- c("Parameter","Value")
    return(datatable(tbl, options = list(pageLength = 25, autoWidth = TRUE)))
    }
)

# Optimisation Plot 1
output$opt909090Table <- DT::renderDataTable({
    return(datatable(Result_909090,options = list(pageLength = 25, autoWidth = TRUE)) %>%
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
)

# Optimisation Plot 1 (brushed)
output$opt909090TableBrushed <- DT::renderDataTable({
    theBrushed <- brushedPoints(df = Result_909090,brush = input$plotOpt909090_brush)
    return(datatable(theBrushed, options = list(pageLength = 25, autoWidth = TRUE)) %>%
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
)

# Optimisation Plot 2
output$optDALYsTable <- DT::renderDataTable({
    return(datatable(Result_DALYs, options = list(pageLength = 25, autoWidth = TRUE)) %>%
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
)

# Optimisation Plot 2 (brushed)
output$optDALYsTableBrushed <- DT::renderDataTable({
    theBrushed <- brushedPoints(df = Result_DALYs,brush = input$plotOptDALYs_brush)
    return(datatable(theBrushed, options = list(pageLength = 25, autoWidth = TRUE)) %>%
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
)

# Optimisation Plot 3
output$optDALYs909090Table <- DT::renderDataTable({
    return(datatable(Result_DALYs_909090, options = list(pageLength = 25, autoWidth = TRUE)) %>%
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
)

# Optimisation Plot 3 (brushed)
output$optDALYs909090TableBrushed <- DT::renderDataTable({
    theBrushed <- brushedPoints(df = Result_DALYs_909090,brush = input$plotOptDALYs909090_brush)
    return(datatable(theBrushed, options = list(pageLength = 25, autoWidth = TRUE)) %>%
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
)

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
