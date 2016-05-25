output$optParTable_Rho <- renderTable({
    theData <- seq(from = input$userOptRho_Range[1], to = input$userOptRho_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Q <- renderTable({
    theData <- seq(from = input$userOptq_Range[1], to = input$userOptq_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Kappa <- renderTable({
    theData <- seq(from = input$userOptKappa_Range[2], to = input$userOptKappa_Range[1], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Gamma <- renderTable({
    theData <- seq(from = input$userOptGamma_Range[1], to = input$userOptGamma_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Sigma <- renderTable({
    theData <- seq(from = input$userOptSigma_Range[1], to = input$userOptSigma_Range[2], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Omega <- renderTable({
    theData <- seq(from = input$userOptOmega_Range[2], to = input$userOptOmega_Range[1], length.out = 4)
    tbl <- matrix(theData, nrow = 1, ncol = 4)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, 4, 1), sep = '')
    return(tbl)
}, digits = 3)
