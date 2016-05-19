output$optParTable_Rho <- renderTable({
    theData <- seq(from = input$userOptRho_Range[1], to = input$userOptRho_Range[2], length.out = input$optimParamLength)
    tbl <- matrix(theData, nrow = 1, ncol = input$optimParamLength)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$optimParamLength, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Q <- renderTable({
    theData <- seq(from = input$userOptq_Range[1], to = input$userOptq_Range[2], length.out = input$optimParamLength)
    tbl <- matrix(theData, nrow = 1, ncol = input$optimParamLength)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$optimParamLength, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Kappa <- renderTable({
    theData <- seq(from = input$userOptKappa_Range[2], to = input$userOptKappa_Range[1], length.out = input$optimParamLength)
    tbl <- matrix(theData, nrow = 1, ncol = input$optimParamLength)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$optimParamLength, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Gamma <- renderTable({
    theData <- seq(from = input$userOptGamma_Range[1], to = input$userOptGamma_Range[2], length.out = input$optimParamLength)
    tbl <- matrix(theData, nrow = 1, ncol = input$optimParamLength)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$optimParamLength, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Sigma <- renderTable({
    theData <- seq(from = input$userOptSigma_Range[1], to = input$userOptSigma_Range[2], length.out = input$optimParamLength)
    tbl <- matrix(theData, nrow = 1, ncol = input$optimParamLength)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$optimParamLength, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Omega <- renderTable({
    theData <- seq(from = input$userOptOmega_Range[2], to = input$userOptOmega_Range[1], length.out = input$optimParamLength)
    tbl <- matrix(theData, nrow = 1, ncol = input$optimParamLength)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$optimParamLength, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optIterationTable <- renderTable({
    ParInput <- data.frame(
        Rho   = seq(from = if (input$TestingCheck)      {input$userOptRho_Range[1]}   else {1}, to = if (input$TestingCheck)      {input$userOptRho_Range[2]}   else {1}, length.out = input$optimParamLength),
        Q     = seq(from = if (input$LinkageCheck)      {input$userOptq_Range[1]}     else {1}, to = if (input$LinkageCheck)      {input$userOptq_Range[2]}     else {1}, length.out = input$optimParamLength),
        Kappa = seq(from = if (input$PreRetentionCheck) {input$userOptKappa_Range[1]} else {1}, to = if (input$PreRetentionCheck) {input$userOptKappa_Range[2]} else {1}, length.out = input$optimParamLength),
        Gamma = seq(from = if (input$InitiationCheck)   {input$userOptGamma_Range[1]} else {1}, to = if (input$InitiationCheck)   {input$userOptGamma_Range[2]} else {1}, length.out = input$optimParamLength),
        Sigma = seq(from = if (input$AdherenceCheck)    {input$userOptSigma_Range[1]} else {1}, to = if (input$AdherenceCheck)    {input$userOptSigma_Range[2]} else {1}, length.out = input$optimParamLength),
        Omega = seq(from = if (input$RetentionCheck)    {input$userOptOmega_Range[1]} else {1}, to = if (input$RetentionCheck)    {input$userOptOmega_Range[2]} else {1}, length.out = input$optimParamLength)
    )
    tbl <- matrix(0, nrow = 2, ncol = 1)
    size <- dim(unique(expand.grid(ParInput)))[1]
    tbl[1,] <- size
    tbl[2,] <- size * 0.01
    colnames(tbl) <- "Value"
    rownames(tbl) <- c("Number of iterations:","Estimated time (seconds):")
    return(tbl)
})
