ExtractCascadeData <- function(year) {
    result <- CallModel()

    # We iterate across _all_ results, and present the mean, and ranges.
    # Also switched to absolute values, not proportions.
    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    CX_data <- unlist(lapply(result, function(x) sum(x$Care[year], x$ART[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    NX <- Rmisc::CI(NX_data, ci = 0.95)
    DX <- Rmisc::CI(DX_data, ci = 0.95)
    CX <- Rmisc::CI(CX_data, ci = 0.95)
    TX <- Rmisc::CI(TX_data, ci = 0.95)
    VS <- Rmisc::CI(VS_data, ci = 0.95)

    res <- c(NX[["mean"]], DX[["mean"]], CX[["mean"]], TX[["mean"]], VS[["mean"]])
    min <- c(NX[["lower"]], DX[["lower"]], CX[["lower"]], TX[["lower"]], VS[["lower"]])
    max <- c(NX[["upper"]], DX[["upper"]], CX[["upper"]], TX[["upper"]], VS[["upper"]])

    def <- c("# PLHIV", "# Diagnosed", "# In Care", "# Treatment", "# Suppressed")
    df <- data.frame(def, res, min, max)
    df$def <- factor(df$def, levels = c("# PLHIV", "# Diagnosed", "# In Care", "# Treatment", "# Suppressed"))
    df
}

ExtractPowersCascadeData <- function(year) {
    result <- CallModel()

    UNDX <- mean(unlist(lapply(result, function(x) sum(x$UnDx[year]))))
    DX   <- mean(unlist(lapply(result, function(x) sum(x$Dx[year]))))
    CX   <- mean(unlist(lapply(result, function(x) sum(x$Care[year]))))
    PLX  <- mean(unlist(lapply(result, function(x) sum(x$PreLtfu[year]))))
    TXN  <- mean(unlist(lapply(result, function(x) sum(x$Tx[year] - x$Vs[year]))))
    VS   <- mean(unlist(lapply(result, function(x) sum(x$Vs[year]))))
    LX   <- mean(unlist(lapply(result, function(x) sum(x$Ltfu[year]))))

    res <- c(VS, TXN, CX, DX, UNDX, PLX, LX,
             VS, TXN, CX, DX, PLX, LX,
             VS, TXN, CX,
             VS, TXN,
             VS)

    state <- c("# Suppressed", "# On Treatment (non-adherent)", "# In Care", "# Diagnosed", "# Undiagnosed", "# pre-ART LTFU", "# LTFU",
               "# Suppressed", "# On Treatment (non-adherent)", "# In Care", "# Diagnosed", "# pre-ART LTFU", "# LTFU",
               "# Suppressed", "# On Treatment (non-adherent)", "# In Care",
               "# Suppressed", "# On Treatment (non-adherent)",
               "# Suppressed")

    order <- c(rep("All"       ,7),
               rep("Diagnosed" ,6),
               rep("Care"      ,3),
               rep("Treatment" ,2),
               rep("Suppressed",1))

    df <- data.frame(state, res, order)
    df$order <- factor(df$order, levels = c("All", "Diagnosed", "Care", "Treatment", "Suppressed"))
    df$state <- factor(df$state, levels = c("# Suppressed", "# On Treatment (non-adherent)", "# In Care", "# Diagnosed", "# Undiagnosed", "# pre-ART LTFU", "# LTFU"))
    df
}

Extract909090Data <- function(...) {
    # Prevent unncessary calls to CallModel() [shiny might actually handle this automatically]
    data <- c(...)
    if (length(data) > 0L) {
        result <- data
    } else {
        result <- CallModel()
    }

    # Always aiming for 2020 here (5.02 / 0.02)
    year <- 251

    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    UN_90 <- Rmisc::CI(DX_data / NX_data, ci = 0.95)
    UN_9090 <- Rmisc::CI(TX_data / DX_data, ci = 0.95)
    UN_909090 <- Rmisc::CI(VS_data / TX_data, ci = 0.95)

    res <- c(UN_90[["mean"]], UN_9090[["mean"]], UN_909090[["mean"]])
    min <- c(UN_90[["lower"]], UN_9090[["lower"]], UN_909090[["lower"]])
    max <- c(UN_90[["upper"]], UN_9090[["upper"]], UN_909090[["upper"]])
    def <- c("% Diagnosed","% On Treatment","% Suppressed")
    out <- data.frame(def, res, min, max)
    out$def <- factor(out$def, levels = c("% Diagnosed", "% On Treatment", "% Suppressed"))
    out
}
