GetError <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

GetErrorResult <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

GetErrorCD4 <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1
    p[["s_2"]] <- 1
    p[["s_3"]] <- 1
    p[["s_4"]] <- 1
    p[["s_5"]] <- 1
    p[["s_6"]] <- 1
    p[["s_7"]] <- 1

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

GetErrorResultCD4 <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1
    p[["s_2"]] <- 1
    p[["s_3"]] <- 1
    p[["s_4"]] <- 1
    p[["s_5"]] <- 1
    p[["s_6"]] <- 1
    p[["s_7"]] <- 1

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

BuildOptimPowersPlot <- function(dat) {

    d <- dat[dat$source == "model" & dat$year == 2015,]

    UNDX <- d[d$indicator == "PLHIV","value"] - d[d$indicator == "PLHIV Diagnosed","value"]
    DX   <- d[d$indicator == "PLHIV Diagnosed","value"] - d[d$indicator == "PLHIV in Care","value"] - d[d$indicator == "PLHIV Pre-ART LTFU","value"] - d[d$indicator == "PLHIV ART LTFU","value"]
    CX   <- d[d$indicator == "PLHIV in Care","value"] - d[d$indicator == "PLHIV on ART","value"]
    PLX  <- d[d$indicator == "PLHIV Pre-ART LTFU","value"]
    TXN  <- d[d$indicator == "PLHIV on ART","value"] - d[d$indicator == "PLHIV Suppressed","value"]
    VS   <- d[d$indicator == "PLHIV Suppressed","value"]
    LX   <- d[d$indicator == "PLHIV ART LTFU","value"]

    res <- c(VS, TXN, CX, DX, UNDX, PLX, LX,
             VS, TXN, CX, DX, PLX, LX,
             VS, TXN, CX,
             VS, TXN,
             VS)

    state <- c("On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Undiagnosed", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART",
               "On ART\n virally suppressed", "On ART\n (non-adherent)",
               "On ART\n virally suppressed")

    order <- c(rep("All"       ,7),
               rep("Diagnosed" ,6),
               rep("Care"      ,3),
               rep("Treatment" ,2),
               rep("Suppressed",1))

    df <- data.frame(state, res, order)
    df$order <- factor(df$order, levels = c("All", "Diagnosed", "Care", "Treatment", "Suppressed"))
    df$state <- factor(df$state, levels = c("On ART\n virally suppressed", "On ART\n (non-adherent)", "In care,\nnot on ART", "Diagnosed,\nnot in care", "Undiagnosed", "Diagnosed,\nLTFU pre-ART", "Diagnosed,\nLTFU post-ART"))
    df

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggOne <- ggplot(df, aes(x = order, y = res, fill = state))
    ggOne <- ggOne + geom_bar(stat = 'identity')
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = p.col, guide = guide_legend(title = ""))
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 13))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 12))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 12))
    ggOne <- ggOne + theme(legend.text = element_text(size = 7))
    ggOne <- ggOne + theme(legend.title = element_text(size = 12))
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + theme(axis.line.y = element_line())
    ggOne <- ggOne + expand_limits(y = round(sum(df[df$order == "All","res"]), -5))
    ggOne
}
