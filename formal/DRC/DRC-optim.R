# Nigeria optim script
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard/inst/app")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")
source("../../formal/zimbabwe/PHIA/optim-functions.R")

MasterName <- "DRC"
MaxError <- 0.1
MinNumber <- 100

MasterData <- GetMasterDataSet(MasterName)

parRange <- DefineParmRange(
    p = c(0.7, 1),
    omega = c(0, 0.01),
    epsilon = c(100, 100),
    q = c(0.9, 1),
    kappa = c(0, 0.05)
)

# function setup
country          = MasterName
data             = MasterData
maxIterations    = 1e4
maxError         = MaxError
limit            = MinNumber
parRange         = parRange
targetIterations = 1e5


# define time
time <- seq(0, 5, 1)

# define p
p <- parameters(
    prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
    prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
    prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
    prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
    prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
    prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
    prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
    t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
    t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
    t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
    t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
    t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
)

# define y
optim_initial <- matrix(0,1,4)
colnames(optim_initial) <- c("plhiv", "plhiv_diag", "plhiv_care", "plhiv_art")
# optim_initial[1,] <- c(445490, 114957.63, 105216.29, 39238.61)
optim_initial[1,] <- c(445490, 60000, 50000, 39238.61)

# p[["p"]] <- 0
y <- GetCalibInitial(p, data, init2010 = optim_initial[1,])

# define i
# using median values from Spectrum
i <- incidence(as.double(data$incidence[2,3:9]))
# scope to add this as a paramter later?

# p
# y
# i
# SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

# define par (the parameters that we want optim to vary)
par <- c(
    Rho     = 0.3148,
    Epsilon = 100,
    Kappa   = 0.0261,
    Gamma   = 0.8037,
    Theta   = 0.3992,
    Omega   = 0.0052,
    p       = 0.8375,
    q       = 0.9479
)

GetError(par)

out <- optim(par, GetError)
out$value
as.list(out$par)

result <- c(
    Rho     = 0.02287549,
    Epsilon = 105.7396,
    Kappa   = 3.883224,
    Gamma   = 1.15771,
    Theta   = 0.08828346,
    Omega   = 0.02574068,
    p       = 2.045676e-05,
    q       = 0.4493568
)

# error = 0.08627207
GetError(result)
# 400000 in care.

calibOut <- GetErrorResult(result)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

# graphics.off(); quartz.options(w = 9, h = 4)
# BuildPHIAPlot(data = calibOut)
# quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

# OKAY we should only be passing parameters that we VARY

# optim parameter bounds


# optim(par = par, fn = GetError, method = "L-BFGS-B", lower = parRange$min, upper = parRange$max)

################################################################################
# Constrained Optimisation TAKE TWO #

GetErrorConstrained <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 100)   par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.1)  par[["p"]]       <- 0.1
    if (par[["q"]]       < 0.3)  par[["q"]]       <- 0.3


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 10) par[["Kappa"]]     <- 10
    if (par[["Gamma"]]   > 4)    par[["Gamma"]]   <- 4
    if (par[["Theta"]]   > 0.5)    par[["Theta"]]   <- 0.5
    if (par[["Omega"]]   > 0.05) par[["Omega"]]   <- 0.05
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

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

GetErrorConstrained(par)
GetError(par)

out <- optim(par, GetErrorConstrained)
out$value
out$par

testResult <- c(
    Rho     = 0.01644884,
    Epsilon = 101.10953770,
    Kappa   = 4.39250933,
    Gamma   = 0.84941599,
    Theta   = 0.49302572,
    Omega   = 6.27621235,
    p       = 0.17461624,
    q       = 1.24494045
)

GetErrorConstrained(out$par)

GetRealPar <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 100)   par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.2)  par[["p"]]       <- 0.2
    if (par[["q"]]       < 0.3)  par[["q"]]       <- 0.3


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 10)    par[["Kappa"]]   <- 10
    if (par[["Gamma"]]   > 4)    par[["Gamma"]]   <- 4
    if (par[["Theta"]]   > 0.5)    par[["Theta"]]   <- 0.5
    if (par[["Omega"]]   > 0.05)    par[["Omega"]]   <- 0.05
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

    par
}

realResult <- GetRealPar(out$par)
as.matrix(realResult)

GetErrorConstrained(testResult)
GetErrorConstrained(realResult)

calibOut <- GetErrorResult(realResult)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

################################################################################
# Wednesday

GetErrorConstrained <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 90)   par[["Epsilon"]] <- 90
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.5)  par[["p"]]       <- 0.5
    if (par[["q"]]       < 0.9)  par[["q"]]       <- 0.9


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 0.5)  par[["Kappa"]]   <- 0.5
    if (par[["Gamma"]]   > 2)    par[["Gamma"]]   <- 2
    if (par[["Theta"]]   > 2)    par[["Theta"]]   <- 2
    if (par[["Omega"]]   > 0.5)  par[["Omega"]]   <- 0.5
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

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

GetErrorConstrained(par)

res <- optim(par, GetErrorConstrained)
as.matrix(res$par)
res$value

testResult <- c(
    Rho     = -1.290357779,
    Epsilon = 101.583061566,
    Kappa   = 9.674792436,
    Gamma   = 1.319025464,
    Theta   = 0.390643260,
    Omega   = 0.004153525,
    p       = 0.785520885,
    q       = 0.897956715
)

GetErrorConstrained(testResult)
GetErrorConstrained(res$par)

GetRealPar <- function(par) {

    # Lower bounds
    if (par[["Rho"]]     < 1e-3) par[["Rho"]]     <- 1e-3
    if (par[["Epsilon"]] < 90)   par[["Epsilon"]] <- 90
    if (par[["Kappa"]]   < 1e-3) par[["Kappa"]]   <- 1e-3
    if (par[["Gamma"]]   < 1e-3) par[["Gamma"]]   <- 1e-3
    if (par[["Theta"]]   < 1e-3) par[["Theta"]]   <- 1e-3
    if (par[["Omega"]]   < 1e-3) par[["Omega"]]   <- 1e-3
    if (par[["p"]]       < 0.5)  par[["p"]]       <- 0.5
    if (par[["q"]]       < 0.9)  par[["q"]]       <- 0.9


    # Upper bounds
    if (par[["Rho"]]     > 1)    par[["Rho"]]     <- 1
    if (par[["Epsilon"]] > 100)  par[["Epsilon"]] <- 100
    if (par[["Kappa"]]   > 0.5)  par[["Kappa"]]   <- 0.5
    if (par[["Gamma"]]   > 2)    par[["Gamma"]]   <- 2
    if (par[["Theta"]]   > 2)    par[["Theta"]]   <- 2
    if (par[["Omega"]]   > 0.5)  par[["Omega"]]   <- 0.5
    if (par[["p"]]       > 1)    par[["p"]]       <- 1
    if (par[["q"]]       > 1)    par[["q"]]       <- 1

    par
}


realResult <- GetRealPar(testResult)
as.matrix(realResult)

GetErrorConstrained(testResult)
GetErrorConstrained(realResult)

calibOut <- GetErrorResult(realResult)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")

################################################################################
# GetErrorCD4

GetErrorCD4(par)

res <- optim(par, GetErrorCD4)
as.matrix(res$par)
res$value

result <- c(
    Rho     = 0.0002390255,
    Epsilon = 102.2647,
    Kappa   = 1.749754,
    Gamma   = 3.581516,
    Theta   = 0.3114793,
    Omega   = 0.006870914,
    p       = 0.8291169,
    q       = 0.9473207
)
# error = 0.01418761
GetErrorCD4(result)
# 400000 in care.

calibOut <- GetErrorResultCD4(result)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

GenSinglePowersPlot_Thesis()
GetPowersCascadeData
GetModel

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")


################################################################################
################################################################################
## CD4 distribution at ART initiation
#
# PLAN:
# omega = 0
# p = 1
# delta 1-5 = 0
# tau 1-7 = 0
# this will force 'Tx_A' to spit out the CD4 count at ART initiation
# test out in optim() and then move to approx Bayesian tomorrow

# first we need a test base.

result <- c(
    Rho     = 0.0002390255,
    Epsilon = 102.2647,
    Kappa   = 1.749754,
    Gamma   = 3.581516,
    Theta   = 0.3114793,
    Omega   = 0.006870914,
    p       = 0.8291169,
    q       = 0.9473207
)

# Calibration 2
# below are the constrained parameters
result_old <- c(
    Rho     = 0.031277257,
    Epsilon = 102.567917414,
    Kappa   = 2.064367960,
    Gamma   = 1.619482994,
    Theta   = 0.421657503,
    Omega   = 0.001290356,
    p       = 0.785773456,
    q       = 1.269157212
)

GetResult_CUSTOM <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # # Not in care ART initiation rate CD4 adjustment
    # p[["s_1"]] <- 1
    # p[["s_2"]] <- 1
    # p[["s_3"]] <- 1
    # p[["s_4"]] <- 1
    # p[["s_5"]] <- 1
    # p[["s_6"]] <- 1
    # p[["s_7"]] <- 1

    # custom edits to output CD4 distribution at ART initiation
    p[["Omega"]]   <- 0
    p[["p"]]       <- 1
    p[["Delta_1"]] <- 0
    p[["Delta_2"]] <- 0
    p[["Delta_3"]] <- 0
    p[["Delta_4"]] <- 0
    p[["Delta_5"]] <- 0
    p[["Tau_1"]]   <- 0
    p[["Tau_2"]]   <- 0
    p[["Tau_3"]]   <- 0
    p[["Tau_4"]]   <- 0
    p[["Tau_5"]]   <- 0
    p[["Tau_6"]]   <- 0
    p[["Tau_7"]]   <- 0

    out <- CallCalibModel(time, y, p, i)
    out
}

test <- GetResult_CUSTOM(result_old)

plot_CD4_at_initiation <- function(year, dat) {

    a <- dat$Tx_A_500 + dat$Tx_A_350500 + dat$Tx_A_250350 + dat$Tx_A_200250 + dat$Tx_A_100200 + dat$Tx_A_50100 + dat$Tx_A_50

    cd4_500 <- dat$Tx_A_500[year] / a[year]
    cd4_350500 <- dat$Tx_A_350500[year] / a[year]
    cd4_250350 <- dat$Tx_A_250350[year] / a[year]
    cd4_200250 <- dat$Tx_A_200250[year] / a[year]
    cd4_100200 <- dat$Tx_A_100200[year] / a[year]
    cd4_50100 <- dat$Tx_A_50100[year] / a[year]
    cd4_50 <- dat$Tx_A_50[year] / a[year]

    proportion <- c(cd4_500, cd4_350500, cd4_250350, cd4_200250, cd4_100200, cd4_50100, cd4_50)
    category <- c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50")
    df <- data.frame(category, proportion)

    # set levels
    df$category <- factor(df$category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))

    # set position
    df$pos <- 1 - (cumsum(df$proportion) - df$proportion / 2)

    ggOut <- ggplot(df, aes(x = "", y = proportion, fill = category))
    ggOut <- ggOut + geom_bar(width = 1, stat = "identity")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + coord_polar(theta = "y")
    ggOut <- ggOut + ggrepel::geom_label_repel(aes(y = pos, label = scales::percent(round(proportion, digits = 2))), size = 8, family = figFont, show.legend = FALSE)
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
    ggOut <- ggOut + theme(legend.position = "right")
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(axis.text = element_blank())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_blank())
    ggOut <- ggOut + theme(axis.ticks = element_blank())
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(legend.text = element_text(size = 15))
    ggOut <- ggOut + theme(legend.key.size = unit(1, "cm"))
    ggOut <- ggOut + ggtitle(paste("CD4 distribution of persons on ART in", 2010 + (year - 1)))
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5, size = 18))
    ggOut
}

plot_CD4_at_initiation(year = 6, dat = test)

graphics.off(); quartz.options(w = 8, h = 6)

year = 6
plot_CD4_at_initiation(year = year, dat = test)
filename <- paste0("../../formal/zimbabwe/PHIA/fig/cal/CD4-", 2010 + (year - 1) ,".pdf")
quartz.save(file = filename, type = "pdf")


# convert this to use the approx Bayesian calibration method

# need some narrative around how the model, with the CD4 dependency on theta,
# alters (I think increases) theta to produce the CD4 distribution we see,
# then when we remove the CD4 dependency on theta, calibration adjusts theta to
# give us a similar result? perhaps.

####################################################################################################
# initiation rate = # initiating by CD4 / number untreated in that category

head(test)

# actually I think its this.
a <- diff(test$Tx_A_500)    / (test$UnDx_500    + test$Dx_500    + test$Care_500    + test$PreLtfu_500)[2:6]
b <- diff(test$Tx_A_350500) / (test$UnDx_350500 + test$Dx_350500 + test$Care_350500 + test$PreLtfu_350500)[2:6]
c <- diff(test$Tx_A_250350) / (test$UnDx_250350 + test$Dx_250350 + test$Care_250350 + test$PreLtfu_250350)[2:6]
d <- diff(test$Tx_A_200250) / (test$UnDx_200250 + test$Dx_200250 + test$Care_200250 + test$PreLtfu_200250)[2:6]
e <- diff(test$Tx_A_100200) / (test$UnDx_100200 + test$Dx_100200 + test$Care_100200 + test$PreLtfu_100200)[2:6]
f <- diff(test$Tx_A_50100)  / (test$UnDx_50100  + test$Dx_50100  + test$Care_50100  + test$PreLtfu_50100)[2:6]
g <- diff(test$Tx_A_50)     / (test$UnDx_50     + test$Dx_50     + test$Care_50     + test$PreLtfu_50)[2:6]



rate <- c(a, b, c, d, e, f, g)
category <- c(
    rep("<500", 5),
    rep("350-500", 5),
    rep("250-350", 5),
    rep("200-250", 5),
    rep("100-200", 5),
    rep("50-100", 5),
    rep("<50", 5)
)
year <- rep(seq(2011, 2015, 1), 7)
df <- data.frame(year, category, rate)

df$category <- factor(df$category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))


graphics.off(); quartz.options(w = 5.5, h = 3.5)
ggplot(df, aes(x = year, y = rate, group = category)) +
geom_line(aes(color = category)) +
geom_point(aes(color = category)) +
scale_colour_manual(values = rev(brewer.pal(7, "RdYlGn"))) +
ggtitle("ART initiation rate by CD4 count", subtitle = "# initiating by CD4 / total number untreated in that category")
quartz.save(file = "../../formal/zimbabwe/PHIA/fig/cal/initiation-rate.pdf", type = "pdf")













####################################################################################################
# Monday 23rd January, 2017

# by removing 'mu' for persons on ART, we can see the distribution at ART initiation

plot_CD4_at_initiation <- function(year, dat) {

    a <- dat$Tx_A_500 + dat$Tx_A_350500 + dat$Tx_A_250350 + dat$Tx_A_200250 + dat$Tx_A_100200 + dat$Tx_A_50100 + dat$Tx_A_50

    # the trouble is, that persons in this group, obviously dwindle over time... natural mortality
    # this branch of 'cascade' has mu for those persons switched off

    cd4_500    <- diff(dat$Tx_A_500)    / diff(a)
    cd4_350500 <- diff(dat$Tx_A_350500) / diff(a)
    cd4_250350 <- diff(dat$Tx_A_250350) / diff(a)
    cd4_200250 <- diff(dat$Tx_A_200250) / diff(a)
    cd4_100200 <- diff(dat$Tx_A_100200) / diff(a)
    cd4_50100  <- diff(dat$Tx_A_50100)  / diff(a)
    cd4_50     <- diff(dat$Tx_A_50)     / diff(a)


    proportion <- c(cd4_500[year], cd4_350500[year], cd4_250350[year], cd4_200250[year], cd4_100200[year], cd4_50100[year], cd4_50[year])
    category <- c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50")
    df <- data.frame(category, proportion)

    # set levels
    df$category <- factor(df$category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))

    # set position
    df$pos <- 1 - (cumsum(df$proportion) - df$proportion / 2)

    ggOut <- ggplot(df, aes(x = "", y = proportion, fill = category))
    ggOut <- ggOut + geom_bar(width = 1, stat = "identity")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + coord_polar(theta = "y")
    ggOut <- ggOut + ggrepel::geom_label_repel(aes(y = pos, label = scales::percent(round(proportion, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(legend.title = element_blank())
    ggOut <- ggOut + theme(axis.text = element_blank())
    ggOut <- ggOut + theme(axis.line.x = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_blank())
    ggOut <- ggOut + theme(axis.ticks = element_blank())
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(legend.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(legend.text = element_text(size = 10))
    ggOut <- ggOut + theme(legend.key.size = unit(0.5, "cm"))
    ggOut <- ggOut + ggtitle(paste("CD4 distribution\nof persons initiating ART in\n", 2010 + year))
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5, size = 12))
    ggOut

}

year = 5
graphics.off(); quartz.options(w = 5, h = 4)
plot_CD4_at_initiation(year = year, dat = test)
filename <- paste0("../../formal/zimbabwe/PHIA/fig/cal/CD4-", 2010 + year ,".pdf")
quartz.save(file = filename, type = "pdf")

# Approximate median CD4 count of persons initiating ART in each year.

# so for each category what is the median?
# >500 = 750?
# 350-500 = 425
# 250-350 = 300
# 200-250 = 225
# 100-200 = 150
# 50-100 = 75
# <50 = 25

calculate_median_cd4_at_initiation <- function(dat) {

    a <- dat$Tx_A_500 + dat$Tx_A_350500 + dat$Tx_A_250350 + dat$Tx_A_200250 + dat$Tx_A_100200 + dat$Tx_A_50100 + dat$Tx_A_50

    cd4_500    <- diff(dat$Tx_A_500)    * 750
    cd4_350500 <- diff(dat$Tx_A_350500) * 425
    cd4_250350 <- diff(dat$Tx_A_250350) * 300
    cd4_200250 <- diff(dat$Tx_A_200250) * 225
    cd4_100200 <- diff(dat$Tx_A_100200) * 150
    cd4_50100  <- diff(dat$Tx_A_50100)  * 75
    cd4_50     <- diff(dat$Tx_A_50)     * 25

    median_cd4_2011 <- sum(cd4_500[1], cd4_350500[1], cd4_250350[1], cd4_200250[1], cd4_100200[1], cd4_50100[1], cd4_50[1]) / diff(a)[1]
    median_cd4_2012 <- sum(cd4_500[2], cd4_350500[2], cd4_250350[2], cd4_200250[2], cd4_100200[2], cd4_50100[2], cd4_50[2]) / diff(a)[2]
    median_cd4_2013 <- sum(cd4_500[3], cd4_350500[3], cd4_250350[3], cd4_200250[3], cd4_100200[3], cd4_50100[3], cd4_50[3]) / diff(a)[3]
    median_cd4_2014 <- sum(cd4_500[4], cd4_350500[4], cd4_250350[4], cd4_200250[4], cd4_100200[4], cd4_50100[4], cd4_50[4]) / diff(a)[4]
    median_cd4_2015 <- sum(cd4_500[5], cd4_350500[5], cd4_250350[5], cd4_200250[5], cd4_100200[5], cd4_50100[5], cd4_50[5]) / diff(a)[5]

    median <- c(median_cd4_2011, median_cd4_2012, median_cd4_2013, median_cd4_2014, median_cd4_2015)
    year <- seq(2011, 2015, 1)
    df <- data.frame(year, median)
    df

}

calculate_median_cd4_at_initiation(dat = test)
