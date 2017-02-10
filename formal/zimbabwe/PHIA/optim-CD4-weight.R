# Zimbabwe optim script
# optim-CD4-weight.R
# this looks at strategies to achieve a good fit to PHIA and all other data, but
# still maintain some degree of CD4 dependency on theta (side-door ART initiation)
rm(list=ls())

# AIM = Boil the calibration down to a set of REALLY NEAT FUNCTIONS.
setwd("~/git/CascadeDashboard/inst/app")
graphics.off()
quartz.options(w = 10, h = 8)
figFont <- "Avenir Next"

# Source initial files
source("../../formal/initial.R")
source("../../formal/zimbabwe/PHIA/optim-functions.R")

MasterName <- "Zimbabwe"
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
optim_initial[1,] <- c(1220058, 728013, 400000, 292573)
# p[["p"]] <- 0
y <- GetCalibInitial(p, data, init2010 = optim_initial[1,])

# define i
# using median values from Spectrum
i <- incidence(as.double(data$incidence[2,3:9]))
# scope to add this as a parameter later?


####################################################################################################

# Setup....

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

GetTestError <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1 + ((1 / 6) * 0)
    p[["s_2"]] <- 1 + ((1 / 6) * 1)
    p[["s_3"]] <- 1 + ((1 / 6) * 2)
    p[["s_4"]] <- 1 + ((1 / 6) * 3)
    p[["s_5"]] <- 1 + ((1 / 6) * 4)
    p[["s_6"]] <- 1 + ((1 / 6) * 5)
    p[["s_7"]] <- 1 + ((1 / 6) * 6)

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

res <- optim(par, GetTestError)
as.matrix(res$par)
res$value

result <- c(
    Rho     = 0.01005603,
    Epsilon = 93.76206033,
    Kappa   = 7.83799114,
    Gamma   = 4.70862792,
    Theta   = 0.24540021,
    Omega   = 0.01115302,
    p       = 0.70216414,
    q       = 1.94654190
)

# error = 0.0201702
GetTestError(result)
# 400000 in care.

GetTestErrorResult <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1 + ((1 / 6) * 0)
    p[["s_2"]] <- 1 + ((1 / 6) * 1)
    p[["s_3"]] <- 1 + ((1 / 6) * 2)
    p[["s_4"]] <- 1 + ((1 / 6) * 3)
    p[["s_5"]] <- 1 + ((1 / 6) * 4)
    p[["s_6"]] <- 1 + ((1 / 6) * 5)
    p[["s_7"]] <- 1 + ((1 / 6) * 6)

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

calibOut <- GetTestErrorResult(result)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
# quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
# quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

GenSinglePowersPlot_Thesis()
GetPowersCascadeData
GetModel

graphics.off(); quartz.options(w = 8, h = 4)
BuildOptimPowersPlot(dat = calibOut)
# quartz.save(file = "~/Desktop/fig/powers.pdf", type = "pdf")


####################################################################################################

# if I have seven points can I define a function to space them apart?
# then fit with that?

par <- c(
    Rho     = 0.3148,
    Epsilon = 100,
    Kappa   = 0.0261,
    Gamma   = 0.8037,
    Theta   = 0.3992,
    Omega   = 0.0052,
    p       = 0.8375,
    q       = 0.9479,
    x       = 1
)

GetTestError <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1 + ((par[["x"]] / 6) * 0)
    p[["s_2"]] <- 1 + ((par[["x"]] / 6) * 1)
    p[["s_3"]] <- 1 + ((par[["x"]] / 6) * 2)
    p[["s_4"]] <- 1 + ((par[["x"]] / 6) * 3)
    p[["s_5"]] <- 1 + ((par[["x"]] / 6) * 4)
    p[["s_6"]] <- 1 + ((par[["x"]] / 6) * 5)
    p[["s_7"]] <- 1 + ((par[["x"]] / 6) * 6)

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

res <- optim(par, GetTestError)
as.matrix(res$par)

result <- c(
    Rho     = 9.092692e-06,
    Epsilon = 99.2865,
    Kappa   = 9.656716,
    Gamma   = 1.559824,
    Theta   = 0.2264882,
    Omega   = 2.979620e-06,
    p       = 0.7987374,
    q       = 1.127657,
    x       = 1.335805
)

# error = 0.0201702
GetTestError(result)
# 400000 in care.

GetTestErrorResult <- function(par) {

    p[["Rho"]]     <- abs(par[["Rho"]])
    p[["Epsilon"]] <- abs(par[["Epsilon"]])
    p[["Kappa"]]   <- abs(par[["Kappa"]])
    p[["Gamma"]]   <- abs(par[["Gamma"]])
    p[["Theta"]]   <- abs(par[["Theta"]])
    p[["Omega"]]   <- abs(par[["Omega"]])
    p[["p"]]       <- abs(par[["p"]])
    p[["q"]]       <- abs(par[["q"]])

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1 + ((par[["x"]] / 6) * 0)
    p[["s_2"]] <- 1 + ((par[["x"]] / 6) * 1)
    p[["s_3"]] <- 1 + ((par[["x"]] / 6) * 2)
    p[["s_4"]] <- 1 + ((par[["x"]] / 6) * 3)
    p[["s_5"]] <- 1 + ((par[["x"]] / 6) * 4)
    p[["s_6"]] <- 1 + ((par[["x"]] / 6) * 5)
    p[["s_7"]] <- 1 + ((par[["x"]] / 6) * 6)

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

calibOut <- GetTestErrorResult(result)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
# quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")

####################################################################################################
####################################################################################################

result <- c(
    Rho     = 9.092692e-06,
    Epsilon = 99.2865,
    Kappa   = 9.656716,
    Gamma   = 1.559824,
    Theta   = 0.2264882,
    Omega   = 2.979620e-06,
    p       = 0.7987374,
    q       = 1
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

    # Not in care ART initiation rate CD4 adjustment
    p[["s_1"]] <- 1 + ((1.335805 / 6) * 0)
    p[["s_2"]] <- 1 + ((1.335805 / 6) * 1)
    p[["s_3"]] <- 1 + ((1.335805 / 6) * 2)
    p[["s_4"]] <- 1 + ((1.335805 / 6) * 3)
    p[["s_5"]] <- 1 + ((1.335805 / 6) * 4)
    p[["s_6"]] <- 1 + ((1.335805 / 6) * 5)
    p[["s_7"]] <- 1 + ((1.335805 / 6) * 6)

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

test <- GetResult_CUSTOM(result)

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

####################################################################################################
####################################################################################################
####################################################################################################
# What if only people with CD4 <200 sought care faster...
# (1) Try scaling TOGETHER
# (2) Tease apart
# NUMBER ONE APPEARS TO BE THE BEST

par <- c(
    Rho     = 0.3148,
    Epsilon = 100,
    Kappa   = 0.0261,
    Gamma   = 0.8037,
    Theta   = 0.3992,
    Omega   = 0.0052,
    p       = 0.8375,
    q       = 0.9479,
    x       = 1
)

GetTestError <- function(par) {

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
    p[["s_5"]] <- par[["x"]]
    p[["s_6"]] <- par[["x"]]
    p[["s_7"]] <- par[["x"]]

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    return(sum(iOut[iOut$source == "error", "value"]))
}

res <- optim(par, GetTestError)
res
as.matrix(res$par)

result <- c(
    Rho     = 0.0007266552,
    Epsilon = 102.1607,
    Kappa   = 2.720689,
    Gamma   = 3.858887,
    Theta   = 0.2643222,
    Omega   = 0.003976591,
    p       = 0.793997,
    q       = 1,
    x       = 2.824565
)

# How does this compare to the OG calibration method bounds?
# parRange <- DefineParmRange(
    # rho     = c(0, 0.001),
    # q       = c(0.9, 1),
    # epsilon = c(100, 100),
    # kappa   = c(0, 3),
    # gamma   = c(0, 4),
    # theta   = c(0, 0.5),
    # p       = c(0.7, 1),
    # omega   = c(0, 0.01)
# )
# should be fine.

# error = 0.01115548
GetTestError(result)

GetTestErrorResult <- function(par) {

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
    p[["s_5"]] <- par[["x"]]
    p[["s_6"]] <- par[["x"]]
    p[["s_7"]] <- par[["x"]]

    iOut <- SSE(AssembleComparisonDataFrame(country = country, model = CallCalibModel(time, y, p, i), data = data))

    iOut
}

calibOut <- GetTestErrorResult(result)

graphics.off(); quartz.options(w = 10, h = 8)
BuildCalibDetailPlot_Thesis(
    data = calibOut,
    originalData = MasterData,
    limit = 1)
# quartz.save(file = "~/Desktop/fig/detail.pdf", type = "pdf")

graphics.off(); quartz.options(w = 9, h = 4)
BuildPHIAPlot(data = calibOut)
quartz.save(file = "~/Desktop/fig/PHIA.pdf", type = "pdf")


GetTestResult <- function(par) {

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
    p[["s_5"]] <- par[["x"]]
    p[["s_6"]] <- par[["x"]]
    p[["s_7"]] <- par[["x"]]

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

test <- GetTestResult(result)

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
grad <- calculate_median_cd4_at_initiation(dat = test)

ggplot(grad, aes(x = year, y = median)) + geom_point(aes(col = median))
