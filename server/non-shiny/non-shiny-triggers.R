# non-shiny optimisation triggers and setup
# input stuff etc.
graphics.off()
quartz.options(w = 10, h = 5)

source("server/model/baseline-model.R",                local = FALSE)
source("server/model/best-fit-model.R",                local = FALSE)
source("server/model/beta.R",                          local = FALSE)
source("server/model/initial.R",                       local = FALSE)
source("server/model/parameters.R",                    local = FALSE)
source("server/non-shiny/non-shiny-optimisation.R",    local = FALSE)
source("server/optimisation/functions.R",              local = FALSE)
source("server/optimisation/parameters.R",             local = FALSE)
source("server/optimisation/plot-functions.R",         local = FALSE)
source("server/optimisation/sim.R",                    local = FALSE)
source("server/projection/CD4-distribution.R",         local = FALSE)

# reactive input setup
MasterCD4_2015 <- GetCD4Distribution2015("Kenya")
MasterData <- GetCountryData("Kenya")

intSwitch <- data.frame(
    testing =      TRUE,
    linkage =      TRUE,
    preRetention = TRUE,
    initiation =   TRUE,
    adherence =    TRUE,
    retention =    TRUE
    )

OptInput <- c()
OptInput$intValue_rho   <- parRange["rho", "max"]
OptInput$intValue_q     <- parRange["q", "max"]
OptInput$intValue_kappa <- parRange["kappa", "min"]
OptInput$intValue_gamma <- parRange["gamma", "max"]
OptInput$intValue_sigma <- 0.1
OptInput$intValue_omega <- parRange["rho", "min"]

# ------------ #
# OPTIMISATION #
# ------------ #

BuildCalibrationBestFitRunsPlot(data = CalibOut, originalData = KenyaData, limit = 100, minErrorRun = minErrorRun, selectedRuns = selectedRuns, propRuns = 0.1)

theOut <- RunNSOptimisation(propRuns = 0.1)
theOut

a = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Rho), alpha = 0.2) + theme_minimal()
b = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Q), alpha = 0.2) + theme_minimal()
c = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Kappa), alpha = 0.2) + theme_minimal()
d = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Gamma), alpha = 0.2) + theme_minimal()
e = ggplot(theOut, aes(x = VS, y = Cost)) + geom_point(aes(col = Sigma), alpha = 0.2) + theme_minimal()

gridExtra::grid.arrange(a, b, c, d, e, ncol = 2, nrow = 3)

# See one big data.frame may not be the best solution.

# We want to calculate the frontier, or approx the function, for EACH of the (j) parameter sets run.

# What if we just take one simulation
dim(theOut)
one <- theOut[1:64,]
ggplot(one, aes(x = VS, y = Cost)) + geom_point()


# Approximations
x = one$VS
y = one$Cost

approx(x = x, y = y)


plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")


f <- approxfun(x, y)
curve(f(x), 0, 1, col = "green2")
points(x, y)
is.function(fc <- approxfun(x, y, method = "const")) # TRUE
curve(fc(x), 0, 10, col = "darkblue", add = TRUE)
## different extrapolation on left and right side :
plot(approxfun(x, y, rule = 2:1), 0, 1,
     col = "tomato", add = TRUE, lty = 3, lwd = 2)

# The problem is that we need to isolate the frontier first...

res <- data.frame(x = one$VS, y = one$Cost)

plot(res)

a <- diff(res$y) / diff(res$x)

order(a)

res[32,]
plot(res)
points(res[32,], col = 'red')

# Sort by cost (y)
res[order(res$y),]

# pick cheapest (cost == 0)
res[37,]

# calculate gradient to all other points
diff(c(res[37,], res))

c <- diff(c(res[37,2], res[,2])) / diff(c(res[37,1], res[,1]))

plot(c)

grad <- (res[37,2] - res[,2]) / (res[37,1] - res[,1])

plot(grad)

min(grad, na.rm = TRUE)

which(grad == min(grad, na.rm = TRUE))

res[53,]


plot(res)
points(res[37,], col = 'green')
points(res[53,], col = 'red')

# okay, done.

# Just need to repeat and place into a function

FindFrontier <- function(x, y) {
    # Create data.frame of x and y
    df <- data.frame(x = x, y = y)
    # Zero the index vector
    frontierIndex <- c()
    # Finding the cost frontier
    rankCost <- order(df$y)
    frontierIndex[1] <- rankCost[1]
    for (i in 1:dim(df)[1]) {
        # Remove rows on the frontier
        noFront <- df[-(frontierIndex),]
        # Only consider values with larger impact
        remain <- noFront[noFront$x > max(df[frontierIndex,1]),]
        # break if remain is empty
        if (dim(remain)[1] == 0) break;
        # calculate gradient of last point on frontier to all remaining
        grad <- (df[frontierIndex[i],2] - remain[,2]) / (df[frontierIndex[i],1] - remain[,1])
        # calculate gradient to all points, everywhere
        ref <- (df[frontierIndex[i],2] - df[,2]) / (df[frontierIndex[i],1] - df[,1])
        # find the smallest, non-zero gradient from those remaining and pin-point
        # it's index in the whole data.frame
        frontierIndex[i+1] <- which(ref == min(grad[grad >= 0], na.rm = TRUE))
    }
    frontierIndex
}

FindFrontierPlot <- function(x, y) {
    # Create data.frame of x and y
    df <- data.frame(x = x, y = y)
    # Zero the index vector
    frontierIndex <- c()
    # Finding the cost frontier
    rankCost <- order(df$y)
    frontierIndex[1] <- rankCost[1]
    for (i in 1:dim(df)[1]) {
        # Remove rows on the frontier
        noFront <- df[-(frontierIndex),]
        # Only consider values with larger impact
        remain <- noFront[noFront$x > max(df[frontierIndex,1]),]
        # break if remain is empty
        if (dim(remain)[1] == 0) break;
        # calculate gradient of last point on frontier to all remaining
        grad <- (df[frontierIndex[i],2] - remain[,2]) / (df[frontierIndex[i],1] - remain[,1])
        # calculate gradient to all points, everywhere
        ref <- (df[frontierIndex[i],2] - df[,2]) / (df[frontierIndex[i],1] - df[,1])
        # find the smallest, non-zero gradient from those remaining and pin-point
        # it's index in the whole data.frame
        frontierIndex[i+1] <- which(ref == min(grad[grad >= 0], na.rm = TRUE))
    }
    ggplot(df, aes(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    theme_minimal() +
    geom_line(data = df[frontierIndex,], aes(x = x, y =y), col = "red", alpha = 0.5) +
    geom_point(data = df[frontierIndex,], aes(x = x, y =y), col = "red", alpha = 0.5)
}

one <- theOut[1:64,]
FindFrontier(x = one$VS, y = one$Cost)
FindFrontierPlot(x = one$VS, y = one$Cost)

repeats <- dim(theOut)[1] / 64

for(m in 1:repeats) {
    lower <- (1 + 64 * (m - 1))
    upper <- (64 + 64 * (m - 1))
    vals <- theOut[lower:upper,]
    assign(letters[m],FindFrontierPlot(x = vals$VS, y = vals$Cost))
}

gridExtra::grid.arrange(a,b,c,d,e,f,g,h,i,j,ncol = 2, nrow = 5)

range(one[,"90"])
range(one[,"90-90"])
range(one[,"90-90-90"])

