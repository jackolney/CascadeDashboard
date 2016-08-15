CheckInterpolation <- function(propRuns, intLength) {

    theOut <- RunNSOptimisation(propRuns = 0.1, intLength = intLength)

    simLength <- dim(GetParaMatrixRun(cParamOut = CalibParamOut, runNumber = 1, length = intLength))[1]

    optRuns <- WhichAchieved73(simData = theOut, simLength = simLength)

    frontierList <- GetFrontiers(simData = theOut, optRuns = optRuns, simLength = simLength)

    output <- RunInterpolation(simData = theOut, optRuns = optRuns, simLength = simLength, frontierList = frontierList)

    results <- c(
        Cost = mean(output[,"iCost"]),
        Test = mean(output[,"iTest"]),
        Link = mean(output[,"iLink"]),
        PreR = mean(output[,"iPreR"]),
        Init = mean(output[,"iInit"]),
        Adhr = mean(output[,"iAdhr"]),
        Retn = mean(output[,"iRetn"])
    )
    results
}

Two <- CheckInterpolation(propRuns = 0.1, intLength = 2)
Three <- CheckInterpolation(propRuns = 0.1, intLength = 3)
Four <- CheckInterpolation(propRuns = 0.1, intLength = 4)

test <- reshape2::melt(cbind(Two, Three))
test2 <- test[test$Var1 != "Cost",]

ggplot(test2, aes(x = Var1, y = value, group = Var2)) + geom_point(aes(col = Var2))

# Make plot then deploy on the cluster for simulation.
graphics.off()
quartz.options(w = 8, h = 5)

test2$value <- abs(test2$value)

ggplot(test2, aes(x = Var1, y = value, group = Var2)) +
geom_point(aes(col = Var2), size = 3, alpha = 0.5) +
theme_minimal() +
theme(axis.line.y = element_line()) +
theme(axis.line.x = element_line())
