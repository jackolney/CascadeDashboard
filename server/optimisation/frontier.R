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
