CalcDifTo90 <- function(result) {
    res_list <- matrix(0, dim(result)[1], 5)
    colnames(res_list) <- c("sim", "90", "90-90", "90-90-90", "total")
    for (i in 1:dim(result)[1]) {
        out <- rep(0, 5)
        out[1] <- i
        out[2] <- result[i,"90"]       - 0.9
        out[3] <- result[i,"90-90"]    - 0.9
        out[4] <- result[i,"90-90-90"] - 0.9
        out[5] <- sum(out[2], out[3], out[4])
        res_list[i,] <- out
    }
    return(res_list)
}

QuickRouteTo909090 <- function(result) {
    # order 90-90-90 by the sum of distance to 90%
    int <- result[order(-result[,"total"]),]

    # Identify simulation closest to the 90% mark.
    x <- 1
    while (int[[x, "90"]] > 0 & int[[x, "90-90"]] > 0 & int[[x, "90-90-90"]] > 0) {x <- x + 1}
    if (x == 1) {
        int[[x, "sim"]]
    } else {
        int[[x - 1, "sim"]]
    }
}

CanWeAchieve909090 <- function(result) {
    if (max(result[,"90"]) >= 0.9) {
        if (max(result[,"90-90"]) >= 0.9) {
            if (max(result[,"90-90-90"]) >= 0.9) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}

Get909090 <- function(result) {
    if (CanWeAchieve909090(result)) {
        message("Achieved 90-90-90 - showing cheapest route.")
        # Then subset the results that hit 90-90-90 and pass out the cheapest solution.
        first <- subset(result, result[,"90"] >= 0.9)
        second <- subset(first, first[,"90-90"] >= 0.9)
        third <- subset(second, second[,"90-90-90"] >= 0.9)

        # Return results of simulation (inc parameter values) for cheapest to achieve 90-90-90
        third[order(third[,"Cost"]),][1,]
    } else {
        message("Failed 90-90-90 - showing closest.")
        # Update ValueBoxes???
        res_list <- CalcDifTo90(result)
        result[QuickRouteTo909090(res_list),]
    }
}


# Get909090(theOut)


output$vb909090_1 <- renderValueBox({

    res <- Get909090(optResult)

    out <- round(res[,"90"], digits = 2)

    if (out >= 0.9) {
        valueBox(
            value = scales::percent(out),
            subtitle = "Diagnosed",
            color = "green",
            icon = icon("check", lib = "font-awesome")
        )
    } else {
        valueBox(
            value = scales::percent(out),
            subtitle = "Diagnosed",
            color = "red",
            icon = icon("times", lib = "font-awesome")
        )
    }
  })

output$vb909090_2 <- renderValueBox({

    res <- Get909090(optResult)

    out <- round(res[,"90-90"], digits = 2)

    if (out >= 0.9) {
        valueBox(
            value = scales::percent(out),
            subtitle = "On Treatment",
            color = "green",
            icon = icon("check", lib = "font-awesome")
        )
    } else {
        valueBox(
            value = scales::percent(out),
            subtitle = "On Treatment",
            color = "red",
            icon = icon("times", lib = "font-awesome")
        )
    }
  })

output$vb909090_3 <- renderValueBox({

    res <- Get909090(optResult)

    out <- round(res[,"90-90-90"], digits = 2)

    if (out >= 0.9) {
        valueBox(
            value = scales::percent(out),
            subtitle = "Virally Suppressed",
            color = "green",
            icon = icon("check", lib = "font-awesome")
        )
    } else {
        valueBox(
            value = scales::percent(out),
            subtitle = "Virally Suppressed",
            color = "red",
            icon = icon("times", lib = "font-awesome")
        )
    }
  })
