# ------------------ #
# OPTIMISATION PLOTS #
# ------------------ #

# Plot 1
output$plotOpt909090 <- renderPlot({
    # dependency on optimStart
    input$optimStart

    Legend.Labels <- c()
    for (i in 1:length(levels(as.factor(Result_909090[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_909090[[input$userStratPoint]]))[i]),2)
    }

    # Determining which interventions achieved 90-90-90
    theResult <- mutate(Result_909090, the909090 = 0)

    for (i in 1:dim(theResult)[1]) {
        if (theResult[i,1] >= 0.9) {
            if (theResult[i,2] >= 0.9) {
                if (theResult[i,3] >= 0.9) {
                    theResult$the909090[i] <- 1
                }
            }
        }
    }

    theStratPoint <<- input$userStratPoint

    ggOut <- ggplot(theResult, aes(x = VS, y = Cost))
    ggOut <- ggOut + geom_point(aes(color = as.factor(get(theStratPoint)), size = as.factor(the909090)))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_color_discrete(name = input$userStratPoint, labels = Legend.Labels)
    ggOut <- ggOut + scale_size_discrete(name = "Achieves 90-90-90", range = c(3, 6), labels = c("no","yes"))
    ggOut <- ggOut + guides(colour = guide_legend(override.aes = list(size = 4)))
    ggOut <- ggOut + theme(legend.title = element_text(size = 15))
    ggOut <- ggOut + theme(legend.text = element_text(size = 13))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
    ggOut <- ggOut + theme(axis.title = element_text(size = 18))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + geom_vline(xintercept = 0.9^3)
    ggOut <- ggOut + xlab("Proportion achieving viral suppression by 2020")
    ggOut <- ggOut + ylab("Additional cost of care (2013 USD)")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma)
    ggOut <- ggOut + scale_x_continuous(labels = scales::comma)
    ggOut <- ggOut + coord_cartesian(xlim = plotOpt_909090.ranges$x, ylim = plotOpt_909090.ranges$y)
    ggOut
    },
    height = 400,
    width = 'auto'
)

# Plot 2
output$plotOptDALYs <- renderPlot({
    # dependency on optimStart
    input$optimStart

    Legend.Labels <- c()
    for (i in 1:length(levels(as.factor(Result_DALYs[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_DALYs[[input$userStratPoint]]))[i]),2)
    }

    theStratPoint <<- input$userStratPoint

    ggOut <- ggplot(Result_DALYs,aes(x = DALYs, y = Cost))
    ggOut <- ggOut + geom_point(aes(color = as.factor(get(theStratPoint))), size = 5)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_color_discrete(name = input$userStratPoint, labels = Legend.Labels)
    ggOut <- ggOut + guides(colour = guide_legend(override.aes = list(size=4)))
    ggOut <- ggOut + theme(legend.title = element_text(size = 15))
    ggOut <- ggOut + theme(legend.text = element_text(size = 13))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
    ggOut <- ggOut + theme(axis.title = element_text(size = 18))
    ggOut <- ggOut + xlab("DALYs Averted (between 2015 and 2020)")
    ggOut <- ggOut + ylab("Additional cost of care (2013 USD)")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma)
    ggOut <- ggOut + scale_x_continuous(labels = scales::comma)
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + coord_cartesian(xlim = plotOpt_DALYs.ranges$x, ylim = plotOpt_DALYs.ranges$y)
    ggOut
    },
    height = 400,
    width = 'auto'
)

# Plot 3
output$plotOptDALYs909090 <- renderPlot({
    # dependency on optimStart
    input$optimStart

    Legend.Labels <- c()
    for (i in 1:length(levels(as.factor(Result_DALYs_909090[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_DALYs_909090[[input$userStratPoint]]))[i]),2)
    }

    theStratPoint <<- input$userStratPoint

    ggOut <- ggOut + ggplot(Result_DALYs_909090,aes(x = DALYs, y = Cost))
    ggOut <- ggOut + geom_point(aes(color = as.factor(get(theStratPoint))), size = 5)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_color_discrete(name = input$userStratPoint, labels = Legend.Labels)
    ggOut <- ggOut + guides(colour = guide_legend(override.aes = list(size = 4)))
    ggOut <- ggOut + theme(legend.title = element_text(size = 15))
    ggOut <- ggOut + theme(legend.text = element_text(size = 13))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
    ggOut <- ggOut + theme(axis.title = element_text(size = 18))
    ggOut <- ggOut + xlab("DALYs Averted (between 2015 and 2020)")
    ggOut <- ggOut + ylab("Additional cost of care (2013 USD)")
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma)
    ggOut <- ggOut + scale_x_continuous(labels = scales::comma)
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + coord_cartesian(xlim = plotOpt_DALYs_909090.ranges$x, ylim = plotOpt_DALYs_909090.ranges$y)
    ggOut
    },
    height = 400,
    width = 'auto'
)
