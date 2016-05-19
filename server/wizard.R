# This is currently not in use.
# REMOTE_plot909090_wizard <- renderPlot({
#     out <- CallModel()
#     PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
#     # dx / PLHIV
#     dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
#     # tx / dx
#     tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
#     # vs / tx
#     vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))

#     p_dx <- dx / PLHIV
#     p_tx <- tx / dx
#     p_vs <- vs / tx

#     results <- c(p_dx,p_tx,p_vs)
#     definition <- c("% Diagnosed","% On Treatment","% Suppressed")
#     Scenario <- c("Baseline")
#     the909090 <- data.frame(definition,results,Scenario)

#     levels(the909090$definition)
#     the909090$definition <- factor(the909090$definition, levels = c("% Diagnosed","% On Treatment","% Suppressed"))

#     red <- rgb(red = 223, green = 74, blue = 50, max = 255)
#     yellow <- rgb(red = 245, green = 157, blue = 0, max = 255)
#     green <- rgb(red = 0, green = 167, blue = 87, max = 255)
#     fill.coll <- c(red,yellow,green)

#     o <- ggplot(the909090,aes(definition,results))
#     o <- o + geom_bar(aes(fill = definition), position = 'dodge', stat = 'identity')
#     o <- o + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), labels = percent)
#     o <- o + scale_fill_manual(values = fill.coll)
#     o <- o + geom_abline(intercept = 0.9, slope = 0)
#     o <- o + theme_classic()
#     o <- o + theme(title = element_text(size = 20))
#     o <- o + theme(axis.title = element_blank())
#     o <- o + theme(axis.text.x = element_text(size = 18))
#     o <- o + theme(axis.text.y = element_text(size = 18))
#     o <- o + theme(legend.position = "none")
#     print(o)

#     print("HELLO?")
#     },
#     height = 500,
#     width = 'auto'
# )
