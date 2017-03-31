# Combination figure generation for Zimbabwe and DRC calibrations

# 90-90-90 Figure
# load the *out.RData files

# DRC
load("../../formal/DRC/UNAIDS-90-90-90.RData")
DRC_out

# Zimbabwe
load("../../formal/zimbabwe/UNAIDS-90-90-90.RData")
zimbabwe_out

# What does the standard 90-90-90 figure look like?
DRC_out$country <- "DRC"
zimbabwe_out$country <- "Zimbabwe"

out <- rbind(DRC_out, zimbabwe_out)
out$country <- factor(out$country, levels = c("Zimbabwe", "DRC"))

# FIGURE CONTROL
graphics.off(); quartz.options(w = 9, h = 4)
ggOut <- ggplot(out, aes(x = def, y = res, group = country))
ggOut <- ggOut + geom_bar(aes(fill = country), position = 'dodge', stat = 'identity')
ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2,
    size = 0.5, position = position_dodge(0.9))
ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
ggOut <- ggOut + scale_fill_brewer(palette = "Set1")
ggOut <- ggOut + geom_abline(intercept = 0.9, slope = 0)
ggOut <- ggOut + theme_classic()
ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
ggOut <- ggOut + theme(title = element_text(size = 20))
ggOut <- ggOut + theme(axis.title = element_blank())
ggOut <- ggOut + theme(axis.ticks.x = element_blank())
ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
ggOut <- ggOut + theme(legend.position = "right")
ggOut <- ggOut + theme(legend.title = element_blank())
ggOut <- ggOut + theme(plot.background = element_blank())
ggOut <- ggOut + theme(panel.background = element_blank())
ggOut <- ggOut + theme(axis.line.y = element_line())
ggOut <- ggOut + theme(text = element_text(family = figFont))
ggOut <- ggOut + geom_label(aes(x = def, label = scales::percent(round(out$res, digits = 2))),
    size = 3, position = position_dodge(0.9), label.padding = unit(0.2, "lines"))
ggOut
quartz.save(file = "../../formal/DRC/fig/pro/90-90-90-combo.pdf", type = "pdf")
