rm(list=ls()); gc()

## {{{ packages and functions
require(reshape2)
require(ggplot2)
require(xts)
require(gridExtra)
require(RColorBrewer)
source("~/R/Rhelpers/helperFuncts.r")
# }}}

## {{{ PATH stuff
projectPATH <- "~/R/AUD/rba"
plotPATH <- file.path(projectPATH, "pics")
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")
# }}}

# {{{ load data
load("~/R/AUD/rba/data/somp_cpi.rdata")
load("~/R/AUD/rba/data/somp_gdp.rdata")
# }}}

# color palettes
RApal3 <- brewer.pal(3, "RdYlBu")

gg <- ggplot((somp_gdp), aes(date)) +
  geom_line(aes(y = actual, colour = "black", size = 1.5)) +
  geom_line(aes(y = SOMP_q2.2013, colour = "0xAF8DC3", size = 1.25)) +
  geom_line(aes(y = SOMP_q1.2013, colour = "0x7FBF7B"))


gp_sompGDP <- ggplot(data = melt(somp_gdp, measure.vars = c(4:2)),
       aes(x=date, y=value, colour=variable)) +
    geom_line(size = 1.25) +
    scale_colour_manual(values = RApal3) +
    labs(title = "RBA SOMP Forecasts: GDP %YoY") +
    labs(y = NULL, x = NULL) +
    theme(legend.position = 'right') +
    theme(legend.title = element_blank())
png(file.path(plotPATH, "gp_sompGDP.png"))
grid.arrange(gp_sompGDP, sub = textGrob('www.ricardianambivalence.com'))
dev.off()


gp_sompCPI <- ggplot(data = melt(somp_cpi, measure.vars = c(4:2)),
       aes(x=date, y=value, colour=variable)) +
    geom_line(size = 1.25) +
    scale_colour_manual(values = RApal3) +
    labs(title = "RBA SOMP Forecasts: CPI %YoY") +
    ylim(2,3) +
    labs(y = NULL, x = NULL) +
    theme(legend.position = 'right') +
    theme(legend.title = element_blank())
png(file.path(plotPATH, "gp_sompCPI.png"))
grid.arrange(gp_sompCPI, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
