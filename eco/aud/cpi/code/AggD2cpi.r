#{{{set-up Packs and Funs
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
#packages and functions
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(quantmod)
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")
# }}}

# {{{ PATHstuff
projPATH <- file.path("~/R/aud/cpi")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "pics")
# }}}

# {{{ data stuff
load("~/R/AUD/cpi/data/longCores.rdata") # RBA and ABS core CPI splice
load("~/r/aud/gdp/data/gdpTables.RData") # GDP tables

ADx <- cbind(gdpT2[, c('domD_sa', 'GNE_sa', 'gdp_sa')],
             gdpT20[, 'rNfGdp_sa'],
             gdpT1[, c('nGDP_sa', 'rGDI_sa')])
ADx_y <- 100 * (ADx / lag(ADx, 4) - 1)

AD2CPI_df <- as.data.frame(merge(longCores$CPI_TMy, lag(ADx_y,4))['1984::'])
AD2CPI_df$year <- as.POSIXlt(index(ADx['1984::']))$year + 1900
AD2CPI_df$split <- '84-93'
AD2CPI_df[AD2CPI_df$year %in% 1994:2003, 'split'] <- '94-03'
AD2CPI_df[AD2CPI_df$year %in% 2004:2013, 'split'] <- '04-13'
AD2CPI_df$split <- factor(AD2CPI_df$split, levels = c('84-93', '94-03', '04-13'),
                             ordered = TRUE)
currentDmd <- as.data.frame(tail(ADx_y, 1))
# }}} done data stuff

# {{{ plots
# domestic demand
gp_Dd2TM <- ggplot(AD2CPI_df[, c('CPI_TMy', 'domD_sa', 'split')],
                   aes(x = domD_sa, y = CPI_TMy, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ real Dom Demand (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'domD_sa')], 1),
                           aes(x = domD_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$domD_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("dd2tmCPI.png")
grid.arrange(gp_Dd2TM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# GNE
gp_GNE2TM <- ggplot(AD2CPI_df[, c('CPI_TMy', 'GNE_sa', 'split')],
                   aes(x = GNE_sa, y = CPI_TMy, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ real GNE (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'GNE_sa')], 1),
                           aes(x = GNE_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$GNE_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("GNE2tmCPI.png")
grid.arrange(gp_GNE2TM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# gdp
gp_GDP2TM <- ggplot(AD2CPI_df[, c('CPI_TMy', 'gdp_sa', 'split')],
                   aes(x = gdp_sa, y = CPI_TMy, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ real GDP (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'gdp_sa')], 1),
                           aes(x = gdp_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$gdp_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("GDP2tmCPI.png")
grid.arrange(gp_GDP2TM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# non-farm gdp
gp_nfGDP2TM <- ggplot(AD2CPI_df[, c('CPI_TMy', 'rNfGdp_sa', 'split')],
                   aes(x = rNfGdp_sa, y = CPI_TMy, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ real non-farm GDP (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'rNfGdp_sa')], 1),
                           aes(x = rNfGdp_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$rNfGdp_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("nfGDP2tmCPI.png")
grid.arrange(gp_nfGDP2TM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# nGDP
gp_nGDP2TM <- ggplot(AD2CPI_df[, c('CPI_TMy', 'nGDP_sa', 'split')],
                   aes(x = nGDP_sa, y = CPI_TMy, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ nominal GDP (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'nGDP_sa')], 1),
                           aes(x = nGDP_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$nGDP_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("nGDP2tmCPI.png")
grid.arrange(gp_nGDP2TM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# nGDP
gp_nGDP2TMpool <- ggplot(subset(AD2CPI_df[, c('CPI_TMy', 'nGDP_sa', 'split')],
                            split %in% c('94-03', '04-13')),
                   aes(x = nGDP_sa, y = CPI_TMy)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ nominal GDP (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point(type = 3, color = 'orange') +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'nGDP_sa')], 1),
                           aes(x = nGDP_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$nGDP_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("nGDP2tmCPIpool.png")
grid.arrange(gp_nGDP2TMpool, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# rGDI
gp_rGDI2TM <- ggplot(AD2CPI_df[, c('CPI_TMy', 'rGDI_sa', 'split')],
                   aes(x = rGDI_sa, y = CPI_TMy, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Trimmed Mean CPI ~ real GDI (lag 4qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(AD2CPI_df[, c('CPI_TMy', 'rGDI_sa')], 1),
                           aes(x = rGDI_sa, y = CPI_TMy), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = "glm") +
                geom_vline(xintercept = currentDmd$rGDI_sa, linetype="dotted", color = 'orange',
                           lwd=1)
pngMk("rGDI2tmCPI.png")
grid.arrange(gp_rGDI2TM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
# }}} end plots
