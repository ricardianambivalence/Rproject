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
# }}} close packages etc

# {{{ PATHstuff
projPATH <- file.path("~/R/aud/alm")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}close PATHs

# {{{get data and mash
load(file.path(dataPATH, "almT2.RData"))
load("~/r/aud/gdp/data/gdpTables.RData") # GDP tables

# make qtrly UR and nPop
lmkt_q <- apply.quarterly(xtsF(absT2[, c('date', 'ur', 'nPop')]), colMeans)
lmkt_q <- lmkt_q[(((as.POSIXlt(index(lmkt_q))$mon +1) %% 3) == 0), ]
lmkt_q_1yD <- diff(lmkt_q, 4)

# extract the six demand measures
ADx <- cbind(gdpT2[, c('domD_sa', 'GNE_sa', 'gdp_sa')],
             gdpT20[, 'rNfGdp_sa'],
             gdpT1[, c('nGDP_sa', 'rGDI_sa')])
ADx_y <- 100 * (ADx / lag(ADx, 4) - 1)

PCframe <- merge(lmkt_q_1yD, lag(ADx_y, 1))
PCframe_df <- as.data.frame(PCframe['1984::'])
PCframe_df$year <- as.POSIXlt(index(ADx['1984::']))$year + 1900
PCframe_df$split <- '84-93'
PCframe_df[PCframe_df$year %in% 1994:2003, 'split'] <- '94-03'
PCframe_df[PCframe_df$year %in% 2004:2013, 'split'] <- '04-13'
PCframe_df$split <- factor(PCframe_df$split, levels = c('84-93', '94-03', '04-13'),
                             ordered = TRUE)
currentDmd <- as.data.frame(tail(ADx_y, 1))
# }}} close data-mash

# {{{plots
# DD --> unemployment rate
gp_Dd2UR <- ggplot(PCframe_df[, c('ur', 'domD_sa', 'split')],
                   aes(x = domD_sa, y = ur, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in UR ~ real Dom Demand (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('ur', 'domD_sa')], 1),
                           aes(x = domD_sa, y = ur), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$domD_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("dd2UR.png")
grid.arrange(gp_Dd2UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
#
# DD --> n-pop
gp_Dd2nPop <- ggplot(PCframe_df[, c('nPop', 'domD_sa', 'split')],
                   aes(x = domD_sa, y = nPop, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in nPop ~ real Dom Demand (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('nPop', 'domD_sa')], 1),
                           aes(x = domD_sa, y = nPop), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$domD_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("dd2nPop.png")
grid.arrange(gp_Dd2nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# GNE --> unemployment rate
gp_gne2UR <- ggplot(PCframe_df[, c('ur', 'GNE_sa', 'split')],
                   aes(x = GNE_sa, y = ur, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in UR ~ real GNE (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('ur', 'GNE_sa')], 1),
                           aes(x = GNE_sa, y = ur), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$GNE_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("gne2UR.png")
grid.arrange(gp_gne2UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
#
# GNE --> n-pop
gp_gne2nPop <- ggplot(PCframe_df[, c('nPop', 'GNE_sa', 'split')],
                   aes(x = GNE_sa, y = nPop, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in nPop ~ real GNE (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('nPop', 'GNE_sa')], 1),
                           aes(x = GNE_sa, y = nPop), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$GNE_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("gne2nPop.png")
grid.arrange(gp_gne2nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# gdp --> unemployment rate
gp_gdp2UR <- ggplot(PCframe_df[, c('ur', 'gdp_sa', 'split')],
                   aes(x = gdp_sa, y = ur, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in UR ~ real gdp (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('ur', 'gdp_sa')], 1),
                           aes(x = gdp_sa, y = ur), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$gdp_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("gdp2UR.png")
grid.arrange(gp_gdp2UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
#
# gdp --> n-pop
gp_gdp2nPop <- ggplot(PCframe_df[, c('nPop', 'gdp_sa', 'split')],
                   aes(x = gdp_sa, y = nPop, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in nPop ~ real gdp (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('nPop', 'gdp_sa')], 1),
                           aes(x = gdp_sa, y = nPop), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$gdp_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("gdp2nPop.png")
grid.arrange(gp_gdp2nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# rNfGdp --> unemployment rate
gp_rNfGdp2UR <- ggplot(PCframe_df[, c('ur', 'rNfGdp_sa', 'split')],
                   aes(x = rNfGdp_sa, y = ur, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in UR ~ real rNfGdp (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('ur', 'rNfGdp_sa')], 1),
                           aes(x = rNfGdp_sa, y = ur), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$rNfGdp_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("rNfGdp2UR.png")
grid.arrange(gp_rNfGdp2UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
#
# rNfGdp --> n-pop
gp_rNfGdp2nPop <- ggplot(PCframe_df[, c('nPop', 'rNfGdp_sa', 'split')],
                   aes(x = rNfGdp_sa, y = nPop, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in nPop ~ real rNfGdp (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('nPop', 'rNfGdp_sa')], 1),
                           aes(x = rNfGdp_sa, y = nPop), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$rNfGdp_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("rNfGdp2nPop.png")
grid.arrange(gp_rNfGdp2nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# nGDP --> unemployment rate
gp_nGDP2UR <- ggplot(PCframe_df[, c('ur', 'nGDP_sa', 'split')],
                   aes(x = nGDP_sa, y = ur, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in UR ~ real nGDP (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('ur', 'nGDP_sa')], 1),
                           aes(x = nGDP_sa, y = ur), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$nGDP_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("nGDP2UR.png")
grid.arrange(gp_nGDP2UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
#
# nGDP --> n-pop
gp_nGDP2nPop <- ggplot(PCframe_df[, c('nPop', 'nGDP_sa', 'split')],
                   aes(x = nGDP_sa, y = nPop, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in nPop ~ real nGDP (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('nPop', 'nGDP_sa')], 1),
                           aes(x = nGDP_sa, y = nPop), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$nGDP_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("nGDP2nPop.png")
grid.arrange(gp_nGDP2nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# rGDI --> unemployment rate
gp_rGDI2UR <- ggplot(PCframe_df[, c('ur', 'rGDI_sa', 'split')],
                   aes(x = rGDI_sa, y = ur, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in UR ~ real rGDI (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('ur', 'rGDI_sa')], 1),
                           aes(x = rGDI_sa, y = ur), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$rGDI_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("rGDI2UR.png")
grid.arrange(gp_rGDI2UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
#
# rGDI --> n-pop
gp_rGDI2nPop <- ggplot(PCframe_df[, c('nPop', 'rGDI_sa', 'split')],
                   aes(x = rGDI_sa, y = nPop, color = split)) +
                labs(y = NULL, x = NULL) +
                labs(title = "1yr change in nPop ~ real rGDI (lagged 1qtr)") +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_point(data = tail(PCframe_df[, c('nPop', 'rGDI_sa')], 1),
                           aes(x = rGDI_sa, y = nPop), color = 'red', pch = 19, size = 2) +
                geom_smooth(method = 'glm') +
                geom_vline(xintercept = currentDmd$rGDI_sa, linetype="dotted", color = 'black',
                           lwd=1)
pngMk("rGDI2nPop.png")
grid.arrange(gp_rGDI2nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
# }}}close plots
