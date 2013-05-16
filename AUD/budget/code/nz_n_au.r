rm(list=ls()); gc()

# {{{ packages and functions
require(ggplot2)
require(reshape2)
require(gridExtra)
require(xts)
source('~/R/Rhelpers/helperFuncts.r')
source('~/R/Rhelpers/RAcolorpal.r')
# }}}

# {{{ PATH stuff
projectPATH <- "~/R/aud/budget/"
dataPATH <- file.path(projectPATH, "data")
codePATH <- file.path(projectPATH, "code")
plotPATH <- file.path(projectPATH, "Rpics")
# }}}

# {{{ load data
load(file.path(dataPATH, "au_z_tot.rdata"))
load(file.path(dataPATH, "au_nz_govtX.rdata"))
load(file.path(dataPATH, "au_nz_govtR.rdata"))
load(file.path(dataPATH, "audnzdFX.rdata"))
load(file.path(dataPATH, "nzdTWI.rdata"))
load(file.path(dataPATH, "audfx.rdata"))
# }}}


# {{{ plot stuff

## {{{ AUD FX stuff
gp_Lineaud <- ggplot(data = meltx(audFX['2000::']),
                 aes(x = date, y = value)) +
            geom_line(size = 0.95, color = 'orange') +
            labs(y = NULL, x = NULL) +
            labs(title = "AUDUSD Exchange rate")
png(file.path(plotPATH, "AUDUSDfx.png"))
grid.arrange(gp_Lineaud, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

gp_HistDiff.5d <- ggplot(data = meltx(100*diff(audFX, 5, log=TRUE)),
                 aes(x = value)) +
            geom_histogram(aes(y = ..density..),
                           binwidth = 0.5,
                           color = 'black',
                           fill = 'white') +
            geom_vline(xintercept = as.numeric(last(100*(diff(audFX, 5, log=T)))),
                       color = 'red', size = 1.2, linetype = 'dashed') +
            labs(y = NULL, x = NULL) +
            labs(title = "AUDUSD Exchange rate: % change 5 trading days")
png(file.path(plotPATH, "AUDUSD_5d.png"))
grid.arrange(gp_HistDiff.5d, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

gp_HistDiff.10d <- ggplot(data = meltx(100*diff(audFX, 10, log=TRUE)),
                 aes(x = value)) +
            geom_histogram(aes(y = ..density..),
                           binwidth = 0.5,
                           color = 'black',
                           fill = 'white') +
            geom_vline(xintercept = as.numeric(last(100*(diff(audFX, 10, log=T)))),
                       color = 'red', size = 1.2, linetype='dashed') +
            labs(y = NULL, x = NULL) +
            labs(title = "AUDUSD Exchange rate: % change 10 trading days")
png(file.path(plotPATH, "AUDUSD_10d.png"))
grid.arrange(gp_HistDiff.10d, sub = textGrob('www.ricardianambivalance.com'))
dev.off()


gp_HistDiff.20d <- ggplot(data = meltx(100*diff(audFX, 20, log=TRUE)),
                 aes(x = value)) +
            geom_histogram(aes(y = ..density..),
                           binwidth = 0.5,
                           color = 'black',
                           fill = 'white') +
            geom_vline(xintercept = as.numeric(last(100*(diff(audFX, 20, log=T)))),
                       color = 'red', size = 1.2, linetype = 'dashed') +
            labs(y = NULL, x = NULL) +
            labs(title = "AUDUSD Exchange rate: % change 20 trading days")
png(file.path(plotPATH, "AUDUSD_20d.png"))
grid.arrange(gp_HistDiff.20d, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

gp_HistDiff.25d <- ggplot(data = meltx(100*diff(audFX, 25, log=TRUE)),
                 aes(x = value)) +
            geom_histogram(aes(y = ..density..),
                           binwidth = 0.5,
                           color = 'black',
                           fill = 'white') +
            geom_vline(xintercept = as.numeric(last(100*(diff(audFX, 25, log=T)))),
                       color = 'red', size = 1.2, linetype='dashed') +
            labs(y = NULL, x = NULL) +
            labs(title = "AUDUSD Exchange rate: % change 25 trading days")
png(file.path(plotPATH, "AUDUSD_25d.png"))
grid.arrange(gp_HistDiff.25d, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

# }}}

# prep some data -

govtX$NZ_exp <- govtX$NZ_exp / govtX$NZ_exp[1] * 100
govtX$AU_exp <- govtX$AU_exp / govtX$AU_exp[1] * 100
govtX <- govtX[, c('date', 'AU_exp', 'NZ_exp')]
govtXx <- xtsF(govtX)

govtR$NZ_rev <- govtR$NZ_rev / govtR$NZ_rev[1] * 100
govtR$AU_rev <- govtR$AU_rev / govtR$AU_rev[1] * 100
govtR <- govtR[, c('date', 'AU_rev', 'NZ_rev')]
govtRx <- xtsF(govtR)

# {{{ NZ and AUD ToT and budget stuff

col2 = c('orange', 'black')

gp_totIdx <- ggplot(data = subset(melt(dd, id.vars = 1),
                                  variable %in% c('atot_idx', 'ztot_idx')),
                    aes(x = date, y = value, color = variable), size = 1.2) +
                    labs(y = NULL, x = NULL) +
                    labs(title = "AUD and NZD Terms of trade") +
                    theme(legend.position = 'right') +
                    theme(legend.title = element_blank()) +
                    geom_line(size = 1.2) +
                    scale_colour_manual(values = col2) +
                    geom_vline(xintercept = as.numeric(as.POSIXct("2013-03-01")),
                               col = 'red', linetype = 'dotdash')
png(file.path(plotPATH, "AU_NZ_tot.png"))
grid.arrange(gp_totIdx, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

gp_totratio <- ggplot(data = subset(melt(dd, id.vars = 1),
                                  variable %in% c('totRatio')),
                    aes(x = date, y = value), size = 1.2) +
                    labs(y = NULL, x = NULL) +
                    labs(title = "AUD/NZD Terms of trade ratio") +
                    theme(legend.position = 'right') +
                    theme(legend.title = element_blank()) +
                    geom_line(size = 1.2) +
                    geom_vline(xintercept = as.numeric(as.POSIXct("2013-03-01")),
                               col = 'red', linetype = 'dotdash')
png(file.path(plotPATH, "AU_NZ_totRatio.png"))
grid.arrange(gp_totratio, sub = textGrob('www.ricardianambivalance.com'))
dev.off()


gp_GovtExp <- ggplot(data = subset(meltx(govtXx), variable %in% c('AU_exp', 'NZ_exp')),
                    aes(x = date, y = value, color = variable), size = 1.2) +
                    labs(y = NULL, x = NULL) +
                    labs(title = "AUD and NZD Govt Expenditure (FY2001-02 = 100)") +
                    theme(legend.position = 'right') +
                    theme(legend.title = element_blank()) +
                    geom_line(size = 1.2) +
                    scale_colour_manual(values = col2) +
                    geom_vline(xintercept = as.numeric(as.POSIXct("2013-03-01")),
                               col = 'red', linetype = 'dotdash')
png(file.path(plotPATH, "AU_NZ_GovtExp.png"))
grid.arrange(gp_GovtExp, sub = textGrob('www.ricardianambivalance.com'))
dev.off()


gp_GovtRev <- ggplot(data = subset(meltx(govtRx), variable %in% c('AU_rev', 'NZ_rev')),
                    aes(x = date, y = value, color = variable), size = 1.2) +
                    labs(y = NULL, x = NULL) +
                    labs(title = "AUD and NZD Govt Revenue (FY2001-02 = 100)") +
                    theme(legend.position = 'right') +
                    theme(legend.title = element_blank()) +
                    geom_line(size = 1.2) +
                    scale_colour_manual(values = col2) +
                    geom_vline(xintercept = as.numeric(as.POSIXct("2013-03-01")),
                               col = 'red', linetype = 'dotdash')
png(file.path(plotPATH, "AU_NZ_govtRev.png"))
grid.arrange(gp_GovtRev, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

gp_Lineaudnzd <- ggplot(data = meltx(audnzdx),
                 aes(x = date, y = value)) +
            geom_line(size = 0.95, color = 'black') +
            labs(y = NULL, x = NULL) +
            labs(title = "AUDNZD Exchange rate") +
            coord_trans(y = 'log')
png(file.path(plotPATH, "AUDNZDfx.png"))
grid.arrange(gp_Lineaudnzd, sub = textGrob('www.ricardianambivalance.com'))
dev.off()

gp_LineNZDtwi <- ggplot(data = meltx(nzdtwix),
                 aes(x = date, y = value)) +
            geom_line(size = 0.95, color = 'black') +
            labs(y = NULL, x = NULL) +
            labs(title = "NZD Trade Weighted Exchange rate") +
            coord_trans(y = 'log')
png(file.path(plotPATH, "nzdTWIfx.png"))
grid.arrange(gp_LineNZDtwi, sub = textGrob('www.ricardianambivalance.com'))
dev.off()


# }}}
