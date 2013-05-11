# get the weekly MOF data and make some charts

# {{{ clean up
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
# }}}

## {{{ packages and functions
require(gdata)
require(data.table)
require(xts)
require(ggplot2)
require(reshape2)
require(TTR)
require(gridExtra)
require(timsac)
source("~/r/Rhelpers/helperFuncts.r")
source("~/r/Rhelpers/RAcolorpal.r")
# }}}

## {{{ PATH stuff
projectPATH <- "~/r/jpy/flows"
plotPATH <- file.path(projectPATH, "pics")
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")
# }}}

# get from web or saved xls?
getWeb <- FALSE

## {{{ get the dataza
if (getWeb)
{
    MOF_weeklycsv <- "http://www.mof.go.jp/international_policy/reference/itn_transactions_in_securities/week.csv"
#
    MOF_data <- read.csv(MOF_weeklycsv,
                         header = FALSE,
                         stringsAsFactors = FALSE,
                         fileEncoding = 'latin1',
                         skip = 16
                         )
#
    MOF_data <- MOF_data[, 1:23]
    MOF_data[, 1] <- seq(as.Date("2005-01-08"), by = 'week', length.out = nrow(MOF_data))
    MOF_data[, -1] <- as.data.frame(lapply(MOF_data[,-1],  # convert to numbers
        function(d) type.convert(gsub(d, pattern=",", replace=""))))
#
    names(MOF_data) <- c('date',
                         'JIA_eq.sales', 'JIA_eq.purch', 'JIA_eq.net',
                         'JIA_bond.sales', 'JIA_bond.purch', 'JIA_bond.net',
                         'JIA_eqBond.subTtl',
                         'JIA_mmkt.sales', 'JIA_mmkt.purch', 'JIA_mmkt.net',
                         'JIA_total.net',
                         'FIJ_eq.purch', 'FIJ_eq.sales', 'FIJ_eq.net',
                         'FIJ_bond.purch', 'FIJ_bond.sales', 'FIJ_bond.net',
                         'FIJ_eqBond.subTtl',
                         'FIJ_mmkt.purch', 'FIJ_mmkt.sales', 'FIJ_mmkt.net',
                         'FIJ_total.net'
                         )
    MOF_data[, 2:23] <- MOF_data[, 2:23] / 10 # native unit is Y100m => converted to Ybn
    save(MOF_data, file = file.path(dataPATH, "weeklyMOF.rdata"))

# +ve net = Japan money inflows to JPY => +ve JIA net is sales > purch: +FIJ is purch > sales
} else {
    load(file.path(dataPATH, "weeklyMOF.rdata"))
}

MOF_datax <- xtsF(MOF_data)
MOF_data_m <- apply.monthly(MOF_datax, colSums)
MOFm_melt <- meltx(MOF_data_m)
MOF_melt <- melt(tail(MOF_data, 16), id.vars = 1)

# }}}

## {{{ PLOTS

# {{{ montly data
# Japan trading foreign equities
gp_JIA.eq <- ggplot(data = subset(MOFm_melt, variable %in% c('JIA_eq.sales',
                                                            'JIA_eq.purch',
                                                            'JIA_eq.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Japanese foreign equity flows: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "jia_eq.png"))
grid.arrange(gp_JIA.eq, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading foreign bonds
gp_JIA.bond <- ggplot(data = subset(MOFm_melt, variable %in% c('JIA_bond.sales',
                                                              'JIA_bond.purch',
                                                              'JIA_bond.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Japanese foreign bond flows: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "jia_bond.png"))
grid.arrange(gp_JIA.bond, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading foreign mmkt
gp_JIA.mmkt <- ggplot(data = subset(MOFm_melt, variable %in% c('JIA_mmkt.sales',
                                                              'JIA_mmkt.purch',
                                                              'JIA_mmkt.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Japanese foreign money market flows: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "jia_mmkt.png"))
grid.arrange(gp_JIA.mmkt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading net foreign
gp_JIA.nets <- ggplot(data = subset(MOFm_melt, variable %in% c('JIA_eq.net',
                                                              'JIA_bond.net',
                                                              'JIA_mmkt.net',
                                                              'JIA_total.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Net Japanese foreign investment flows: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_4, guide = 'none') +
                    scale_fill_manual(values = RAPal_4, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "jia_nets.png"))
grid.arrange(gp_JIA.nets, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# foreign trading japanese equities
gp_FIJ.eq <- ggplot(data = subset(MOFm_melt, variable %in% c('FIJ_eq.sales',
                                                            'FIJ_eq.purch',
                                                            'FIJ_eq.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Foreign flows in Japanese equities: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "fij_eq.png"))
grid.arrange(gp_FIJ.eq, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# foreign trading JGBs
gp_FIJ.bond <- ggplot(data = subset(MOFm_melt, variable %in% c('FIJ_bond.sales',
                                                              'FIJ_bond.purch',
                                                              'FIJ_bond.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Foreign flows in JGBs: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "fij_bond.png"))
grid.arrange(gp_FIJ.bond, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# foreign trading JPY mmkt
gp_FIJ.mmkt <- ggplot(data = subset(MOFm_melt, variable %in% c('FIJ_mmkt.sales',
                                                              'FIJ_mmkt.purch',
                                                              'FIJ_mmkt.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Foreign flows in JPY mmkt: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "fij_mmkt.png"))
grid.arrange(gp_FIJ.mmkt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading net foreign
gp_FIJ.nets <- ggplot(data = subset(MOFm_melt, variable %in% c('FIJ_eq.net',
                                                              'FIJ_bond.net',
                                                              'FIJ_mmkt.net',
                                                              'FIJ_total.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Net foreign flows in Japanese securities: JPYbn/mth") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_4, guide = 'none') +
                    scale_fill_manual(values = RAPal_4, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week", "wk2mon",  "fij_nets.png"))
grid.arrange(gp_FIJ.nets, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
# }}}

# {{{ weekly data
# Japan trading foreign equities
gp_JIAwk.eq <- ggplot(data = subset(MOF_melt, variable %in% c('JIA_eq.sales',
                                                            'JIA_eq.purch',
                                                            'JIA_eq.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Japanese foreign equity flows: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "jia_eqWk.png"))
grid.arrange(gp_JIAwk.eq, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading foreign bonds
gp_JIAwk.bond <- ggplot(data = subset(MOF_melt, variable %in% c('JIA_bond.sales',
                                                              'JIA_bond.purch',
                                                              'JIA_bond.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Japanese foreign bond flows: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "jia_bondWk.png"))
grid.arrange(gp_JIAwk.bond, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading foreign mmkt
gp_JIAwk.mmkt <- ggplot(data = subset(MOF_melt, variable %in% c('JIA_mmkt.sales',
                                                              'JIA_mmkt.purch',
                                                              'JIA_mmkt.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Japanese foreign money market flows: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "jia_mmktWk.png"))
grid.arrange(gp_JIAwk.mmkt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading net foreign
gp_JIAwk.nets <- ggplot(data = subset(MOF_melt, variable %in% c('JIA_eq.net',
                                                              'JIA_bond.net',
                                                              'JIA_mmkt.net',
                                                              'JIA_total.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Net Japanese foreign investment flows: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_4, guide = 'none') +
                    scale_fill_manual(values = RAPal_4, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "jia_netsWk.png"))
grid.arrange(gp_JIAwk.nets, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# foreign trading japanese equities
gp_FIJwk.eq <- ggplot(data = subset(MOF_melt, variable %in% c('FIJ_eqwk.sales',
                                                            'FIJ_eq.purch',
                                                            'FIJ_eq.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Foreign flows in Japanese equities: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "fij_eqWk.png"))
grid.arrange(gp_FIJwk.eq, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# foreign trading JGBs
gp_FIJwk.bond <- ggplot(data = subset(MOF_melt, variable %in% c('FIJ_bond.sales',
                                                              'FIJ_bond.purch',
                                                              'FIJ_bond.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Foreign flows in JGBs: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "fij_bondWk.png"))
grid.arrange(gp_FIJwk.bond, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# foreign trading JPY mmkt
gp_FIJwk.mmkt <- ggplot(data = subset(MOF_melt, variable %in% c('FIJ_mmkt.sales',
                                                              'FIJ_mmkt.purch',
                                                              'FIJ_mmkt.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Foreign flows in JPY mmkt: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_3, guide = 'none') +
                    scale_fill_manual(values = RAPal_3, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "fij_mmktWk.png"))
grid.arrange(gp_FIJwk.mmkt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# japan trading net foreign
gp_FIJwk.nets <- ggplot(data = subset(MOF_melt, variable %in% c('FIJ_eq.net',
                                                              'FIJ_bond.net',
                                                              'FIJ_mmkt.net',
                                                              'FIJ_total.net')),
                    aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    labs(y = NULL, x = NULL) +
                    labs(title = "Net foreign flows in Japanese securities: JPYbn/wk") +
                    theme(legend.position = 'none') +
                    scale_color_manual(values = RAPal_4, guide = 'none') +
                    scale_fill_manual(values = RAPal_4, guide = 'none') +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "week",  "fij_netsWk.png"))
grid.arrange(gp_FIJwk.nets, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
# }}}

