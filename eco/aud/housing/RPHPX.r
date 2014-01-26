# get the xls file and make some housing market charts
rm(list=ls()); gc()
Sys.setenv(TZ = 'UTC')

# {{{ packages and functions
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(TTR)
require(gridExtra)
require(timsac)
source("/Users/mcooganj/R/Rhelpers/helperFuncts.r")
# }}}

## {{{ PATH stuff
projectPATH <- "~/R/AUD/housing"
plotPATH <- file.path(projectPATH, "pics")
codePATH <- file.path(projectPATH, "code")
dataPATH <- '~/data/aud/housing'
# }}}

# {{{ get data and SA
RPxls <- file.path(dataPATH, "RPdata_hpx.xls")
RPdata <- read.xls(RPxls, sheet = 'Capital City Hedonic Actual', as.is=TRUE,
                   header=TRUE, skip=1)
names(RPdata) <- c('date', 'SydDwl', 'SydHouse', 'SydUnit', 'MelbDwl', 'MelbHouse', 'MelbUnit',
                      'BrisGcDwl', 'BrisGcHouse', 'BrisGcUnit', 'BrisDwl' ,'BrisHouse', 'BrisUnit',
                      'AdelDwl', 'AdelHouse', 'AdelUnit', 'PerthDwl', 'PerthHouse', 'PerthUnit',
                      'HobartDwl', 'HobartHouse', 'HobartUnit', 'DarwinDwl', 'DarwinHouse', 'DarwinUnit',
                      'CanbDwl', 'CanbHouse', 'CanbUnit', 'Cap5Dwl', 'Cap5House', 'Cap5Unit',
                      'Cap8Dwl', 'Cap8House', 'Cap8Unit')
RPdata <- RPdata[, 1:34]

RPdata$date <- as.Date(RPdata$date, format = "%d/%m/%Y")
RPnum <- sapply(RPdata[, -1], FUN = function(X) as.numeric(X))
RPdata_x <- xts(RPnum, order.by = RPdata$date)
RPdata_SA <- exp(mj_SAmat_m(log(RPdata_x)))
RPMoM <- diff(RPdata_SA, log=TRUE) * 100
RP3mma <- rollapplyr(RPMoM, 3, colMeans)
# }}}

# {{{ plots

# the aim is to package the line and bar plots -- 2/3 to line, and 1/3 to bar at bottom

makeTwins <- function(XL, XD) {
    options(warn = -1)
    pdf(file = file.path(plotPATH, paste0("RP_", names(XD), ".pdf")))
    #
    lineTitle <- paste0(names(XL)[1], ": Price (log 'k) & 3m Ave % change")
    gp_RP_PXline <- ggplot(subset(meltx(XL), variable == names(XL)),
                                 aes( x = date, y = value)) +
                            theme_grey() +
                            coord_trans(y = "log") +
                            labs(y = NULL, x = NULL) +
                            labs(title = lineTitle) +
                            theme(legend.position = 'none') +
                            theme(legend.title = element_blank()) +
                            geom_line(color = 'blue')
    #
    gp_RP_diffbar <- ggplot(subset(meltx(XD), variable == names(XD)),
                                 aes( x = date, y = value)) +
                            theme_grey() +
                            labs(y = NULL, x = "") +
                            theme(legend.position = 'none') +
                            theme(legend.title = element_blank()) +
                            geom_bar(stat = 'identity', color = 'red', fill = 'red')
    #
    pushViewport(viewport(layout = grid.layout(3,1)))
    print(gp_RP_PXline, vp = vplayout(1:2, 1))
    print(gp_RP_diffbar, vp = vplayout(3, 1))
    upViewport() # brings you to the top viewport for the whole plotting area
    grid.text("ricardianambivalence.com", x = unit(0.8, "npc"), y = unit(0.02, "npc"))
    dev.off()
}

for(i in 1:ncol(RPdata_SA)) {
    makeTwins(RPdata_SA[,i], RP3mma[,i])
}

gp_DwlpxBar <- ggplot(subset(meltx(last(RP3mma)),
                          variable %in% c('SydDwl','MelbDwl','BrisDwl', 'AdelDwl', 'PerthDwl',
                                          'HobartDwl', 'DarwinDwl', 'CanbDwl', 'Cap8Dwl')),
                    aes(x = variable, y = value, color = variable, fill = variable)) +
                    labs(y = NULL, X = '') +
                    theme(axis.title.x = element_blank()) +
                    theme(legend.position = 'none') +
                    labs(title = "3mma Capital Price change (%)") +
                    geom_bar(stat = 'identity') +
                    scale_fill_brewer(palette = 'Set1') +
                    scale_color_brewer(palette = 'Set1')
png(file.path(plotPATH, "gp_DwlpxBar.png"))
grid.arrange(gp_DwlpxBar, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_HousepxBar <- ggplot(subset(meltx(last(RP3mma)),
                          variable %in% c('SydHouse', 'MelbHouse','BrisHouse', 'AdelHouse', 'PerthHouse',
                                          'HobartHouse', 'DarwinHouse', 'CanbHouse', 'Cap8House')),
                    aes(x = variable, y = value, color = variable, fill = variable)) +
                    labs(y = NULL, X = '') +
                    theme(axis.title.x = element_blank()) +
                    theme(legend.position = 'none') +
                    labs(title = "3mma Capital Price change (%)") +
                    geom_bar(stat = 'identity') +
                    scale_fill_brewer(palette = 'Set1') +
                    scale_color_brewer(palette = 'Set1')
png(file.path(plotPATH, "gp_HousepxBar.png"))
grid.arrange(gp_HousepxBar, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_UnitpxBar <- ggplot(subset(meltx(last(RP3mma)),
                          variable %in% c('SydUnit', 'MelbUnit','BrisUnit', 'AdelUnit', 'PerthUnit',
                                          'HobartUnit', 'DarwinUnit', 'CanbUnit', 'Cap8Unit')),
                    aes(x = variable, y = value, color = variable, fill = variable)) +
                    labs(y = NULL, X = '') +
                    theme(axis.title.x = element_blank()) +
                    theme(legend.position = 'none') +
                    labs(title = "3mma Capital Price change (%)") +
                    geom_bar(stat = 'identity') +
                    scale_fill_brewer(palette = 'Set1') +
                    scale_color_brewer(palette = 'Set1')
png(file.path(plotPATH, "gp_UnitpxBar.png"))
grid.arrange(gp_UnitpxBar, sub = textGrob('www.ricardianambivalence.com'))
dev.off()


