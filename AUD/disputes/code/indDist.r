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
projPATH <- file.path("~/R/aud/disputes")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}

getABS <- FALSE

if(getABS) {
    indDstH <- "http://abs.gov.au/AUSSTATS/ABS@Archive.nsf/log?openagent&6321055001table1.xls&6321.0.55.001&Time%20Series%20Spreadsheet&6C52FD6B1F5664DDCA257B810013D028&0&Mar%202013&06.06.2013&Latest"
    indDist <- readABS(indDstH)
    names(indDist) <- c(
                        'dispCmncd', 'dispOcrrd', 'dispCmncd_n', 'dispOcrrd_n',
                        'dispOcrrd_days', 'dispCmncd_12m', 'dispOcrrd_12m',
                        'dispCmncd_n_12m', 'dispOcrrd_n_12m', 'wdaysLost_12m'
                        )

    almT2h <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202001.xls&6202.0&Time%20Series%20Spreadsheet&4D9B3C38E962657FCA257B6500148B16&0&Apr%202013&09.05.2013&Latest"
    almT2 <- readABS(almT2h)
    names(almT2) <- c('ftM', 'ftF', 'ft', 'ptM', 'ptF', 'pt', 'nM', 'nF', 'n',
                      'unFtM', 'unFtF', 'unFT', 'unPtM', 'unPtF', 'unPt', 'unM', 'unF', 'un',
                      'lfM', 'lfF', 'lf', 'urFtM', 'urFtF', 'urFt', 'urM', 'urF', 'ur',
                      'prM', 'prF', 'pr', 'nPopM', 'nPopF', 'nPop', 'unPopM', 'unPopF', 'unPop'
                      )

    save(indDist, almT2, file = file.path(dataPATH, "indDist.rdata"))
} else {
    load(file = file.path(dataPATH, "indDist.rdata"))
}

almT2_q <- apply.quarterly(almT2, colMeans)

indDist$ur <- almT2_q$ur
indDist$nPop <- almT2_q$nPop
indDist$n <- almT2_q$n
indDist$lost2n <- indDist$dispCmncd_n / indDist$n * 100

indDist_y <- apply.yearly(indDist, colMeans)
indDist_y$year = as.POSIXlt(index(indDist_y))$year + 1900
indDist_y_df <- as.data.frame(indDist_y)
indDist_y_df$split <- '84-93'
indDist_y_df[indDist_y_df$year %in% 1994:2003, 'split'] <- '94-03'
indDist_y_df[indDist_y_df$year %in% 2004:2013, 'split'] <- '04-13'
indDist_y_df$split <- factor(indDist_y_df$split, levels = c('84-93', '94-03', '04-13'),
                             ordered = TRUE)

# try and quadratic fit -- see if you get a u-shape for the 85-94 part

plot(indDist_y$lost2n)

gp_unionUR <- ggplot(indDist_y_df[, c('ur', 'lost2n', 'split')],
                     aes(x = lost2n, y = ur, color = split)) +
                labs(y = 'yr ave unemployment',
                     x = '% employess involved in disputes') +
                labs(title = "% of employees in disputes --> unemployment") +
                scale_color_brewer(palette = 'Set1') +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_point() +
                geom_smooth(method = 'glm')
pngMk("disputes2UR.png")
grid.arrange(gp_unionUR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
