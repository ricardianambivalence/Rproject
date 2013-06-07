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

getABS <- TRUE

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

plot(indDist_y$lost2n)

igp_unionUR <- ggplot() +
                geom_point() +
                geom_smooth()
