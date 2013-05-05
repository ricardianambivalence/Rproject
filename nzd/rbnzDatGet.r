# get the RBNZ prices file
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(TTR)
require(gridExtra)
require(timsac)

## get from web or saved xls?
getWeb <- FALSE

## PATH stuff
projectPATH <- "~/R/AUD/alm"
plotPATH <- file.path(projectPATH, "Rpics")


## download from web and format or get from store?
if (getWeb)
{
    RBNZ_A3 <- "http://www.rbnz.govt.nz/statistics/econind/a3/ha3.xls"
    RBNZ_A3_xls <- read.xls(RBNZ_A3, sheet = 'HISTORY_A3', skip=5, header=FALSE, as.is=TRUE)
# fix dates
    RBNZ_A3_xls[,1] <- as.Date(paste0(substr(absT1[,1], 5, 9), "-", substr(absT1[,1], 1, 3), "-01"), format = "%Y-%b-%d")
# fix names
    names(RBNZ_A3_xls) <- c('date', 'ftM', 'ftF', 'ft', 'ptM', 'ptF', 'pt', 'nM', 'nF', 'n',
                             'unFtM', 'unFtF', 'unFT', 'unPtM', 'unPtF', 'unPt', 'unM', 'unF', 'un',
                             'lfM', 'lfF', 'lf', 'urFtM', 'urFtF', 'urFt', 'urM', 'urF', 'ur',
                             'prM', 'prF', 'pr', 'nPopM', 'nPopF', 'nPop', 'unPopM', 'unPopF', 'unPop'
                             )
    rm(absT1xls)
    save(absT1, file = "~/data/aud/alm/almT1.RData")

    absT2 <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202002.xls&6202.0&Time%20Series%20Spreadsheet&7942DC1D64ADDF2DCA257B4900128C21&0&Mar%202013&11.04.2013&Latest"
    absT2xls <- read.xls(absT2, sheet = 'Data1')
    absT2 <- absT2xls[-c(1:10),]
    absT2[,1] <- as.Date(paste0(substr(absT2[,1], 5, 9), "-", substr(absT2[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT2) <- c('date', 'ftM', 'ftF', 'ft', 'ptM', 'ptF', 'pt', 'nM', 'nF', 'n',
                             'unFtM', 'unFtF', 'unFT', 'unPtM', 'unPtF', 'unPt', 'unM', 'unF', 'un',
                             'lfM', 'lfF', 'lf', 'urFtM', 'urFtF', 'urFt', 'urM', 'urF', 'ur',
                             'prM', 'prF', 'pr', 'nPopM', 'nPopF', 'nPop', 'unPopM', 'unPopF', 'unPop'
                             )
    absT2[,-1] <- sapply(absT2[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT2xls)
    save(absT2, file = "~/data/aud/alm/almT2.RData")

    absT3 <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202003.xls&6202.0&Time%20Series%20Spreadsheet&AEA0E7444BF8B1CFCA257B4900128CFA&0&Mar%202013&11.04.2013&Latest"
    absT3xls <- read.xls(absT3, sheet = 'Data1')
    absT3 <- absT3xls[-c(1:10),]
    absT3[,1] <- as.Date(paste0(substr(absT3[,1], 5, 9), "-", substr(absT3[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT3) <- c('date', 'ftM', 'ftF', 'ft', 'ptM', 'ptF', 'pt', 'nM', 'nF', 'n',
                             'unFtM', 'unFtF', 'unFT', 'unPtM', 'unPtF', 'unPt', 'unM', 'unF', 'un',
                             'lfM', 'lfF', 'lf', 'nonM', 'nonF', 'non', 'popM', 'popF', 'pop',
                             'urFtM', 'urFtF', 'urFt', 'urM', 'urF', 'ur', 'prM', 'prF', 'pr',
                             'nPopM', 'nPopF', 'nPop', 'unPopM', 'unPopF', 'unPop'
                             )
    absT3[,-1] <- sapply(absT3[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT3xls)
    save(absT3, file = "~/data/aud/alm/almT3.RData")

    absT19 <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202019.xls&6202.0&Time%20Series%20Spreadsheet&A72F1768257CEE10CA257B490012A4D8&0&Mar%202013&11.04.2013&Latest"
    absT19xls <- read.xls(absT19, sheet = 'Data1')
    absT19 <- absT19xls[-c(1:10),]
    absT19[,1] <- as.Date(paste0(substr(absT19[,1], 5, 9), "-", substr(absT19[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT19) <- c('date', 'hrsFtM_t', 'hrsFtF_t', 'hrsFt_t', 'hrsPtM_t', 'hrsPtF_t',
                       'hrsPt_t', 'hrsM_t', 'hrsF_t', 'hrs_t',
                       'hrsFtM_sa', 'hrsFtF_sa', 'hrsFt_sa', 'hrsPtM_sa', 'hrsPtF_sa', 'hrsPt_sa',
                       'hrsM_sa', 'hrsF_sa', 'hrs_sa')
    absT19[,-1] <- sapply(absT19[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT19xls)
    save(absT19, file = "~/data/aud/alm/almT19.RData")

    gflowsX <- "~/data/Mar13/hazard rates.xls"
    gflowsXls <- read.xls(gflowsX, sheet = 'toR', as.is=TRUE)
    gflowsXx <- xts(gflowsXls[, -1], order.by = as.Date(gflowsXls[, 1]))
    rm(gflowsX, gflowsXls)
    save(gflowsXx, file = "~/data/aud/alm/almGFlows.RData")
} else {
    load("~/data/aud/alm/almT1.RData")
    load("~/data/aud/alm/almT2.RData")
    load("~/data/aud/alm/almT3.RData")
    load("~/data/aud/alm/almT19.RData")
    load("~/data/aud/alm/almGFlows.RData")
}
