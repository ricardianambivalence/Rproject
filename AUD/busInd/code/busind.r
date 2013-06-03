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
projPATH <- file.path("~/R/aud/busInd")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}

getABS <- TRUE

if(getABS) {
    bIndT11 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56760011.xls&5676.0&Time%20Series%20Spreadsheet&176FE7FE2E6E2E68CA257B7C0013187E&0&Mar%202013&03.06.2013&Latest"
    corpGOP<- readABS(bIndT11)
    names(corpGOP) <- c(
                       'mining_nsa', 'manu_nsa', 'utils_nsa', 'const_nsa', 'whole_nsa',
                       'retail_nsa', 'accomFood_nsa', 'transport_nsa', 'ict_nsa', 'fins_nsa',
                       'rentRE_nsa', 'profScience_nsa', 'admin_nsa', 'artsRec_nsa',
                       'othServ_nsa', 'total_nsa',
                       'mining_sa', 'manu_sa', 'utils_sa', 'const_sa', 'whole_sa',
                       'retail_sa', 'accomFood_sa', 'transport_sa', 'ict_sa', 'fins_sa',
                       'rentRE_sa', 'profScience_sa', 'admin_sa', 'artsRec_sa',
                       'othServ_sa', 'total_sa',
                       'mining_t', 'manu_t', 'utils_t', 'const_t', 'whole_t',
                       'retail_t', 'accomFood_t', 'transport_t', 'ict_t', 'fins_t',
                       'rentRE_t', 'profScience_t', 'admin_t', 'artsRec_t',
                       'othServ_t', 'total_t'
                      )

    bIndT15 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56760015.xls&5676.0&Time%20Series%20Spreadsheet&79FD3B7461475845CA257B7C00131AE3&0&Mar%202013&03.06.2013&Latest"
    bizGOP<- readABS(bIndT15)
    names(bizGOP) <- c(
                       'mining_nsa', 'manu_nsa', 'utils_nsa', 'const_nsa', 'whole_nsa',
                       'retail_nsa', 'accomFood_nsa', 'transport_nsa', 'ict_nsa', 'fins_nsa',
                       'rentRE_nsa', 'profScience_nsa', 'admin_nsa', 'artsRec_nsa',
                       'othServ_nsa', 'total_nsa',
                       'mining_sa', 'manu_sa', 'utils_sa', 'const_sa', 'whole_sa',
                       'retail_sa', 'accomFood_sa', 'transport_sa', 'ict_sa', 'fins_sa',
                       'rentRE_sa', 'profScience_sa', 'admin_sa', 'artsRec_sa',
                       'othServ_sa', 'total_sa',
                       'mining_t', 'manu_t', 'utils_t', 'const_t', 'whole_t',
                       'retail_t', 'accomFood_t', 'transport_t', 'ict_t', 'fins_t',
                       'rentRE_t', 'profScience_t', 'admin_t', 'artsRec_t',
                       'othServ_t', 'total_t'
                      )

    save(corpGOP, bizGOP, file = file.path(dataPATH, "profits.rdata"))
} else {
    load(file.path(dataPATH, "profits.rdata"))
}

corpGOP_3split <- corpGOP[, c('mining_sa', 'total_sa')]
corpGOP_3split$tradable_notMining <- rowSums(corpGOP[,
                                            c('manu_sa', 'whole_sa', 'retail_sa',
                                              'accomFood_sa', 'transport_sa')],
                                            na.rm=TRUE)
corpGOP_3split$nonTradable <- corpGOP$total_sa - corpGOP$mining_sa - corpGOP_3split$tradable_notMining

bizGOP_3split <- bizGOP[, c('mining_sa', 'total_sa')]
bizGOP_3split$tradable_notMining <- rowSums(bizGOP[,
                                            c('manu_sa', 'whole_sa', 'retail_sa',
                                              'accomFood_sa', 'transport_sa')],
                                            na.rm=TRUE)
bizGOP_3split$nonTradable <- bizGOP$total_sa - bizGOP$mining_sa - bizGOP_3split$tradable_notMining
