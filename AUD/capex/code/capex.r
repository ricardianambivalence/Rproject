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
projPATH <- file.path("~/R/aud/capex")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}

# {{{ color palette
RApal <- brewer.pal(10, "RdYlBu")
RApal_front5 <- brewer.pal(5, "RdYlBu")[1:5]
RApal_back5 <- brewer.pal(10, "RdYlBu")[6:10]
RAPal_5 <- brewer.pal(5, 'RdYlBu')
# }}}

## get from web or saved xls?
getWeb <- TRUE

## download from web and format or get from store?
if (getWeb)
{
    capexT1a <- file.path(dataPATH, "5625001a.xls")
    capexT1b <- file.path(dataPATH, "5625001b.xls")
    capexT1c <- file.path(dataPATH, "5625001c.xls")
    capexT1e <- file.path(dataPATH, "5625001e.xls")
    capexT1f <- file.path(dataPATH, "5625001f.xls")
    capexT2a <- file.path(dataPATH, "5625002a.xls")
    capexT2b <- file.path(dataPATH, "5625002b.xls")
    capexT2c <- file.path(dataPATH, "5625002c.xls")
    capexT2e <- file.path(dataPATH, "5625002e.xls")
    capexT2f <- file.path(dataPATH, "5625002f.xls")
    capexT3a <- file.path(dataPATH, "5625003a.xls")
    capexT3b <- file.path(dataPATH, "5625003b.xls")
    capexT4a <- file.path(dataPATH, "5625004a.xls")
    capexT4b <- file.path(dataPATH, "5625004b.xls")
    capexT4c <- file.path(dataPATH, "5625004c.xls")
    capexT5a <- file.path(dataPATH, "5625005a.xls")
    capexT5b <- file.path(dataPATH, "5625005b.xls")
    capexT5c <- file.path(dataPATH, "5625005c.xls")
    capexT6a <- file.path(dataPATH, "5625006a.xls")
    capexT6b <- file.path(dataPATH, "5625006b.xls")
    capexT7a <- file.path(dataPATH, "5625007a.xls")
    capexT7b <- file.path(dataPATH, "5625007b.xls")
    capexT8a <- file.path(dataPATH, "5625008a.xls")
    capexT8b <- file.path(dataPATH, "5625008b.xls")
    capexT9a <- file.path(dataPATH, "5625009a.xls")
    capexT9b <- file.path(dataPATH, "5625009b.xls")

    cxT1a <- readABS(capexT1a)
    names(cxT1a) <- c(
                      'manu_BnS_nom_nsa', 'manu_EPM_nom_nsa', 'manu_Ttl_nom_nsa',
                      'min_BnS_nom_nsa', 'min_EPM_nom_nsa', 'min_Ttl_nom_nsa',
                      'othr_BnS_nom_nsa', 'othr_EPM_nom_nsa', 'othr_Ttl_nom_nsa',
                      'all_BnS_nom_nsa', 'all_EPM_nom_nsa', 'all_Ttl_nom_nsa'
                      )

    cxT1b <- readABS(capexT1b)
    names(cxT1b) <- c(
                      'manu_BnS_stX', 'manu_EPM_stX', 'manu_Ttl_stX',
                      'min_BnS_stX', 'min_EPM_stX', 'min_Ttl_stX',
                      'othr_BnS_stX', 'othr_EPM_stX', 'othr_Ttl_stX',
                      'all_BnS_stX', 'all_EPM_stX', 'all_Ttl_stX'
                      )




} else {
    load("~/data/aud/trade/tradeXByDest.RData")
}


