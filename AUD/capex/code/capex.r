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

# {{{ get data
getWeb <- TRUE # new data, or from the store?

## download from web and format or get from store?
if (getWeb)
{
# file paths -- could be html addy
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
    capexT12a <- file.path(dataPATH, "56250012a.xls")
    capexT12b <- file.path(dataPATH, "56250012b.xls")

    # {{{ get data
    cxT1a <- readABS(capexT1a)
    names(cxT1a) <- c(
                      'min_BnS_nom_nsa','manu_BnS_nom_nsa', 'othr_BnS_nom_nsa', 'all_BnS_nom_nsa',
                      'min_EPM_nom_nsa','manu_EPM_nom_nsa', 'othr_EPM_nom_nsa', 'all_EPM_nom_nsa',
                      'min_Ttl_nom_nsa','manu_Ttl_nom_nsa', 'othr_Ttl_nom_nsa', 'all_Ttl_nom_nsa'
                      )

    cxT1b <- readABS(capexT1b)
    names(cxT1b) <- c(
                      'min_BnS_stX','manu_BnS_stX', 'othr_BnS_stX', 'all_BnS_stX',
                      'min_EPM_stX','manu_EPM_stX', 'othr_EPM_stX', 'all_EPM_stX',
                      'min_Ttl_stX','manu_Ttl_stX', 'othr_Ttl_stX', 'all_Ttl_stX'
                      )

    cxT1c <- readABS(capexT1c)
    names(cxT1c) <- c(
                      'min_BnS_ltX','manu_BnS_ltX', 'othr_BnS_ltX', 'all_BnS_ltX',
                      'min_EPM_ltX','manu_EPM_ltX', 'othr_EPM_ltX', 'all_EPM_ltX',
                      'min_Ttl_ltX','manu_Ttl_ltX', 'othr_Ttl_ltX', 'all_Ttl_ltX'
                      )

    cxT1e <- readABS(capexT1e)
    names(cxT1e) <- c(
                      'min_BnS_nom_sa','manu_BnS_nom_sa', 'othr_BnS_nom_sa', 'all_BnS_nom_sa',
                      'min_EPM_nom_sa','manu_EPM_nom_sa', 'othr_EPM_nom_sa', 'all_EPM_nom_sa',
                      'min_Ttl_nom_sa','manu_Ttl_nom_sa', 'othr_Ttl_nom_sa', 'all_Ttl_nom_sa'
                      )

    cxT1f <- readABS(capexT1f)
    names(cxT1f) <- c(
                      'min_BnS_nom_t','manu_BnS_nom_t', 'othr_BnS_nom_t', 'all_BnS_nom_t',
                      'min_EPM_nom_t','manu_EPM_nom_t', 'othr_EPM_nom_t', 'all_EPM_nom_t',
                      'min_Ttl_nom_t','manu_Ttl_nom_t', 'othr_Ttl_nom_t', 'all_Ttl_nom_t'
                      )

    cxT2a <- readABS(capexT2a)
    names(cxT2a) <- c(
                      'min_nom_nsa', 'manuFood_nom_nsa', 'manuBevTob_nom_nsa', 'manuTCF_nom_nsa',
                      'manuWood_nom_nsa', 'manuPulp_nom_nsa', 'manuPrinting_nom_nsa',
                      'manuFFuels_nom_nsa', 'manuChem_nom_nsa', 'manuPolymer_nom_nsa',
                      'manuNonMetalMin_nom_nsa', 'manuMetal_nom_nsa',
                      'manuFabMetal_nom_nsa', 'manuTranspt_nom_nsa', 'manuMachEqp_nom_nsa',
                      'manuFurn_nom_nsa', 'manuTtl_nom_nsa', 'utils_nom_nsa',
                      'constn_nom_nsa', 'wholsl_nom_nsa', 'retail_nom_nsa',
                      'transpt_nom_nsa', 'ict_nom_nsa', 'fins_nom_nsa', 'rentRE_nom_nsa',
                      'profScient_nom_nsa', 'othrServ_nom_nsa', 'otherAll_nom_nsa',
                      'all_nom_nsa', 'minCoal_nom_nsa', 'minOilGas_nom_nsa',
                      'minOre_nom_nsa', 'minNonMetal_nom_nsa', 'minExplr_nom_nsa'
                      )

    cxT2b <- readABS(capexT2b)
    names(cxT2b) <- c(
                      'min_stX', 'manuFood_stX', 'manuBevTob_stX', 'manuTCF_stX',
                      'manuWood_stX', 'manuPulp_stX', 'manuPrinting_stX',
                      'manuFFuels_stX', 'manuChem_stX', 'manuPolymer_stX',
                      'manuNonMetalMin_stX', 'manuMetal_stX',
                      'manuFabMetal_stX', 'manuTranspt_stX', 'manuMachEqp_stX',
                      'manuFurn_stX', 'manuTtl_stX', 'utils_stX',
                      'constn_stX', 'wholsl_stX', 'retail_stX',
                      'transpt_stX', 'ict_stX', 'fins_stX', 'rentRE_stX',
                      'profScient_stX', 'othrServ_stX', 'otherAll_stX', 'all_stX'
                      )

    cxT2c <- readABS(capexT2c)
    names(cxT2c) <- c(
                      'min_ltX', 'manuFood_ltX', 'manuBevTob_ltX', 'manuTCF_ltX',
                      'manuWood_ltX', 'manuPulp_ltX', 'manuPrinting_ltX',
                      'manuFFuels_ltX', 'manuChem_ltX', 'manuPolymer_ltX',
                      'manuNonMetalMin_ltX', 'manuMetal_ltX',
                      'manuFabMetal_ltX', 'manuTranspt_ltX', 'manuMachEqp_ltX',
                      'manuFurn_ltX', 'manuTtl_ltX', 'utils_ltX',
                      'constn_ltX', 'wholsl_ltX', 'retail_ltX',
                      'transpt_ltX', 'ict_ltX', 'fins_ltX', 'rentRE_ltX',
                      'profScient_ltX', 'othrServ_ltX', 'otherAll_ltX', 'all_ltX'
                      )

    cxT2e <- readABS(capexT2e)
    names(cxT2e) <- c(
                      'min_nom_sa', 'manuFood_nom_sa', 'manuBevTob_nom_sa', 'manuTCF_nom_sa',
                      'manuWood_nom_sa', 'manuPulp_nom_sa', 'manuPrinting_nom_sa',
                      'manuFFuels_nom_sa', 'manuChem_nom_sa', 'manuPolymer_nom_sa',
                      'manuNonMetalMin_nom_sa', 'manuMetal_nom_sa',
                      'manuFabMetal_nom_sa', 'manuTranspt_nom_sa', 'manuMachEqp_nom_sa',
                      'manuFurn_nom_sa', 'manuTtl_nom_sa', 'utils_nom_sa',
                      'constn_nom_sa', 'wholsl_nom_sa', 'retail_nom_sa',
                      'transpt_nom_sa', 'ict_nom_sa', 'fins_nom_sa', 'rentRE_nom_sa',
                      'profScient_nom_sa', 'othrServ_nom_sa', 'otherAll_nom_sa', 'all_nom_sa'
                      )

    cxT2f <- readABS(capexT2f)
    names(cxT2f) <- c(
                      'min_nom_t', 'manuFood_nom_t', 'manuBevTob_nom_t', 'manuTCF_nom_t',
                      'manuWood_nom_t', 'manuPulp_nom_t', 'manuPrinting_nom_t',
                      'manuFFuels_nom_t', 'manuChem_nom_t', 'manuPolymer_nom_t',
                      'manuNonMetalMin_nom_t', 'manuMetal_nom_t',
                      'manuFabMetal_nom_t', 'manuTranspt_nom_t', 'manuMachEqp_nom_t',
                      'manuFurn_nom_t', 'manuTtl_nom_t', 'utils_nom_t',
                      'constn_nom_t', 'wholsl_nom_t', 'retail_nom_t',
                      'transpt_nom_t', 'ict_nom_t', 'fins_nom_t', 'rentRE_nom_t',
                      'profScient_nom_t', 'othrServ_nom_t', 'otherAll_nom_t', 'all_nom_t'
                      )

    cxT3a <- readABS(capexT3a)
    names(cxT3a) <- c(
                      'BnS_real_nsa', 'EPM_real_nsa', 'all_real_nsa',
                      'BnS_real_sa', 'EPM_real_sa', 'all_real_sa',
                      'BnS_real_t', 'EPM_real_t', 'all_real_t'
                      )

    cxT3b <- readABS(capexT3b)
    names(cxT3b) <- c(
                      'min_real_nsa', 'manu_real_nsa', 'othr_real_nsa', 'all_real_nsa',
                      'min_real_sa', 'manu_real_sa', 'othr_real_sa', 'all_real_sa',
                      'min_real_t', 'manu_real_t', 'othr_real_t', 'all_real_t'
                      )

    cxT12a <- readABS(capexT12a)
    names(cxT12a) <- c(
                       'min_BnS_e1', 'min_BnS_e2', 'min_BnS_e3', 'min_BnS_e4',
                       'min_BnS_e5', 'min_BnS_e6', 'min_BnS_e7',
                       'min_EPM_e1', 'min_EPM_e2', 'min_EPM_e3', 'min_EPM_e4',
                       'min_EPM_e5', 'min_EPM_e6', 'min_EPM_e7',
                       'min_Ttl_e1', 'min_Ttl_e2', 'min_Ttl_e3', 'min_Ttl_e4',
                       'min_Ttl_e5', 'min_Ttl_e6', 'min_Ttl_e7',
                       'manu_BnS_e1', 'manu_BnS_e2', 'manu_BnS_e3', 'manu_BnS_e4',
                       'manu_BnS_e5', 'manu_BnS_e6', 'manu_BnS_e7',
                       'manu_EPM_e1', 'manu_EPM_e2', 'manu_EPM_e3', 'manu_EPM_e4',
                       'manu_EPM_e5', 'manu_EPM_e6', 'manu_EPM_e7',
                       'manu_Ttl_e1', 'manu_Ttl_e2', 'manu_Ttl_e3', 'manu_Ttl_e4',
                       'manu_Ttl_e5', 'manu_Ttl_e6', 'manu_Ttl_e7',
                       'othr_BnS_e1', 'othr_BnS_e2', 'othr_BnS_e3', 'othr_BnS_e4',
                       'othr_BnS_e5', 'othr_BnS_e6', 'othr_BnS_e7',
                       'othr_EPM_e1', 'othr_EPM_e2', 'othr_EPM_e3', 'othr_EPM_e4',
                       'othr_EPM_e5', 'othr_EPM_e6', 'othr_EPM_e7',
                       'othr_Ttl_e1', 'othr_Ttl_e2', 'othr_Ttl_e3', 'othr_Ttl_e4',
                       'othr_Ttl_e5', 'othr_Ttl_e6', 'othr_Ttl_e7',
                       'all_BnS_e1', 'all_BnS_e2', 'all_BnS_e3', 'all_BnS_e4',
                       'all_BnS_e5', 'all_BnS_e6', 'all_BnS_e7',
                       'all_EPM_e1', 'all_EPM_e2', 'all_EPM_e3', 'all_EPM_e4',
                       'all_EPM_e5', 'all_EPM_e6', 'all_EPM_e7',
                       'all_Ttl_e1', 'all_Ttl_e2', 'all_Ttl_e3', 'all_Ttl_e4',
                       'all_Ttl_e5', 'all_Ttl_e6', 'all_Ttl_e7'
                      )

    cxT12b <- readABS(capexT12b)
    names(cxT12b) <- c(
                       'min_BnS_e1RR', 'min_BnS_e2RR', 'min_BnS_e3RR', 'min_BnS_e4RR',
                       'min_BnS_e5RR', 'min_BnS_e6RR', 'min_BnS_e7RR',
                       'min_EPM_e1RR', 'min_EPM_e2RR', 'min_EPM_e3RR', 'min_EPM_e4RR',
                       'min_EPM_e5RR', 'min_EPM_e6RR', 'min_EPM_e7RR',
                       'min_Ttl_e1RR', 'min_Ttl_e2RR', 'min_Ttl_e3RR', 'min_Ttl_e4RR',
                       'min_Ttl_e5RR', 'min_Ttl_e6RR', 'min_Ttl_e7RR',
                       'manu_BnS_e1RR', 'manu_BnS_e2RR', 'manu_BnS_e3RR', 'manu_BnS_e4RR',
                       'manu_BnS_e5RR', 'manu_BnS_e6RR', 'manu_BnS_e7RR',
                       'manu_EPM_e1RR', 'manu_EPM_e2RR', 'manu_EPM_e3RR', 'manu_EPM_e4RR',
                       'manu_EPM_e5RR', 'manu_EPM_e6RR', 'manu_EPM_e7RR',
                       'manu_Ttl_e1RR', 'manu_Ttl_e2RR', 'manu_Ttl_e3RR', 'manu_Ttl_e4RR',
                       'manu_Ttl_e5RR', 'manu_Ttl_e6RR', 'manu_Ttl_e7RR',
                       'othr_BnS_e1RR', 'othr_BnS_e2RR', 'othr_BnS_e3RR', 'othr_BnS_e4RR',
                       'othr_BnS_e5RR', 'othr_BnS_e6RR', 'othr_BnS_e7RR',
                       'othr_EPM_e1RR', 'othr_EPM_e2RR', 'othr_EPM_e3RR', 'othr_EPM_e4RR',
                       'othr_EPM_e5RR', 'othr_EPM_e6RR', 'othr_EPM_e7RR',
                       'othr_Ttl_e1RR', 'othr_Ttl_e2RR', 'othr_Ttl_e3RR', 'othr_Ttl_e4RR',
                       'othr_Ttl_e5RR', 'othr_Ttl_e6RR', 'othr_Ttl_e7RR',
                       'all_BnS_e1RR', 'all_BnS_e2RR', 'all_BnS_e3RR', 'all_BnS_e4RR',
                       'all_BnS_e5RR', 'all_BnS_e6RR', 'all_BnS_e7RR',
                       'all_EPM_e1RR', 'all_EPM_e2RR', 'all_EPM_e3RR', 'all_EPM_e4RR',
                       'all_EPM_e5RR', 'all_EPM_e6RR', 'all_EPM_e7RR',
                       'all_Ttl_e1RR', 'all_Ttl_e2RR', 'all_Ttl_e3RR', 'all_Ttl_e4RR',
                       'all_Ttl_e5RR', 'all_Ttl_e6RR', 'all_Ttl_e7RR'
                      )
    # }}}

} else {
    load("~/data/aud/trade/tradeXByDest.RData")
}

# }}}

