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
                      'Ttl_nom_nsa', 'minCoal_nom_nsa', 'minOilGas_nom_nsa',
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
                      'profScient_stX', 'othrServ_stX', 'otherAll_stX',
                      'Ttl_stX', 'minCoal_stX', 'minOilGas_stX',
                      'minOre_stX', 'minNonMetal_stX', 'minExplr_stX'
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
                      'profScient_stX', 'othrServ_stX', 'otherAll_stX', 'Ttl_stX'
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
                      'profScient_ltX', 'othrServ_ltX', 'otherAll_ltX', 'Ttl_ltX'
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
                      'profScient_nom_sa', 'othrServ_nom_sa', 'otherAll_nom_sa', 'Ttl_nom_sa'
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
                      'profScient_nom_t', 'othrServ_nom_t', 'otherAll_nom_t', 'Ttl_nom_t'
                      )
    # }}}

} else {
    load("~/data/aud/trade/tradeXByDest.RData")
}


