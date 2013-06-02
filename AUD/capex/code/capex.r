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

getABS <- FALSE # new data, or from the store?

# {{{ data stuff

## download from web and format or get from store?
if (getABS) {
# {{{ file paths -- html addy
    capexT1a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625001a.xls&5625.0&Time%20Series%20Spreadsheet&D9D8A1CB23C5992FCA257B7A0018F0AB&0&March%202013&30.05.2013&Latest"
    capexT1b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625001a.xls&5625.0&Time%20Series%20Spreadsheet&D9D8A1CB23C5992FCA257B7A0018F0AB&0&March%202013&30.05.2013&Latest"
    capexT1c <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625001c.xls&5625.0&Time%20Series%20Spreadsheet&C9523DCD4D1151F7CA257B7A0018F70C&0&March%202013&30.05.2013&Latest"
    capexT1e <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625001e.xls&5625.0&Time%20Series%20Spreadsheet&FA0752443C411F47CA257B7A0018F7A5&0&March%202013&30.05.2013&Latest"
    capexT1f <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625001f.xls&5625.0&Time%20Series%20Spreadsheet&7D3FF74795ACA6EBCA257B7A0018F846&0&March%202013&30.05.2013&Latest"
    capexT2a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625002a.xls&5625.0&Time%20Series%20Spreadsheet&6D6835EE754FB91DCA257B7A0018F8E0&0&March%202013&30.05.2013&Latest"
    capexT2b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625002b.xls&5625.0&Time%20Series%20Spreadsheet&A88688F3E1A8D040CA257B7A0018F97E&0&March%202013&30.05.2013&Latest"
    capexT2c <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625002c.xls&5625.0&Time%20Series%20Spreadsheet&B751460C105CF01BCA257B7A0018FA23&0&March%202013&30.05.2013&Latest"
    capexT2e <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625002e.xls&5625.0&Time%20Series%20Spreadsheet&A6129D5540E5A75ACA257B7A0018FACB&0&March%202013&30.05.2013&Latest"
    capexT2f <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625002f.xls&5625.0&Time%20Series%20Spreadsheet&1B75E3D9105A0318CA257B7A0018FB63&0&March%202013&30.05.2013&Latest"
    capexT3a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625003a.xls&5625.0&Time%20Series%20Spreadsheet&C70066FC2601E670CA257B7A0018FC02&0&March%202013&30.05.2013&Latest"
    capexT3b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625003b.xls&5625.0&Time%20Series%20Spreadsheet&76B8FA728C0D7C47CA257B7A0018FC91&0&March%202013&30.05.2013&Latest"
    capexT4a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625004a.xls&5625.0&Time%20Series%20Spreadsheet&644FDB13E52CDABACA257B7A0018FD21&0&March%202013&30.05.2013&Latest"
    capexT4b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625004b.xls&5625.0&Time%20Series%20Spreadsheet&81483A9419508F90CA257B7A0018FDB5&0&March%202013&30.05.2013&Latest"
    capexT4c <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625004c.xls&5625.0&Time%20Series%20Spreadsheet&54E64F25C45DA453CA257B7A0018FE64&0&March%202013&30.05.2013&Latest"
    capexT5a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625005a.xls&5625.0&Time%20Series%20Spreadsheet&4A458C0EE5FA2F79CA257B7A0018FF09&0&March%202013&30.05.2013&Latest"
    capexT5b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625005b.xls&5625.0&Time%20Series%20Spreadsheet&D9D5CE41C2C05951CA257B7A0018FFAB&0&March%202013&30.05.2013&Latest"
    capexT5c <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625005c.xls&5625.0&Time%20Series%20Spreadsheet&A218C3A469B5363DCA257B7A00190052&0&March%202013&30.05.2013&Latest"
    capexT6a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625006a.xls&5625.0&Time%20Series%20Spreadsheet&A1BC836368A39050CA257B7A001900E8&0&March%202013&30.05.2013&Latest"
    capexT6b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625006b.xls&5625.0&Time%20Series%20Spreadsheet&999554A590313245CA257B7A00190189&0&March%202013&30.05.2013&Latest"
    capexT7a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625007a.xls&5625.0&Time%20Series%20Spreadsheet&E6E05F4108651D07CA257B7A00190228&0&March%202013&30.05.2013&Latest"
    capexT7b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625007b.xls&5625.0&Time%20Series%20Spreadsheet&7D9DCC83366CC03ECA257B7A001902CA&0&March%202013&30.05.2013&Latest"
    capexT8a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625008a.xls&5625.0&Time%20Series%20Spreadsheet&1B7A0CB731B258ACCA257B7A0019036C&0&March%202013&30.05.2013&Latest"
    capexT8b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625009b.xls&5625.0&Time%20Series%20Spreadsheet&0A70783BD1709037CA257B7A0019054B&0&March%202013&30.05.2013&Latest"
    capexT9a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625009a.xls&5625.0&Time%20Series%20Spreadsheet&E28996DBAB6FF607CA257B7A001904AB&0&March%202013&30.05.2013&Latest"
    capexT9b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5625009b.xls&5625.0&Time%20Series%20Spreadsheet&0A70783BD1709037CA257B7A0019054B&0&March%202013&30.05.2013&Latest"
    capexT10a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56250010a.xls&5625.0&Time%20Series%20Spreadsheet&3BB84D1C3C4AB99ECA257B7A001905E6&0&March%202013&30.05.2013&Latest"
    capexT10b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56250010b.xls&5625.0&Time%20Series%20Spreadsheet&7430769A1F072C43CA257B7A00190682&0&March%202013&30.05.2013&Latest"
    capexT11a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56250011a.xls&5625.0&Time%20Series%20Spreadsheet&A2D9054BC7CA62B3CA257B7A0019072D&0&March%202013&30.05.2013&Latest"
    capexT11b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56250011b.xls&5625.0&Time%20Series%20Spreadsheet&66A4EE2B1EF6C082CA257B7A001907C9&0&March%202013&30.05.2013&Latest"
    capexT12a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56250012a.xls&5625.0&Time%20Series%20Spreadsheet&12347313402139CACA257B7A00190862&0&March%202013&30.05.2013&Latest"
    capexT12b <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&56250012b.xls&5625.0&Time%20Series%20Spreadsheet&0449CA64CA305A0ECA257B7A001908FE&0&March%202013&30.05.2013&Latest"
# }}}

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
# }}} close get data

    save(cxT1a, cxT1b, cxT1c, cxT1e, cxT1f, cxT2a, cxT2b, cxT2c, cxT2e, cxT2f,
         cxT3a, cxT3b, cxT12a, cxT12b,
         file = file.path(dataPATH, "capexData.rdata"))

} else {
    load(file.path(dataPATH, "capexData.rdata"))
}

# }}}

# {{{ data analysis

# {{{ realisation ratio stuff
RR_calc <- cxT12a # make more accurate RRatios
RR_calc[, seq(7, 84, 7)] <- 1

for (i in c(1:6, 8:13, 15:20, 22:27, 29:34, 36:41, 43:48, 50:55, 57:62, 64:69, 71:76, 78:83)) {
    RR_calc[,i] <- cxT12a[, (i %/% 7 + 1)*7] / cxT12a[, i]
}

RR5y <- rollapplyr(RR_calc, 5, colMeans)

RR5y_PredFrame <- cxT12a
est_1n2 <- sort(c(seq(1, 78, 7), seq(2, 79, 7)))
est_3t6 <- sort(c(seq(3, 80, 7), seq(4, 81, 7), seq(5, 82, 7), seq(6, 82, 7)))
RR5y_PredFrame[,est_1n2] <- RR5y_PredFrame[,est_1n2] * lag(RR5y[, est_1n2], 2)
RR5y_PredFrame[,est_3t6] <- RR5y_PredFrame[,est_3t6] * lag(RR5y[, est_3t6], 1)

# {{{ combine RR5y_PredFrame forecasts
RR5y_PredFrame$combo_Ttl_e1 <- apply(RR5y_PredFrame[, c(1, 8, 22, 29, 43, 50)], 1, sum)
RR5y_PredFrame$combo_Ttl_e2 <- apply(RR5y_PredFrame[, c(2, 9, 23, 30, 44, 51)], 1, sum)
RR5y_PredFrame$combo_Ttl_e3 <- apply(RR5y_PredFrame[, c(3, 10, 24, 31, 45, 52)], 1, sum)
RR5y_PredFrame$combo_Ttl_e4 <- apply(RR5y_PredFrame[, c(4, 11, 25, 32, 46, 53)], 1, sum)
RR5y_PredFrame$combo_Ttl_e5 <- apply(RR5y_PredFrame[, c(5, 12, 26, 33, 47, 54)], 1, sum)
RR5y_PredFrame$combo_Ttl_e6 <- apply(RR5y_PredFrame[, c(6, 13, 27, 34, 48, 55)], 1, sum)
RR5y_PredFrame$combo_Ttl_e7 <- apply(RR5y_PredFrame[, c(7, 14, 28, 35, 49, 56)], 1, sum)
#
RR5y_PredFrame$combo_BnS_e1 <- apply(RR5y_PredFrame[, c(1, 22, 43)], 1, sum)
RR5y_PredFrame$combo_BnS_e2 <- apply(RR5y_PredFrame[, c(2, 23, 44)], 1, sum)
RR5y_PredFrame$combo_BnS_e3 <- apply(RR5y_PredFrame[, c(3, 24, 45)], 1, sum)
RR5y_PredFrame$combo_BnS_e4 <- apply(RR5y_PredFrame[, c(4, 25, 46)], 1, sum)
RR5y_PredFrame$combo_BnS_e5 <- apply(RR5y_PredFrame[, c(5, 26, 47)], 1, sum)
RR5y_PredFrame$combo_BnS_e6 <- apply(RR5y_PredFrame[, c(6, 27, 48)], 1, sum)
RR5y_PredFrame$combo_BnS_e7 <- apply(RR5y_PredFrame[, c(7, 28, 49)], 1, sum)
#
RR5y_PredFrame$combo_EPM_e1 <- apply(RR5y_PredFrame[, c(8, 29, 50)], 1, sum)
RR5y_PredFrame$combo_EPM_e2 <- apply(RR5y_PredFrame[, c(9, 30, 51)], 1, sum)
RR5y_PredFrame$combo_EPM_e3 <- apply(RR5y_PredFrame[, c(10, 31, 52)], 1, sum)
RR5y_PredFrame$combo_EPM_e4 <- apply(RR5y_PredFrame[, c(11, 32, 53)], 1, sum)
RR5y_PredFrame$combo_EPM_e5 <- apply(RR5y_PredFrame[, c(12, 33, 54)], 1, sum)
RR5y_PredFrame$combo_EPM_e6 <- apply(RR5y_PredFrame[, c(13, 34, 55)], 1, sum)
RR5y_PredFrame$combo_EPM_e7 <- apply(RR5y_PredFrame[, c(14, 35, 56)], 1, sum)
# }}} close combine forecasts

RRprior_PredFrame <- cxT12a
RRprior_PredFrame[, est_1n2] <- RRprior_PredFrame[, est_1n2] * lag(RR_calc[, est_1n2], 2)
RRprior_PredFrame[, est_3t6] <- RRprior_PredFrame[, est_3t6] * lag(RR_calc[, est_3t6], 1)

# {{{ combine RRprior_PredFrame forecasts
RRprior_PredFrame$combo_Ttl_e1 <- apply(RRprior_PredFrame[, c(1, 8, 22, 29, 43, 50)], 1, sum)
RRprior_PredFrame$combo_Ttl_e2 <- apply(RRprior_PredFrame[, c(2, 9, 23, 30, 44, 51)], 1, sum)
RRprior_PredFrame$combo_Ttl_e3 <- apply(RRprior_PredFrame[, c(3, 10, 24, 31, 45, 52)], 1, sum)
RRprior_PredFrame$combo_Ttl_e4 <- apply(RRprior_PredFrame[, c(4, 11, 25, 32, 46, 53)], 1, sum)
RRprior_PredFrame$combo_Ttl_e5 <- apply(RRprior_PredFrame[, c(5, 12, 26, 33, 47, 54)], 1, sum)
RRprior_PredFrame$combo_Ttl_e6 <- apply(RRprior_PredFrame[, c(6, 13, 27, 34, 48, 55)], 1, sum)
RRprior_PredFrame$combo_Ttl_e7 <- apply(RRprior_PredFrame[, c(7, 14, 28, 35, 49, 56)], 1, sum)
#
RRprior_PredFrame$combo_BnS_e1 <- apply(RRprior_PredFrame[, c(1, 22, 43)], 1, sum)
RRprior_PredFrame$combo_BnS_e2 <- apply(RRprior_PredFrame[, c(2, 23, 44)], 1, sum)
RRprior_PredFrame$combo_BnS_e3 <- apply(RRprior_PredFrame[, c(3, 24, 45)], 1, sum)
RRprior_PredFrame$combo_BnS_e4 <- apply(RRprior_PredFrame[, c(4, 25, 46)], 1, sum)
RRprior_PredFrame$combo_BnS_e5 <- apply(RRprior_PredFrame[, c(5, 26, 47)], 1, sum)
RRprior_PredFrame$combo_BnS_e6 <- apply(RRprior_PredFrame[, c(6, 27, 48)], 1, sum)
RRprior_PredFrame$combo_BnS_e7 <- apply(RRprior_PredFrame[, c(7, 28, 49)], 1, sum)
#
RRprior_PredFrame$combo_EPM_e1 <- apply(RRprior_PredFrame[, c(8, 29, 50)], 1, sum)
RRprior_PredFrame$combo_EPM_e2 <- apply(RRprior_PredFrame[, c(9, 30, 51)], 1, sum)
RRprior_PredFrame$combo_EPM_e3 <- apply(RRprior_PredFrame[, c(10, 31, 52)], 1, sum)
RRprior_PredFrame$combo_EPM_e4 <- apply(RRprior_PredFrame[, c(11, 32, 53)], 1, sum)
RRprior_PredFrame$combo_EPM_e5 <- apply(RRprior_PredFrame[, c(12, 33, 54)], 1, sum)
RRprior_PredFrame$combo_EPM_e6 <- apply(RRprior_PredFrame[, c(13, 34, 55)], 1, sum)
RRprior_PredFrame$combo_EPM_e7 <- apply(RRprior_PredFrame[, c(14, 35, 56)], 1, sum)
# }}} close combo of forecasts

# }}} close realisation ratio stuff

# {{{ regression adjustment
modList = list()
sector = 'all'

getSector <- function(code) {
    sector <- switch(as.character(code),
                     "0" = "min_BnS",
                     "1" = "min_EPM",
                     "2" = "min_Ttl",
                     "3" = "manu_BnS",
                     "4" = "manu_EPM",
                     "5" = "manu_Ttl",
                     "6" = "othr_BnS",
                     "7" = "othr_EPM",
                     "8" = "othr_Ttl",
                     "9" = "all_BnS",
                     "10" = "all_EPM",
                     "11" = "all_Ttl")
}

for (i in seq(7, 84, 7)) {
    for (j in 1:6) {
        sector <- getSector(i %/% 7 - 1)
        modList[[sector]][[j]] <- lm(log(cxT12a[, i]) ~ log(cxT12a[,(i-7+j)]))
    }
}

RegPredFrame <- cxT12a
for (i in c(1:6, 8:13, 15:20, 22:27, 29:34, 36:41, 43:48, 50:55, 57:62, 64:69, 71:76, 78:83)) {
    sector <- getSector(i %/% 7)
    est <- i %% 7
    RegPredFrame[,i] <- exp(modList[[sector]][[est]]$coefficients[1] +
                        modList[[sector]][[est]]$coefficients[2] * log(cxT12a[,i]))
}

# {{{ combine forecasts
RegPredFrame$combo_Ttl_e1 <- apply(RegPredFrame[, c(1, 8, 22, 29, 43, 50)], 1, sum)
RegPredFrame$combo_Ttl_e2 <- apply(RegPredFrame[, c(2, 9, 23, 30, 44, 51)], 1, sum)
RegPredFrame$combo_Ttl_e3 <- apply(RegPredFrame[, c(3, 10, 24, 31, 45, 52)], 1, sum)
RegPredFrame$combo_Ttl_e4 <- apply(RegPredFrame[, c(4, 11, 25, 32, 46, 53)], 1, sum)
RegPredFrame$combo_Ttl_e5 <- apply(RegPredFrame[, c(5, 12, 26, 33, 47, 54)], 1, sum)
RegPredFrame$combo_Ttl_e6 <- apply(RegPredFrame[, c(6, 13, 27, 34, 48, 55)], 1, sum)
RegPredFrame$combo_Ttl_e7 <- apply(RegPredFrame[, c(7, 14, 28, 35, 49, 56)], 1, sum)
#
RegPredFrame$combo_BnS_e1 <- apply(RegPredFrame[, c(1, 22, 43)], 1, sum)
RegPredFrame$combo_BnS_e2 <- apply(RegPredFrame[, c(2, 23, 44)], 1, sum)
RegPredFrame$combo_BnS_e3 <- apply(RegPredFrame[, c(3, 24, 45)], 1, sum)
RegPredFrame$combo_BnS_e4 <- apply(RegPredFrame[, c(4, 25, 46)], 1, sum)
RegPredFrame$combo_BnS_e5 <- apply(RegPredFrame[, c(5, 26, 47)], 1, sum)
RegPredFrame$combo_BnS_e6 <- apply(RegPredFrame[, c(6, 27, 48)], 1, sum)
RegPredFrame$combo_BnS_e7 <- apply(RegPredFrame[, c(7, 28, 49)], 1, sum)
#
RegPredFrame$combo_EPM_e1 <- apply(RegPredFrame[, c(8, 29, 50)], 1, sum)
RegPredFrame$combo_EPM_e2 <- apply(RegPredFrame[, c(9, 30, 51)], 1, sum)
RegPredFrame$combo_EPM_e3 <- apply(RegPredFrame[, c(10, 31, 52)], 1, sum)
RegPredFrame$combo_EPM_e4 <- apply(RegPredFrame[, c(11, 32, 53)], 1, sum)
RegPredFrame$combo_EPM_e5 <- apply(RegPredFrame[, c(12, 33, 54)], 1, sum)
RegPredFrame$combo_EPM_e6 <- apply(RegPredFrame[, c(13, 34, 55)], 1, sum)
RegPredFrame$combo_EPM_e7 <- apply(RegPredFrame[, c(14, 35, 56)], 1, sum)
# }}} close combo of forecasts



# }}} close reg adj

# }}} close data analysis


# {{{ plots
# percent difference between regression adj and actual
plot(100*(RegPredFrame[,6] / cxT12a[,7] - 1), type='o', pch=19, las=2, major.format="%Y")

# plot of mining BnS by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 1:7]/1e3), type='l',
     ylim = c(20, 120), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Mining Buildings and Structures (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 1:7]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 7]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 1:7]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 7]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 1:7]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 7]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 1:7]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 7]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 1:7]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 7]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 1:7]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 7]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of mining EPM by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 8:14]/1e3), type='l',
     ylim = c(5, 20), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Mining Equip Plant and Machinery (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 8:14]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 14]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 8:14]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 14]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 8:14]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 14]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 8:14]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 14]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 8:14]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 14]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 8:14]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 14]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of manu BnS by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 22:28]/1e3), type='l',
     ylim = c(2, 8), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Manu Buildings and Structures (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 22:28]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 28]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 22:28]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 28]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 22:28]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 28]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 22:28]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 28]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 22:28]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 28]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 22:28]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 28]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of manu EPM by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 29:35]/1e3), type='l',
     ylim = c(6, 10), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Manu Equip Plant and Machinery (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 29:35]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 35]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 29:35]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 35]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 29:35]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 35]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 29:35]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 35]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 29:35]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 35]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 29:35]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 35]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of other BnS by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 43:49]/1e3), type='l',
     ylim = c(15, 30), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Other Buildings and Structures (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 43:49]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 49]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 43:49]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 49]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 43:49]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 49]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 43:49]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 49]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 43:49]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 49]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 43:49]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 49]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of other EPM by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 50:56]/1e3), type='l',
     ylim = c(30, 45), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Other Equip Plant and Machinery (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 50:56]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 56]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 50:56]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 56]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 50:56]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 56]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 50:56]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 56]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 50:56]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 56]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 50:56]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 56]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))


# plot of other BnS by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 92:98]/1e3), type='l',
     ylim = c(40, 140), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Other Buildings and Structures (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 92:98]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 98]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 92:98]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 98]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 92:98]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 98]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 92:98]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 98]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 92:98]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 98]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 92:98]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 98]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of other EPM by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 99:105]/1e3), type='l',
     ylim = c(45, 65), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Total (all sectors) Equip Plant and Machinery (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 99:105]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 105]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 99:105]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 105]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 99:105]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 105]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 99:105]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 105]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 99:105]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 105]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 99:105]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 105]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))

# plot of combo all capex by forecast vintage
plot(as.numeric(RegPredFrame['20140601', 85:91]/1e3), type='l',
     ylim = c(80, 200), col = 1, lwd=3,
     xlab = "est number", ylab = "AUDbn", las=1,
     main = "Total (all types, all sectors) Capex (Reg RR adj)")
lines(as.numeric(RegPredFrame['20130601', 85:91]/1e3), col = 2, lwd=2)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20130601', 91]/1e3)), col = 2, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20120601', 85:91]/1e3), col = 3)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20120601', 91]/1e3)), col = 3, type = 'o', pch = 4)
lines(as.numeric(RegPredFrame['20110601', 85:91]/1e3), col = 4)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20110601', 91]/1e3)), col = 4, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20100601', 85:91]/1e3), col = 5)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20100601', 91]/1e3)), col = 5, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20090601', 85:91]/1e3), col = 6)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20090601', 91]/1e3)), col = 6, type = 'o', pch=4)
lines(as.numeric(RegPredFrame['20080601', 85:91]/1e3), col = 8)
lines(c(rep(NA, 6), as.numeric(RegPredFrame['20080601', 91]/1e3)), col = 8, type = 'o', pch=4)
legend('topright', c('FY14', 'FY13', 'FY12', 'FY11', 'FY10', 'FY09', 'FY08'),
       col = c(1:6, 8), lwd = c(3, 2, rep(1, 5)))
# }}}
