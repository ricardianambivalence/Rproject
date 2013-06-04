# get the ABS gdp data and make some charts
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
source('~/R/Rhelpers/helperFuncts.r')
source('~/R/Rhelpers/RAcolorpal.r')

## get from web or saved xls?
getWeb <- TRUE

## PATH stuff
projectPATH <- "~/R/AUD/gdp"
plotPATH <- file.path(projectPATH, "pics")
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")

## download from web and format or get from store?
if (getWeb)
{
    gdpT1h <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.xls&5206.0&Time%20Series%20Spreadsheet&A246FCFF27391908CA257B2500116904&0&Dec%202012&06.03.2013&Latest"
    gdpT1 <- readABS(gdpT1h)
    names(gdpT1) <- c(
                      'gdp_t_qq', 'gdpPcap_t_qq', 'gvaMkt_t_qq', 'ndp_t_qq', 'rGDP_t_qq', 'rGNI_t_qq',
                      'rNNDI_t_qq_t', 'rNNDiPcap_t_qq', 'nGDP_t_qq', 'hrs_t_qq', 'hrsMkt_t_qq', 'gdpPhr_t_qq',
                      'gvaPhr_mkt_t_qq', 'rUnitLabour_t_qq', 'rNfUnitLabour_t_qq', 'tot_t_qq', 'rGDP_t_qq',
                      'rGDPcap_t_qq', 'rGVAmkt_t_qq', 'rNDP_t_qq', 'rGDI_t_qq', 'rGNI_t_qq', 'rNNDI_t_qq',
                      'rNNDIcap_t_qq', 'rGDP_t_qq', 'rGDPcap_t_qq', 'nGNI_t', 'netSaving_t', 'hhSavingRate_t',
                      'hrsWorked_t', 'hrsWorkedMkt_t', 'gdpPhr_t', 'gvaPhrMkt_t', 'rULC_t', 'rnfULC_t', 'tot_t',
                      'gdp_sa_qq', 'gdpPcap_sa_qq', 'gvaMkt_sa_qq', 'ndp_sa_qq', 'rGDP_sa_qq', 'rGNI_sa_qq',
                      'rNNDI_sa_qq', 'rNNDiPcap_sa_qq', 'nGDP_sa_qq', 'hrs_sa_qq', 'hrsMkt_sa_qq', 'gdpPhr_sa_qq',
                      'gvaPhr_mkt_sa_qq', 'rUnitLabour_sa_qq', 'rNfUnitLabour_sa_qq', 'tot_sa_qq', 'rGDP_sa_qq',
                      'rGDPcap_sa_qq', 'rGVAmkt_sa_qq', 'rNDP_sa_qq', 'rGDI_sa_qq', 'rGNI_sa_qq', 'rNNDI_sa_qq',
                      'rNNDIcap_sa_qq', 'rGDP_sa_qq', 'rGDPcap_sa_qq', 'nGNI_sa', 'netSaving_sa', 'hhSavingRate_sa',
                      'hrsWorked_sa', 'hrsWorkedMkt_sa', 'gdpPhr_sa', 'gvaPhrMkt_sa', 'rULC_sa', 'rnfULC_sa', 'tot_sa',
                      'gdp_nsa_qq', 'gdpPcap_nsa_qq', 'gvaMkt_nsa_qq', 'ndp_nsa_qq', 'rGDP_nsa_qq', 'rGNI_nsa_qq',
                      'rNNDI_nsa_qq', 'rNNDiPcap_nsa_qq', 'nGDP_nsa_qq', 'hrs_nsa_qq', 'hrsMkt_nsa_qq',
                      'gdpPhr_nsa_qq', 'gvaPhr_mkt_nsa_qq', 'rUnitLabour_nsa_qq', 'rNfUnitLabour_nsa_qq', 'tot_nsa_qq',
                      'rGDP_nsa_qq', 'rGDPcap_nsa_qq', 'rGVAmkt_nsa_qq', 'rNDP_nsa_qq', 'rGDI_nsa_qq', 'rGNI_nsa_qq',
                      'rNNDI_nsa_qq', 'rNNDIcap_nsa_qq', 'rGDP_nsa_qq', 'rGDPcap_nsa_qq', 'nGNI_nsa', 'netSaving_nsa',
                      'hhSavingRate_nsa', 'hrsWorked_nsa', 'hrsWorkedMkt_nsa', 'gdpPhr_nsa', 'gvaPhrMkt_nsa',
                      'rULC_nsa', 'rnfULC_nsa', 'tot_nsa')

    gdpT20h <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206020_selected_analytical_series.xls&5206.0&Time%20Series%20Spreadsheet&FFB25313C54C0060CA257B2500117912&0&Dec%202012&06.03.2013&Latest"
    gdpT20 <- readABS(gdpT20h)
    names(gdpT20) <- c(
                       'rGdpE_t', 'rGdpI_t', 'rGdpP_t', 'rNfGdp_t', 'nNfGdp_t', 'NfGDP_ipd_t',
                       'rFGdp_t', 'nFGdp_t', 'FGdp_ipd_t', 'rGfcfBiz_t', 'nGfcfBiz_t',
                       'rPrNfInv_t', 'nPrNfInv_t', 'nDomSales_t', 'nSales_t', 'PrInv2Sales_t',
                       'nMerchGoodIm_t', 'Im2DomSales_t', 'wageShare_t', 'ProfShare_t',
                       'nAveComp_t', 'nNfTtlComp_t', 'nNfAveComp_t',
                       'rGdpE_qq_t', 'rGdpI_qq_t', 'rGdpP_qq_t', 'rNfGdp_qq_t', 'nNfGdp_qq_t', 'NfGDP_ipd_qq_t',
                       'rFGdp_qq_t', 'nFGdp_qq_t', 'FGdp_ipd_qq_t', 'rGfcfBiz_qq_t', 'nGfcfBiz_qq_t',
                       'nAveComp_qq_t', 'nNfTtlComp_qq_t', 'nNfAveComp_qq_t',
                       'rGdpE_sa', 'rGdpI_sa', 'rGdpP_sa', 'rNfGdp_sa', 'nNfGdp_sa', 'NfGDP_ipd_sa',
                       'rFGdp_sa', 'nFGdp_sa', 'FGdp_ipd_sa', 'rGfcfBiz_sa', 'nGfcfBiz_sa',
                       'rPrNfInv_sa', 'nPrNfInv_sa', 'nDomSales_sa', 'nSales_sa', 'PrInv2Sales_sa',
                       'nMerchGoodIm_sa', 'Im2DomSales_sa', 'wageShare_sa', 'ProfShare_sa',
                       'nAveComp_sa', 'nNfTtlComp_sa', 'nNfAveComp_sa',
                       'rGdpE_qq_sa', 'rGdpI_qq_sa', 'rGdpP_qq_sa', 'rNfGdp_qq_sa', 'nNfGdp_qq_sa', 'NfGDP_ipd_qq_sa',
                       'rFGdp_qq_sa', 'nFGdp_qq_sa', 'FGdp_ipd_qq_sa', 'rGfcfBiz_qq_sa', 'nGfcfBiz_qq_sa',
                       'nAveComp_qq_sa', 'nNfTtlComp_qq_sa', 'nNfAveComp_qq_sa',
                       'rGdpE_nsa', 'rGdpI_nsa', 'rGdpP_nsa', 'rNfGdp_nsa', 'nNfGdp_nsa', 'NfGDP_ipd_nsa',
                       'rFGdp_nsa', 'nFGdp_nsa', 'FGdp_ipd_nsa', 'rGfcfBiz_nsa', 'nGfcfBiz_nsa',
                       'rPrNfInv_nsa', 'nPrNfInv_nsa', 'nDomSales_nsa', 'nSales_nsa', 'PrInv2Sales_nsa',
                       'nMerchGoodIm_nsa', 'Im2DomSales_nsa', 'wageShare_nsa', 'ProfShare_nsa',
                       'nAveComp_nsa', 'nNfTtlComp_nsa', 'nNfAveComp_nsa',
                       'rGdpE_qq_nsa', 'rGdpI_qq_nsa', 'rGdpP_qq_nsa', 'rNfGdp_qq_nsa', 'nNfGdp_qq_nsa', 'NfGDP_ipd_qq_nsa',
                       'rFGdp_qq_nsa', 'nFGdp_qq_nsa', 'FGdp_ipd_qq_nsa', 'rGfcfBiz_qq_nsa', 'nGfcfBiz_qq_nsa',
                       'nAveComp_qq_nsa', 'nNfTtlComp_qq_nsa', 'nNfAveComp_qq_nsa'
                       )

    save(gdpT1, gdpT20, file = file.path(dataPATH, "gdpTables.RData"))

} else {
    load("~/r/aud/gdp/data/gdpTables.RData")
}


