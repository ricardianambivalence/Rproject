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

## get from web or saved xls?
getWeb <- FALSE

## PATH stuff
projectPATH <- "~/R/AUD/gdp"
plotPATH <- file.path(projectPATH, "pics")
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")

## download from web and format or get from store?
if (getWeb)
{
    absT1 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.xls&5206.0&Time%20Series%20Spreadsheet&A246FCFF27391908CA257B2500116904&0&Dec%202012&06.03.2013&Latest"
    absT1xls <- read.xls(absT1, sheet = 'Data1', as.is=TRUE)
    absT1 <- absT1xls[-c(1:10),]
    absT1[,1] <- as.Date(paste0(substr(absT1[,1], 5, 9), "-", substr(absT1[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT1) <- c('date', 'gdp_t_qq', 'gdpPcap_t_qq', 'gvaMkt_t_qq', 'ndp_t_qq', 'rGDP_t_qq', 'rGNI_t_qq',
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
    absT1[,-1] <- sapply(absT1[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT1xls)
    save(absT1, file = file.path(dataPATH, "gdpT1.RData"))

} else {
    load("~/r/aud/gdp/data/gdpT1.RData")
}
