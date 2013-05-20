# get the ABS files and make some cpi charts
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

## get from web or saved xls?
getWeb <- TRUE

## PATH stuff
projectPATH <- "~/R/AUD/cpi"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- "~/data/aud/cpi"

## download from web and format or get from store?
if (getWeb)
{
    absT8 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&640106.xls&6401.0&Time%20Series%20Spreadsheet&D5C3A9F4C8A53FDACA257B5600163981&0&Mar%202013&24.04.2013&Latest"
    absT8xls <- read.xls(absT8, sheet = 'Data1')
    absT8 <- absT8xls[-c(1:10),]
    absT8[,1] <- as.Date(paste0(substr(absT8[,1], 5, 9), "-", substr(absT8[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT8) <- c('date', 'CPI', 'CPI_sa', 'CPI_tm', 'CPI_wm', 'CPI_tradable', 'CPI_nontradable', 'CPI_goods',
                              'CPI_services', 'CPI_incIndirectFin', 'CPI_mktGoodsXvol', 'CPI_mktServXvol', 'CPI_mktXvol',
                              'CPI_Xfood', 'CPI_XalcoTob', 'CPI_XclothFoot', 'CPI_Xhousing', 'CPI_Xhousehold', 'CPI_Xhealth',
                              'CPI_Xtransport', 'CPI_Xcomms', 'CPI_Xrec', 'CPI_Xedu', 'CPI_XinsFins',
                              'CPI_XhousingInsFins', 'CPI_Xmedical', 'CPI_XfoodEnergy', 'CPI_Xvol',
                              'CPI_q', 'CPI_sa_q', 'CPI_tm_q', 'CPI_wm_q', 'CPI_tradable_q', 'CPI_nontradable_q', 'CPI_goods_q',
                              'CPI_services_q', 'CPI_incIndirectFin_q', 'CPI_mktGoodsXvol_q', 'CPI_mktServXvol_q', 'CPI_mktXvol_q',
                              'CPI_Xfood_q', 'CPI_XalcoTob_q', 'CPI_XclothFoot_q', 'CPI_Xhousing_q', 'CPI_Xhousehold_q', 'CPI_Xhealth_q',
                              'CPI_Xtransport_q', 'CPI_Xcomms_q', 'CPI_Xrec_q', 'CPI_Xedu_q', 'CPI_XinsFins_q',
                              'CPI_XhousingInsFins_q', 'CPI_Xmedical_q', 'CPI_XfoodEnergy_q', 'CPI_Xvol_q',
                              'CPI_y', 'CPI_sa_y', 'CPI_tm_y', 'CPI_wm_y', 'CPI_tradable_y', 'CPI_nontradable_y', 'CPI_goods_y',
                              'CPI_services_y', 'CPI_incIndirectFin_y', 'CPI_mktGoodsXvol_y', 'CPI_mktServXvol_y', 'CPI_mktXvol_y',
                              'CPI_Xfood_y', 'CPI_XalcoTob_y', 'CPI_XclothFoot_y', 'CPI_Xhousing_y', 'CPI_Xhousehold_y', 'CPI_Xhealth_y',
                              'CPI_Xtransport_y', 'CPI_Xcomms_y', 'CPI_Xrec_y', 'CPI_Xedu_y', 'CPI_XinsFins_y',
                              'CPI_XhousingInsFins_y', 'CPI_Xmedical_y', 'CPI_XfoodEnergy_y', 'CPI_Xvol_y',
                              'CPI_cont', 'CPI_tradable_cont', 'CPI_nontradable_cont', 'CPI_goods_cont',
                              'CPI_services_cont', 'CPI_mktGoodsXvol_cont', 'CPI_mktServXvol_cont', 'CPI_mktXvol_cont',
                              'CPI_Xfood_cont', 'CPI_XalcoTob_cont', 'CPI_XclothFoot_cont', 'CPI_Xhousing_cont', 'CPI_Xhousehold_cont', 'CPI_Xhealth_cont',
                              'CPI_Xtransport_cont', 'CPI_Xcomms_cont', 'CPI_Xrec_cont', 'CPI_Xedu_cont', 'CPI_XinsFins_cont',
                              'CPI_XhousingInsFins_cont', 'CPI_Xmedical_cont', 'CPI_XfoodEnergy_cont', 'CPI_Xvol_cont',
                              'CPI_Dcont', 'CPI_tradable_Dcont', 'CPI_nontradable_Dcont', 'CPI_goods_Dcont',
                              'CPI_services_Dcont', 'CPI_mktGoodsXvol_Dcont', 'CPI_mktServXvol_Dcont', 'CPI_mktXvol_Dcont',
                              'CPI_Xfood_Dcont', 'CPI_XalcoTob_Dcont', 'CPI_XclothFoot_Dcont', 'CPI_Xhousing_Dcont', 'CPI_Xhousehold_Dcont', 'CPI_Xhealth_Dcont',
                              'CPI_Xtransport_Dcont', 'CPI_Xcomms_Dcont', 'CPI_Xrec_Dcont', 'CPI_Xedu_Dcont', 'CPI_XinsFins_Dcont',
                              'CPI_XhousingInsFins_Dcont', 'CPI_Xmedical_Dcont', 'CPI_XfoodEnergy_Dcont', 'CPI_Xvol_Dcont'
                             )
    absT8[,-1] <- sapply(absT8[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT8xls)
    save(absT8, file = "~/data/aud/cpi/cpiT8.RData")

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
    load("~/data/aud/cpi/cpiT8.RData")
    load("~/r/aud/gdp/data/gdpT1.RData")
}

tradNtrad <- data.frame('date' = absT8$date[-c(1:44)],
                        'tNt' = absT8$CPI_nontradable[-c(1:44)] / absT8$CPI_tradable[-c(1:44)] * 100)

tradNtradx <- xtsF(tradNtrad)

tNt_tot <- merge(tradNtradx, xtsF(absT1[, c(1, 109)]))
names(tNt_tot) <- c('tNt', 'tot_nsa')

sDF <- data.frame(coredata(tNt_tot['19980601/20031231']),
                  tt=as.numeric(index(tNt_tot['19980601/20031231'])))
tNt_trend <- lm(tNt ~ tt, data=sDF)

plot(tNt_tot['19980601::', 1], main = "Ratio of non-tradable to tradable CPI",
     type = 'o', major.format = "%b-%y", las=1)
abline(coef(tNt_trend), col=2, lty = 2, lwd=2)
legend('topleft', c('NonT/T CPI ratio', '1998-2003 trend'),
       pch = c(1, NA), lwd = 2, col = c(1,2), horiz = TRUE, bg = 'grey91')
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: ABS', side=1, line=4, adj=0)

# now make a 2 axis one showing the ToT also
plot(tNt_tot['19980601::', 1], main = "Ratio of non-tradable to tradable CPI",
     type = 'o', major.format = "%b-%y", las=1)
abline(coef(tNt_trend), col=2, lty = 2, lwd=2)
legend('topleft', c('NonT/T CPI ratio', '1998-2003 trend'),
       pch = c(1, NA), lwd = 2, col = c(1,2), horiz = TRUE, bg = 'grey91')
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: ABS', side=1, line=4, adj=0)






