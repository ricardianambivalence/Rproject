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

    absT1 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&640101.xls&6401.0&Time%20Series%20Spreadsheet&93AB022C7BA78C6FCA257B560016340B&0&Mar%202013&24.04.2013&Latest"
    absT1xls <- read.xls(absT1, sheet = 'Data1')
    absT1 <- absT1xls[-c(1:10),]
    absT1[,1] <- as.Date(paste0(substr(absT1[,1], 5, 9), "-", substr(absT1[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT1) <- c('date', 'cpi_syd', 'cpi_melb', 'cpi_bris', 'cpi_adel', 'cpi_perth', 'cpi_hobart', 'cpi_darwin', 'cpi_canbra', 'cpi_aust',
                              'cpiY_syd', 'cpiY_melb', 'cpiY_bris', 'cpiY_adel', 'cpiY_perth', 'cpiY_hobart', 'cpiY_darwin', 'cpiY_canbra', 'cpiY_aust',
                              'cpiQ_syd', 'cpiQ_melb', 'cpiQ_bris', 'cpiQ_adel', 'cpiQ_perth', 'cpiQ_hobart', 'cpiQ_darwin', 'cpiQ_canbra', 'cpiQ_aust'
                             )
    absT1[,-1] <- sapply(absT1[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT1xls)
    save(absT1, file = "~/data/aud/cpi/cpiT1.RData")

    absT2 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&640102.xls&6401.0&Time%20Series%20Spreadsheet&F1E0E778B5C15BC0CA257B560016351B&0&Mar%202013&24.04.2013&Latest"
    absT2xls <- read.xls(absT2, sheet = 'Data1')
    absT2 <- absT2xls[-c(1:10),]
    absT2[,1] <- as.Date(paste0(substr(absT2[,1], 5, 9), "-", substr(absT2[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT2) <- c('date', 'foodBev', 'alcoholTob', 'clothFoot', 'housing', 'householdEqServ', 'health',
                              'transport', 'communication', 'recCulture', 'education', 'insFins', 'CPI',
                              'foodBev_y', 'alcoholTob_y', 'clothFoot_y', 'housing_y', 'householdEqServ_y', 'health_y',
                              'transport_y', 'communication_y', 'recCulture_y', 'education_y', 'insFins_y', 'CPI_y',
                              'foodBev_q', 'alcoholTob_q', 'clothFoot_q', 'housing_q', 'householdEqServ_q', 'health_q',
                              'transport_q', 'communication_q', 'recCulture_q', 'education_q', 'insFins_q', 'CPI_q'
                              )
    absT2[,-1] <- sapply(absT2[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT2xls)
    save(absT2, file = "~/data/aud/cpi/cpiT2.RData")

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

    rbaG01 <- "http://www.rba.gov.au/statistics/tables/xls/g01hist.xls"
    rbaG01xls <- read.xls(rbaG01, sheet = 'Data')
    rbaG01 <- rbaG01xls[-c(1:8),]
    mems <- rbaG01xls[8,]
    mems[1] <- 'date'
    goodCols <- which(!is.na(mems))
    rbaG01 <- rbaG01[, goodCols]
    names(rbaG01) <- sapply(mems[goodCols], FUN = function(X) as.character(X))
    rbaG01[,1] <- as.Date(paste0(substr(rbaG01[,1], 5, 9), "-", substr(rbaG01[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    rbaG01[,-1] <- sapply(rbaG01[, -1], FUN = function(X) as.numeric(as.character(X)))
    goodRows <- which(!is.na(rbaG01[, 12]))
    rbaG01 <- rbaG01[goodRows, ]
    # rm(rbaG01xls)
    save(rbaG01, file = "~/data/aud/cpi/rbaG01.RData")

} else {
    load("~/data/aud/cpi/cpiT1.RData")
    load("~/data/aud/cpi/cpiT2.RData")
    load("~/data/aud/cpi/cpiT8.RData")
    load("~/data/aud/cpi/rbaG01.RData")
}


# join ABS and RBA WM and TM data
rbaCore <- xtsF(rbaG01[, c('date','GCPIOCPMWMYP', 'GCPIOCPMTMYP', 'GCPIOCPMWMQP', 'GCPIOCPMTMQP')])

absCoreIdx <- xtsF(absT8[, c(1,5,4)])
absCore_q <- 100 * (absCoreIdx / lag(absCoreIdx) - 1) # good for absCore_q['20020901::']
absCore_y <- 100 * (absCoreIdx / lag(absCoreIdx, 4) - 1) # good for absCore_q['20030601::']
absCore <- merge(absCore_y, absCore_q)
names(absCore) <- names(rbaCore)

longCore_q <- rbind(rbaCore['::20020601', 3:4], absCore['20020901::', 3:4])
longCore_y <- rbind(rbaCore['::20030301', 1:2], absCore['20030601::', 1:2])
longCores <- merge(longCore_q, longCore_y)
names(longCores) <- c('CPI_WMq', 'CPI_TMq', 'CPI_WMy', 'CPI_TMy')

gp_tm <- ggplot() +
            geom_bar(data = subset(meltx(longCores), variable %in% c('CPI_TMq') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(meltx(longCores), variable %in% c('CPI_TMy') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'Trimmed Mean CPI') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "trimmedMean.png"))
grid.arrange(gp_tm, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_wm <- ggplot() +
            geom_bar(data = subset(meltx(longCores), variable %in% c('CPI_WMq') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(meltx(longCores), variable %in% c('CPI_WMy') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'Weighted Median CPI') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "weightedMedian.png"))
grid.arrange(gp_wm, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpi <- ggplot() +
            geom_bar(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_q') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_y') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'CPI (NSA)') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "cpi.png"))
grid.arrange(gp_cpi, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpiSA <- ggplot() +
            geom_bar(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_sa_q') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_sa_y') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'Seasonally Adjusted CPI') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "cpiSA.png"))
grid.arrange(gp_cpiSA, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpiXFF <- ggplot() +
            geom_bar(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_XfoodEnergy_q') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_XfoodEnergy_y') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'CPI excluding Food and Energy (NSA)') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "cpiXFF.png"))
grid.arrange(gp_cpiXFF, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpiTrad <- ggplot() +
            geom_bar(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_tradable_q') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_tradable_y') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'Tradable CPI (NSA)') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "cpiTrad.png"))
grid.arrange(gp_cpiTrad, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpiNonTrad <- ggplot() +
            geom_bar(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_nontradable_q') & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable),
                     stat = 'identity') +
            scale_fill_brewer(palette = 'Set1') +
            geom_line(data = subset(melt(absT8, id.var = 1), variable %in% c('CPI_nontradable_y') & date > as.Date('2002-01-01')),
                      aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            labs(y = NULL, x = NULL) +
            labs(title = 'Non-Tradable CPI (NSA)') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "cpiNonTrad.png"))
grid.arrange(gp_cpiNonTrad, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpiCityLil4 <- ggplot() +
            geom_line(data = subset(melt(absT1, id.var = 1), variable %in%
                                   c('cpiY_adel', 'cpiY_hobart', 'cpiY_darwin', 'cpiY_canbra', 'cpiY_aust')
                                   & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            facet_grid(variable ~ . ) +
            scale_fill_brewer(palette = 'Set1') +
            labs(y = NULL, x = NULL) +
            labs(title = 'CPI by capital city') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "city_lil4.png"))
grid.arrange(gp_cpiCityLil4, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_cpiCityTop4 <- ggplot() +
            geom_line(data = subset(melt(absT1, id.var = 1), variable %in%
                                   c('cpiY_syd', 'cpiY_melb', 'cpiY_bris', 'cpiY_perth', 'cpiY_aust')
                                   & date > as.Date('2002-01-01')),
                     aes(x = date, y = value, fill = variable, color = variable), size = 1.5) +
            facet_grid(variable ~ . ) +
            scale_fill_brewer(palette = 'Set1') +
            labs(y = NULL, x = NULL) +
            labs(title = 'CPI by capital city') +
            theme(legend.position="none",
                  legend.direction="vertical",
                  legend.title = element_blank()
                 )
png(file.path(plotPATH, "cityBig4.png"))
grid.arrange(gp_cpiCityTop4, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
