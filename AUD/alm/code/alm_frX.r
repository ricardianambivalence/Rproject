#{{{set-up Packs and Funs
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
#packages and functions
require(gdata)
require(timsac)
require(xts)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(quantmod)
source("~/Rproject/Rhelpers/helperFuncts.r")
source("~/Rproject/Rhelpers/RAcolorpal.r")
# }}}

### {{{ PATH stuff
projectPATH <- "~/Rproject/AUD/alm"
plotPATH <- file.path(projectPATH, "plot")
dataPATH <- file.path(projectPATH, "data")
codePATH <- file.path(projectPATH, "code")
# }}} close paths

## get from web or saved xls?
getWeb <- TRUE

# {{{ get data
if (getWeb)
{
    absT1 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202001.xls&6202.0&Time%20Series%20Spreadsheet&FA62848F675FC378CA257C1B000D8949&0&Oct%202013&07.11.2013&Latest"
    absT1xls <- read.xls(absT1, sheet = 'Data1')
    absT1 <- absT1xls[-c(1:10),]
    absT1[,1] <- as.Date(paste0(substr(absT1[,1], 5, 9), "-", substr(absT1[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT1) <- c('date', 'ftM', 'ftF', 'ft', 'ptM', 'ptF', 'pt', 'nM', 'nF', 'n',
                             'unFtM', 'unFtF', 'unFT', 'unPtM', 'unPtF', 'unPt', 'unM', 'unF', 'un',
                             'lfM', 'lfF', 'lf', 'urFtM', 'urFtF', 'urFt', 'urM', 'urF', 'ur',
                             'prM', 'prF', 'pr', 'nPopM', 'nPopF', 'nPop', 'unPopM', 'unPopF', 'unPop'
                             )
    absT1[,-1] <- sapply(absT1[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT1xls)
    save(absT1, file = file.path(dataPATH, "almT1.RData"))

    absT2 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202002.xls&6202.0&Time%20Series%20Spreadsheet&5EDCE161CFFE7712CA257C1B000D8A24&0&Oct%202013&07.11.2013&Latest"
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
    save(absT2, file = file.path(dataPATH, "almT2.RData"))

    absT3 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202003.xls&6202.0&Time%20Series%20Spreadsheet&0FD9F07A76FE5495CA257C1B000D8AF2&0&Oct%202013&07.11.2013&Latest"
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
    save(absT3, file = file.path(dataPATH, "almT3.RData"))

    absT19 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202019.xls&6202.0&Time%20Series%20Spreadsheet&EF81361B4DFC6826CA257C1B000DA07C&0&Oct%202013&07.11.2013&Latest"
    absT19xls <- read.xls(absT19, sheet = 'Data1')
    absT19 <- absT19xls[-c(1:10),]
    absT19[,1] <- as.Date(paste0(substr(absT19[,1], 5, 9), "-", substr(absT19[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    names(absT19) <- c('date', 'hrsFtM_t', 'hrsFtF_t', 'hrsFt_t', 'hrsPtM_t', 'hrsPtF_t',
                       'hrsPt_t', 'hrsM_t', 'hrsF_t', 'hrs_t',
                       'hrsFtM_sa', 'hrsFtF_sa', 'hrsFt_sa', 'hrsPtM_sa', 'hrsPtF_sa', 'hrsPt_sa',
                       'hrsM_sa', 'hrsF_sa', 'hrs_sa')
    absT19[,-1] <- sapply(absT19[, -1], FUN = function(X) as.numeric(as.character(X)))
    rm(absT19xls)
    save(absT19, file = file.path(dataPATH, "almT19.RData"))

    gflowsX <- file.path(dataPATH, "hazard rates.xls")
    gflowsXls <- read.xls(gflowsX, sheet = 'toR', as.is=TRUE)
    gflowsXx <- xts(gflowsXls[, -1], order.by = as.Date(gflowsXls[, 1]))
    rm(gflowsX, gflowsXls)
    save(gflowsXx, file = file.path(dataPATH, "almGFlows.RData"))
} else {
    load(file = file.path(dataPATH, "almT1.RData"))
    load(file = file.path(dataPATH, "almT2.RData"))
    load(file = file.path(dataPATH, "almT3.RData"))
    load(file = file.path(dataPATH, "almT19.RData"))
    load(file = file.path(dataPATH, "almGFlows.RData"))
}
# }}} close get data

# date subset prep
chStartYr <- as.POSIXlt(max(absT2$date))$year + 1900 - 9
xtsDateCut <- paste0(chStartYr, "::")

# convert data to an xts object
absT1x <- xts(absT1[,-1], order.by = absT1[,1])
absT2x <- xts(absT2[,-1], order.by = absT2[,1])
absT3x <- xts(absT3[,-1], order.by = absT3[,1])
absT19x <- xts(absT19[,-1], order.by = absT19[,1])

# add some stuff to the data frames
# table 1 == trend
absT1x$nilfPopM <- 100 - absT1x$nPopM - absT1x$unPopM
absT1x$nilfPopF <- 100 - absT1x$nPopF - absT1x$unPopF
absT1x$nilfPop <- 100 - absT1x$nPop - absT1x$unPop
DabsT1x <- diff(absT1x) # to subset by date add [xtsDateCut]

# table 2 == seas adj
absT2x$nilfPopM <- 100 - absT2x$nPopM - absT2x$unPopM
absT2x$nilfPopF <- 100 - absT2x$nPopF - absT2x$unPopF
absT2x$nilfPop <- 100 - absT2x$nPop - absT2x$unPop
DabsT2x <- diff(absT2x) # to subset by date add [xtsDateCut]

# table 3 == raw
absT3x$nilfPopM <- 100 - absT3x$nPopM - absT3x$unPopM
absT3x$nilfPopF <- 100 - absT3x$nPopF - absT3x$unPopF
absT3x$nilfPop <- 100 - absT3x$nPop - absT3x$unPop
DabsT3x <- diff(absT3x) # to subset by date add [xtsDateCut]

# table 19 == hrs agg monthly hrs -> xPer is per employee
absT19xPer_t <- cbind(absT19x$hrsFtM_t / absT1x$ftM, absT19x$hrsFtF_t / absT1x$ftF,
                      absT19x$hrsFt_t / absT1x$ft, absT19x$hrsPtM_t / absT1x$ptM,
                      absT19x$hrsPtF_t / absT1x$ptF, absT19x$hrsPt_t / absT1x$pt,
                      absT19x$hrsM_t / absT1x$nM, absT19x$hrsF_t / absT1x$nF,
                      absT19x$hrs_t / absT1x$n)
names(absT19xPer_t) <- c('hrsFtM', 'hrsFtF', 'hrsFt',
                         'hrsPtM', 'hrsPtF', 'hrsPt',
                         'hrsM', 'hrsF', 'hrs')
absT19xPer_sa <- cbind(absT19x$hrsFtM_sa / absT2x$ftM, absT19x$hrsFtF_sa / absT2x$ftF,
                      absT19x$hrsFt_sa / absT2x$ft, absT19x$hrsPtM_sa / absT2x$ptM,
                      absT19x$hrsPtF_sa / absT2x$ptF, absT19x$hrsPt_sa / absT2x$pt,
                      absT19x$hrsM_sa / absT2x$nM, absT19x$hrsF_sa / absT2x$nF,
                      absT19x$hrs_sa / absT2x$n)
names(absT19xPer_sa) <- c('hrsFtM', 'hrsFtF', 'hrsFt',
                         'hrsPtM', 'hrsPtF', 'hrsPt',
                         'hrsM', 'hrsF', 'hrs')

hrsN_t_melt <- melt(
                    data.frame(date = index(absT19xPer_t),
                               absT19xPer_t), measure.vars = c(2:10))

hrsN_sa_melt <- melt(
                    data.frame(date = index(absT19xPer_sa),
                               absT19xPer_sa), measure.vars = c(2:10))

# hours per worker by male and female
gg_hrsN_twin <- ggplot() +
            geom_line(data = subset(hrsN_t_melt, variable %in% c('hrsM', 'hrsF', 'hrs')),
                      aes(x = date, y = value, fill = variable), size = 1.2) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(hrsN_sa_melt, variable %in% c('hrsM', 'hrsF', 'hrs')),
                      aes(x = date, y = value, fill = variable), size = 0.35, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Ave hours worked per employee /month -- by gender")
png(file.path(plotPATH, "hrsN_twin.png"))
grid.arrange(gg_hrsN_twin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# hours per FULL TIME worker by male and female
gg_hrsFtN_twin <- ggplot() +
            geom_line(data = subset(hrsN_t_melt, variable %in% c('hrsFtM', 'hrsFtF', 'hrsFt')),
                      aes(x = date, y = value, fill = variable), size = 1.2) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(hrsN_sa_melt, variable %in% c('hrsFtM', 'hrsFtF', 'hrsFt')),
                      aes(x = date, y = value, fill = variable), size = 0.35, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Ave hours worked per full time employee /month -- by gender")
png(file.path(plotPATH, "hrsFtN_twin.png"))
grid.arrange(gg_hrsFtN_twin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# hours per PART TIME worker by male and female
gg_hrsPtN_twin <- ggplot() +
            geom_line(data = subset(hrsN_t_melt, variable %in% c('hrsPtM', 'hrsPtF', 'hrsPt')),
                      aes(x = date, y = value, fill = variable), size = 1.2) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(hrsN_sa_melt, variable %in% c('hrsPtM', 'hrsPtF', 'hrsPt')),
                      aes(x = date, y = value, fill = variable), size = 0.35, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Ave hours worked per part time employee /month -- by gender")
png(file.path(plotPATH, "hrsPtN_twin.png"))
grid.arrange(gg_hrsPtN_twin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

hrsPop_t <- cbind(absT19x$hrsM_t / absT3x$popM, absT19x$hrsF_t / absT3x$popF,
                      absT19x$hrs_t / absT3x$pop)
names(hrsPop_t) <- c('hrsM', 'hrsF', 'hrs')
#
hrsPop_sa <- cbind(absT19x$hrsM_sa / absT3x$popM, absT19x$hrsF_sa / absT3x$popF,
                   absT19x$hrs_sa / absT3x$pop)
names(hrsPop_sa) <- c('hrsM', 'hrsF', 'hrs')

hrsPop_t_melt <- melt(
                       data.frame(date = index(hrsPop_t),
                                  hrsPop_t), measure.vars = c(2:4))

hrsPop_sa_melt <- melt(
                       data.frame(date = index(hrsPop_sa),
                                  hrsPop_sa), measure.vars = c(2:4))

# hours per head of popn by male and female
gg_hrsPop_twin <- ggplot() +
            geom_line(data = subset(hrsPop_t_melt, variable %in% c('hrsM', 'hrsF', 'hrs')),
                      aes(x = date, y = value, fill = variable), size = 1.2) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(hrsPop_sa_melt, variable %in% c('hrsM', 'hrsF', 'hrs')),
                      aes(x = date, y = value, fill = variable), size = 0.4, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Ave hours worked per person (popn) /month -- by gender")
png(file.path(plotPATH, "hrsPop_twin.png"))
grid.arrange(gg_hrsPop_twin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# growth of hours and employment :: SA + T
growth6mAR_hrs = rollapplyr(diff(absT19x, log=TRUE), 6, colMeans, na.rm=TRUE)[-c(1:6),] * 1200

growth6mAR_hrsT <- growth6mAR_hrs[, c(1:9)]
names(growth6mAR_hrsT) <- names(absT1x)[1:9]
growth6mAR_hrsSA <- growth6mAR_hrs[, c(10:18)]
names(growth6mAR_hrsSA) <- names(absT1x)[1:9]
#

growth6mAR_nT = rollapplyr(diff(absT1x[, 1:9], log=TRUE), 6, colMeans, na.rm=TRUE)[-c(1:6),] * 1200
growth6mAR_nSA = rollapplyr(diff(absT2x[, 1:9], log=TRUE), 6, colMeans, na.rm=TRUE)[-c(1:6),] * 1200

growthMelt_6mAR_hrsT <- melt(data.frame(date = index(growth6mAR_hrs[xtsDateCut]),
                                      growth6mAR_hrsT[xtsDateCut,]), measure.vars = c(2:10))
growthMelt_6mAR_hrsSA <- melt(data.frame(date = index(growth6mAR_hrs[xtsDateCut]),
                                      growth6mAR_hrsSA[xtsDateCut,]), measure.vars = c(2:10))
growthMelt_6mAR_nT <- melt(data.frame(date = index(growth6mAR_nT[xtsDateCut]),
                                      growth6mAR_nT[xtsDateCut,]), measure.vars = c(2:10))
growthMelt_6mAR_nSA <- melt(data.frame(date = index(growth6mAR_nSA[xtsDateCut]),
                                      growth6mAR_nSA[xtsDateCut,]), measure.vars = c(2:10))

# growth of hours and employment SA ->>
gg_hrsN_SAtwin <- ggplot() +
            geom_line(data = subset(growthMelt_6mAR_nSA, variable %in% c('nM', 'nF', 'n')),
                      aes(x = date, y = value, fill = variable), size = 1.2, color = 'blue') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(growthMelt_6mAR_hrsSA, variable %in% c('nM', 'nF', 'n')),
                      aes(x = date, y = value, fill = variable), size = 0.7, color = 'orange') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "SA'd growth (6mAR) of Hrs (orange) and Employment (blue)")
png(file.path(plotPATH, "hrsN_SA6mAR.png"))
grid.arrange(gg_hrsN_SAtwin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# growth of hours and employment Trend ->>
gg_hrsN_Ttwin <- ggplot() +
            geom_line(data = subset(growthMelt_6mAR_nT, variable %in% c('nM', 'nF', 'n')),
                      aes(x = date, y = value, fill = variable), size = 1.2, color = 'blue') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(growthMelt_6mAR_hrsT, variable %in% c('nM', 'nF', 'n')),
                      aes(x = date, y = value, fill = variable), size = 0.7, color = 'orange') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Trend growth (6mAR) of Hrs (orange) and Employment (blue)")
png(file.path(plotPATH, "hrsN_T6mAR.png"))
grid.arrange(gg_hrsN_Ttwin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# matched v unmatched
matchy <- cbind(cbind(absT2x$n, absT3x$n)['19971001::'], gflowsXx$matchedN)
names(matchy) <- c('n_sa', 'n_raw', 'n_matched')
matchy$seasRatio <- log(matchy$n_sa) - log(matchy$n_raw)
matchy$n_matchSA <- (1 + matchy$seasRatio) * matchy$n_matched
matchy$n_UNmatchSA <- matchy$n_sa - matchy$n_matchSA
Dmatchy <- diff(matchy)

# prepare the SA'd data for plotting
lvlN <- melt(data.frame(date = index(absT2x[xtsDateCut]),
                        absT2x[xtsDateCut, c(3, 6, 9)]), measure.vars = c(2:4))
ratesLvl <- melt(data.frame(date = index(absT2x[xtsDateCut]),
                            absT2x[xtsDateCut, c(25:39)]), measure.vars = c(2:16))
ratesLvl_LT <- melt(data.frame(date = index(absT2x),
                            absT2x[, c(25:39)]), measure.vars = c(2:16))
diffN <- melt(data.frame(date = index(DabsT2x[xtsDateCut]),
                         coredata(DabsT2x[xtsDateCut,c(3, 6, 9)]))[-1,], measure.vars = c(2:4))

n_pop <- 100*cbind(absT2x$ft / absT3x$pop, absT2x$pt / absT3x$pop, absT2x$n / absT3x$pop)
Dn_pop <- diff(n_pop)
DlvlN_pop <- melt(data.frame(date = index(Dn_pop[xtsDateCut]),
                                  coredata(Dn_pop[xtsDateCut, ]))[-1,], measure.vars = c(2:4))

matchLvl <- melt(data.frame(date = index(matchy[xtsDateCut]),
                            matchy[xtsDateCut, c(1,5,6)]), measure.vars = c(2:4))

DmatchLvl <- melt(data.frame(date = index(Dmatchy[xtsDateCut]),
                            Dmatchy[xtsDateCut, c(1,5,6)]), measure.vars = c(2:4))

# level stack -- pt + ft
gp_ftpt <- ggplot(subset(lvlN, variable %in% c('ft', 'pt')),
                  aes(x = date, y = value, color = variable, fill = variable)) +
                theme_grey() +
                scale_color_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "employment by type ('000k)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'stack')
#
png(file.path(plotPATH, "ftpt_lvl.png"))
grid.arrange(gp_ftpt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# Matched v. unmatched -- level 'k + stacked
gp_matchN <- ggplot(subset(matchLvl, variable %in% c('n_matchSA', 'n_UNmatchSA')),
                  aes(x = date, y = value, fill = variable)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "Employment by type ('000k)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'stack')
#
png(file.path(plotPATH, "matchN.png"))
grid.arrange(gp_matchN, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# Matched v. unmatched -- diff 'k
gp_DmatchN <- ggplot(subset(DmatchLvl, variable %in% c('n_matchSA', 'n_UNmatchSA')),
                aes(x = date, y = value, fill = variable)) +
                facet_grid(variable ~ ., scale = 'free_y') +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                labs(title = "Employment by type ('000k)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "DmatchN.png"))
grid.arrange(gp_DmatchN, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# k-change over the prior date-subset years
gp_Dftpt <- ggplot(diffN,
                   aes(x = date, y = value, fill = variable)) +
                facet_grid(variable ~ ., scale = 'free_y') + theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                labs(title = "Change in Employment (k/m, by type)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'none') +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "Dftpt_lvl.png"))
grid.arrange(gp_Dftpt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# %-change over the prior date-subset years
gp_pcDftpt <- ggplot(DlvlN_pop,
                   aes(x = date, y = value, fill = variable)) +
                facet_grid(variable ~ ., scale = 'free_y') + theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                labs(title = "Change in Employment (%Popn, by type)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'none') +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "pcDftpt_lvl.png"))
grid.arrange(gp_pcDftpt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# unemployment rates for f/t (all on one chart)
png(file.path(plotPATH, "UR_x.png"))
    par( mar = c(6, 3, 4, 2))
    plot(absT2x$ur[xtsDateCut], main="AUD: Unemployment rate", las=2, major.format = "%b-%y",
         type = 'o', pch = 19)
    lines(SMA(absT2x$ur, 3), lwd=4, col=3)
    lines(absT2x$ur ,lwd = 2, type = 'o', pch=19)
    legend('topleft', c('Unemployment Rate', '3mma'), lwd = c (3, 3), pch = c(19, NA),
           col = c(1,3), bg = 'lightgrey')
    mtext('Source: ABS', side = 1, line = 5, adj = 0)
    mtext('www.ricardianambivalence.com', side = 1, line = 5, adj = 1)
dev.off()

# unemployment rates -- by gender XTSplot
png(file.path(plotPATH, "UR3mma_x.png"))
    par( mar = c(6, 3, 4, 2))
    plot(SMA(absT2x$ur[xtsDateCut], 3), main = "AUD unemployment rates (3mma)", las=2, major.format = "%b-%y",
         ylim = c(3.5, 6.5))
    lines(SMA(absT2x$urM, 3), lwd=2, col = 4)
    lines(SMA(absT2x$urF, 3), lwd=2, col = 6)
    lines(SMA(absT2x$ur, 3), lwd = 3, col = 1)
    legend('topleft', c('Unemployment Rate', 'Male UR%', 'Female UR%'), lwd = c (3, 2, 2),
           col = c(1, 4, 6), bg = 'lightgrey')
    mtext('Source: ABS', side = 1, line = 5, adj = 0)
    mtext('www.ricardianambivalence.com', side = 1, line = 5, adj = 1)
dev.off()

# URates %LF -- by genders
gp_UR <- ggplot(subset(ratesLvl, variable %in% c('urM', 'urF', 'ur')),
                  aes(x = date, y = value, fill = variable)) +
                  theme_grey() +
                  facet_grid(variable ~ ., scale = 'free_y') +
                  scale_fill_brewer(palette = 'Set1') +
                  labs(title = "Unemployment (% of Labour Force)", y = NULL, x = NULL) +
                  geom_line()
#
png(file.path(plotPATH, "UR.png"))
grid.arrange(gp_UR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# employment to population %rates -- by gender
gp_nPop <- ggplot(subset(ratesLvl, variable %in% c('nPop', 'nPopM', 'nPopF')),
                  aes(x = date, y = value, fill = variable)) +
                  theme_grey() +
                  facet_grid(variable ~ ., scale = 'free_y') +
                  scale_fill_brewer(palette = 'Set1') +
                  labs(title = "Employment (% of Popn)", y = NULL, x = NULL) +
                  geom_line()
#
png(file.path(plotPATH, "nPop.png"))
grid.arrange(gp_nPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# partRates %popn -- by gender
gp_PR <- ggplot(subset(ratesLvl_LT, variable %in% c('pr', 'prM', 'prF')),
                  aes(x = date, y = value, fill = variable)) +
                  theme_grey() +
                  facet_grid(variable ~ ., scale = 'free_y') +
                  scale_fill_brewer(palette = 'Set1') +
                  labs(title = "Participation (% of Popn)", y = NULL, x = NULL) +
                  geom_line()
#
png(file.path(plotPATH, "partRate.png"))
grid.arrange(gp_PR, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# unemployment-popn %rates by gender
gp_unPop <- ggplot(subset(ratesLvl_LT, variable %in% c('unPop', 'unPopM', 'unPopF')),
                  aes(x = date, y = value, fill = variable)) +
                  theme_grey() +
                  facet_grid(variable ~ ., scale = 'free_y') +
                  scale_fill_brewer(palette = 'Set1') +
                  labs(title = "Unemployment (% of Popn)", y = NULL, x = NULL) +
                  geom_line()
#
png(file.path(plotPATH, 'unPop.png'))
grid.arrange(gp_unPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# not-in-labour-force to population rates% by gender
gp_nilfPop <- ggplot(subset(ratesLvl_LT, variable %in% c('nilfPop', 'nilfPopM', 'nilfPopF')),
                  aes(x = date, y = value, fill = variable)) +
                  theme_grey() +
                  facet_grid(variable ~ ., scale = 'free_y') +
                  scale_fill_brewer(palette = 'Set1') +
                  labs(title = "Not in labour force (% of Popn)", y = NULL, x = NULL) +
                  geom_line()
#
png(file.path(plotPATH, 'nilfPop.png'))
grid.arrange(gp_nilfPop, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# all -- attachment N / UN / NILF
gp_attachment <- ggplot(subset(ratesLvl_LT, variable %in% c('nPop', 'unPop', 'nilfPop')),
                  aes(x = date, y = value, color = variable, fill = variable)) +
                theme_grey() +
                scale_color_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "All -- attachment of the labour force") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'fill')
#
png(file.path(plotPATH, "attach_all.png"))
grid.arrange(gp_attachment, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# male -- attachment N / UN / NILF
gp_attachmentM <- ggplot(subset(ratesLvl_LT, variable %in% c('nPopM', 'unPopM', 'nilfPopM')),
                  aes(x = date, y = value, color = variable, fill = variable)) +
                theme_grey() +
                scale_color_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "Male -- attachment of the labour force") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'fill')
#
png(file.path(plotPATH, "attach_M.png"))
grid.arrange(gp_attachmentM, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# female -- attachment N / UN / NILF
gp_attachmentF <- ggplot(subset(ratesLvl_LT, variable %in% c('nPopF', 'unPopF', 'nilfPopF')),
                  aes(x = date, y = value, color = variable, fill = variable)) +
                theme_grey() +
                scale_color_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "Female -- attachment of the labour force") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'fill')
#
png(file.path(plotPATH, "attach_F.png"))
grid.arrange(gp_attachmentF, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

## gross flows data

gflowsXx$n2n <- 1 - gflowsXx$n2un - gflowsXx$n2non
gflowsXx$un2un <- 1 - gflowsXx$un2n - gflowsXx$un2non
gflowsXx$non2non <- 1 - gflowsXx$non2n - gflowsXx$non2un
#
gfAdj = xts(sapply(gflowsXx,
                   FUN = function(X) baysea(X, trend.order=1, year=1997, month=10, plot=F)$adjust),
            order.by = index(gflowsXx))
#
gfTrend = xts(sapply(gflowsXx,
                   FUN = function(X) baysea(X, trend.order=1, year=1997, month=10, plot=F)$trend),
            order.by = index(gflowsXx))
#
gflowsMl_sa <- melt(data.frame(date = index(gfAdj),
                            gfAdj[, c(3:14)]), measure.vars = c(2:13))
#
gflowsMl_T <- melt(data.frame(date = index(gfTrend),
                            gfTrend[, c(3:14)]), measure.vars = c(2:13))

# employment ->>
gg_Ntwin <- ggplot() +
            geom_line(data = subset(gflowsMl_T, variable %in% c('n2un', 'n2non', 'n2n')),
                      aes(x = date, y = value, fill = variable), size = 1.5) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(gflowsMl_sa, variable %in% c('n2un', 'n2non', 'n2n')),
                      aes(x = date, y = value, fill = variable), size = 0.7, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Transition Probabilities: employed ->")
png(file.path(plotPATH, "N_flows.png"))
grid.arrange(gg_Ntwin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# unemployment ->>
gg_UNtwin <- ggplot() +
            geom_line(data = subset(gflowsMl_T, variable %in% c('un2un', 'un2non', 'un2n')),
                      aes(x = date, y = value, fill = variable), size = 1.5) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(gflowsMl_sa, variable %in% c('un2un', 'un2non', 'un2n')),
                      aes(x = date, y = value, fill = variable), size = 0.7, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Transition Probabilities: unemployed ->")
png(file.path(plotPATH, "UN_flows.png"))
grid.arrange(gg_UNtwin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# NILF ->>
gg_NONtwin <- ggplot() +
            geom_line(data = subset(gflowsMl_T, variable %in% c('non2un', 'non2non', 'non2n')),
                      aes(x = date, y = value, fill = variable), size = 1.5) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(gflowsMl_sa, variable %in% c('non2un', 'non2non', 'non2n')),
                      aes(x = date, y = value, fill = variable), size = 0.7, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Transition Probabilities: Not in labour force ->")
png(file.path(plotPATH, "NON_flows.png"))
grid.arrange(gg_NONtwin, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
