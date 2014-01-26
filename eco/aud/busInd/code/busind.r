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
corpGOP_3split <- corpGOP_3split[, c(2, 1, 3, 4)]

bizGOP_3split <- bizGOP[, c('mining_sa', 'total_sa')]
bizGOP_3split$tradable_notMining <- rowSums(bizGOP[,
                                            c('manu_sa', 'whole_sa', 'retail_sa',
                                              'accomFood_sa', 'transport_sa')],
                                            na.rm=TRUE)
bizGOP_3split$nonTradable <- bizGOP$total_sa - bizGOP$mining_sa - bizGOP_3split$tradable_notMining
bizGOP_3split <- bizGOP_3split[, c(2, 1, 3, 4)]

ratesPeaks <- c(as.Date("1994-12-01"), as.Date("2000-08-1"), as.Date("2008-03-05"),
                   as.Date("2010-11-03"))

splitLength <- nrow(corpGOP_3split[paste0(ratesPeaks[4], "::")])

cycle1 <- corpGOP_3split[paste0(ratesPeaks[1], "::")][1:splitLength]
cycle2 <- corpGOP_3split[paste0(ratesPeaks[2], "::")][1:splitLength]
cycle3 <- corpGOP_3split[paste0(ratesPeaks[3], "::")][1:splitLength]
cycle4 <- corpGOP_3split[paste0(ratesPeaks[4], "::")][1:splitLength]

c1S <- dfxColScl(cycle1)
c2S <- dfxColScl(cycle2)
c3S <- dfxColScl(cycle3)
c4S <- dfxColScl(cycle4)

totalCycles <- data.frame('p94' = as.numeric(c1S[,1]), 'p00' = as.numeric(c2S[,1]),
                          'p08' = as.numeric(c3S[,1]), 'p10' = as.numeric(c4S[,1]),
                          'idx' = 0:(splitLength-1)
                          )

miningCycles <- data.frame('p94' = as.numeric(c1S[,2]), 'p00' = as.numeric(c2S[,2]),
                          'p08' = as.numeric(c3S[,2]), 'p10' = as.numeric(c4S[,2]),
                          'idx' = 0:(splitLength-1)
                          )

tradNonMinCycles <- data.frame('p94' = as.numeric(c1S[,3]), 'p00' = as.numeric(c2S[,3]),
                          'p08' = as.numeric(c3S[,3]), 'p10' = as.numeric(c4S[,3]),
                          'idx' = 0:(splitLength-1)
                          )

nonTradCycles <- data.frame('p94' = as.numeric(c1S[,4]), 'p00' = as.numeric(c2S[,4]),
                          'p08' = as.numeric(c3S[,4]), 'p10' = as.numeric(c4S[,4]),
                          'idx' = 0:(splitLength-1)
                          )

gp_totalCorpGOP <- ggplot(melt(totalCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'qtrs from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Corporate Gross Operating Profits: total") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 10, 2)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "cGOP.png"))
grid.arrange(gp_totalCorpGOP, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_miningCorpGOP <- ggplot(melt(miningCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'qtrs from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Corporate Gross Operating Profits: mining") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 10, 2)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "mining_cGOP.png"))
grid.arrange(gp_miningCorpGOP, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_tradNonMinCorpGOP <- ggplot(melt(tradNonMinCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'qtrs from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Corporate Gross Operating Profits: tradNonMin") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 10, 2)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "tradNonMin_cGOP.png"))
grid.arrange(gp_tradNonMinCorpGOP, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_nonTradCorpGOP <- ggplot(melt(nonTradCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'qtrs from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Corporate Gross Operating Profits: nonTrad") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 10, 2)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "nonTrad_cGOP.png"))
grid.arrange(gp_nonTradCorpGOP, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
