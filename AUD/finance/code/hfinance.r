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
# }}} close setup

# {{{ PATHstuff
projPATH <- file.path("~/R/aud/finance")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}} close paths

getABS <- FALSE

if(getABS) {
    hFinT1 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&560901.xls&5609.0&Time%20Series%20Spreadsheet&4A9ED11D7D831626CA257B8300124872&0&Apr%202013&11.06.2013&Latest"
    hFinOO<- readABS(hFinT1)
    names(hFinOO) <- c(
                        'constNum_nsa', 'constVal_nsa', 'purchNewNum_nsa', 'purchNewVal_nsa',
                        'purchEstNum_nsa', 'purchEstVal_nsa', 'totalNum_nsa', 'totalVal_nsa',
                        'estRefiNum_nsa', 'estRefiVal_nsa', 'totalExRefiNum_nsa', 'totalExRefiVal_nsa',
                        'constNum_sa', 'constVal_sa', 'purchNewNum_sa', 'purchNewVal_sa',
                        'purchEstNum_sa', 'purchEstVal_sa', 'totalNum_sa', 'totalVal_sa',
                        'estRefiNum_sa', 'estRefiVal_sa', 'totalExRefiNum_sa', 'totalExRefiVal_sa',
                        'constNum_t', 'constVal_t', 'purchNewNum_t', 'purchNewVal_t',
                        'purchEstNum_t', 'purchEstVal_t', 'totalNum_t', 'totalVal_t',
                        'estRefiNum_t', 'estRefiVal_t', 'totalExRefiNum_t', 'totalExRefiVal_t'
                      )

    hFinT11 <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5609011.xls&5609.0&Time%20Series%20Spreadsheet&5E25C00FB32B2A05CA257B83001251C2&0&Apr%202013&11.06.2013&Latest"
    hFinPurp <- readABS(hFinT11)
    names(hFinPurp) <- c(
                        'oOcc_Const_nsa', 'oOcc_Purch_nsa', 'oOcc_refi_nsa', 'oOcc_purch_nsa',
                        'Invmt_const_nsa', 'Invmt_IndPurch_nsa', 'Invmt_OthPurch_nsa', 'total_nsa',
                        'oOcc_Const_sa', 'oOcc_Purch_sa', 'oOcc_refi_sa', 'oOcc_purch_sa',
                        'Invmt_const_sa', 'Invmt_IndPurch_sa', 'Invmt_OthPurch_sa', 'total_sa',
                        'oOcc_Const_t', 'oOcc_Purch_t', 'oOcc_refi_t', 'oOcc_purch_t',
                        'Invmt_const_t', 'Invmt_IndPurch_t', 'Invmt_OthPurch_t', 'total_t'
                      )

    save(hFinOO, hFinPurp, file = file.path(dataPATH, "hfinTables.rdata"))
} else {
    load(file = file.path(dataPATH, "hfinTables.rdata"))
}

hFinPurp$ttlInvmnt_sa <- hFinPurp$Invmt_const_sa + hFinPurp$Invmt_IndPurch_sa + hFinPurp$Invmt_OthPurch_sa

ratesPeaks <- c(as.Date("1994-12-01"), as.Date("2000-08-01"), as.Date("2008-03-05"),
                   as.Date("2010-11-03"))

splitLength <- nrow(hFinOO[paste0(ratesPeaks[4], "::")])

cycle1 <- hFinOO[paste0(ratesPeaks[1], "::")][1:splitLength]
cycle2 <- hFinOO[paste0(ratesPeaks[2], "::")][1:splitLength]
cycle3 <- hFinOO[paste0(ratesPeaks[3], "::")][1:splitLength]
cycle4 <- hFinOO[paste0(ratesPeaks[4], "::")][1:splitLength]

c1S <- dfxColScl(cycle1)
c2S <- dfxColScl(cycle2)
c3S <- dfxColScl(cycle3)
c4S <- dfxColScl(cycle4)

PurpCycle1 <- hFinPurp[paste0(ratesPeaks[1], "::")][1:splitLength]
PurpCycle2 <- hFinPurp[paste0(ratesPeaks[2], "::")][1:splitLength]
PurpCycle3 <- hFinPurp[paste0(ratesPeaks[3], "::")][1:splitLength]
PurpCycle4 <- hFinPurp[paste0(ratesPeaks[4], "::")][1:splitLength]
#
pc1S <- dfxColScl(PurpCycle1)
pc2S <- dfxColScl(PurpCycle2)
pc3S <- dfxColScl(PurpCycle3)
pc4S <- dfxColScl(PurpCycle4)

totalExRefiCycles <- data.frame('p94' = as.numeric(c1S[,23]), 'p00' = as.numeric(c2S[,23]),
                          'p08' = as.numeric(c3S[,23]), 'p10' = as.numeric(c4S[,23]),
                          'idx' = 0:(splitLength-1)
                          )

constCycles <- data.frame('p94' = as.numeric(c1S[,14]), 'p00' = as.numeric(c2S[,14]),
                          'p08' = as.numeric(c3S[,14]), 'p10' = as.numeric(c4S[,14]),
                          'idx' = 0:(splitLength-1)
                          )

prchNwCycles <- data.frame('p94' = as.numeric(c1S[,15]), 'p00' = as.numeric(c2S[,15]),
                          'p08' = as.numeric(c3S[,15]), 'p10' = as.numeric(c4S[,15]),
                          'idx' = 0:(splitLength-1)
                          )

InvmtCycles <- data.frame('p94' = as.numeric(pc1S[,25]), 'p00' = as.numeric(pc2S[,25]),
                          'p08' = as.numeric(pc3S[,25]), 'p10' = as.numeric(pc4S[,25]),
                          'idx' = 0:(splitLength-1)
                          )

allValueCycles <- data.frame('p94' = as.numeric(pc1S[,16]), 'p00' = as.numeric(pc2S[,16]),
                          'p08' = as.numeric(pc3S[,16]), 'p10' = as.numeric(pc4S[,16]),
                          'idx' = 0:(splitLength-1)
                          )

gp_allValueCycles <- ggplot(melt(allValueCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'months from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Housing Finance -- total value (OwnOcc + Investor) (SA)") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 30, 3)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "hfallValue.png"))
grid.arrange(gp_allValueCycles, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_totalExRefiCycles <- ggplot(melt(totalExRefiCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'months from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Housing Finance -- ex Refi (Number/m, SA)") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 30, 3)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "hfExRefi.png"))
grid.arrange(gp_totalExRefiCycles, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_ConstCycles <- ggplot(melt(constCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'months from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Housing Finance -- Construction (Number/m, SA)") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 30, 3)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "hfConst.png"))
grid.arrange(gp_ConstCycles, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_purchNwCycles <- ggplot(melt(prchNwCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'months from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Housing Finance -- purchase of new (Number/m, SA)") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 30, 3)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "hfpurchNew.png"))
grid.arrange(gp_purchNwCycles, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_InvmtCycles <- ggplot(melt(InvmtCycles, id.var = 5),
                          aes(x = idx, y = value, color = variable)) +
                        labs(y = 'Idx = 100 @ RBA peak', x = 'months from rates peak') +
                        scale_color_brewer(palette = 'Set1') +
                        labs(title = "Housing Finance -- Investment (value/m, SA)") +
                        theme(legend.position = 'top') +
                        theme(legend.title = element_blank()) +
                        scale_x_continuous(breaks=seq(0, 30, 3)) +
                        geom_line(size = 1.5)
png(file.path(plotPATH, "hfInvmnt.png"))
grid.arrange(gp_InvmtCycles, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
