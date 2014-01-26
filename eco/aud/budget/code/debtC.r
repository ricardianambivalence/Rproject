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
projPATH <- file.path("~/R/aud/budget")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "Rpics")
# }}}


CGSpeak = c(268, 290, 330, 340, NA)
CGSyrEnd = c(252, 280, 300, 320, 330)
CGSmkt = c(293, 321, 345, 357, 370)

tt = c(as.Date('2013-06-30'), as.Date("2014-06-30"), as.Date("2015-06-30"),
       as.Date("2016-06-30"), as.Date("2017-06-30"))

debtc = data.frame(CGSpeak, CGSyrEnd, CGSmkt)
debtc_x = xts(debtc, order.by = tt)

gp_debtC <- ggplot(meltx(debtc_x[,1]),
                   aes(x = date, y = value)) +
                geom_bar(stat = 'identity')

png(file.path(plotPATH, "peakCGS.png"))
barplot(debtc_x[,1], ylim = c(0, 350), las=1,
        main = "Peak CGS subject to debt ceiling (AUDbn)")
abline(h = 300, col=2, lty=2, lwd=2)
mtext('Source: Aus. Treasury', side = 1, line = 4, adj = 0)
mtext('www.ricardianambivalence.com', side = 1, line = 4, adj = 1)
dev.off()

png(file.path(plotPATH, "yrEndCGS.png"))
barplot(debtc_x[,2], ylim = c(0, 350), las=1,
        main = "CGS subject to debt ceiling at year end (AUDbn)")
abline(h = 300, col=2, lty=2, lwd=2)
mtext('Source: Aus. Treasury', side = 1, line = 4, adj = 0)
mtext('www.ricardianambivalence.com', side = 1, line = 4, adj = 1)
dev.off()

png(file.path(plotPATH, "mktCGS.png"))
barplot(debtc_x[,3], ylim = c(0, 350), las=1,
        main = "Market value of debt (AUDbn)")
abline(h = 300, col=2, lty=2, lwd=2)
mtext('Source: Aus. Treasury', side = 1, line = 4, adj = 0)
mtext('www.ricardianambivalence.com', side = 1, line = 4, adj = 1)
dev.off()
