# set up environment
rm(list=ls()); gc()
Sys.setenv(TZ = 'UTC')

# packages and functions
require(TTR)
require(quantmod)
require(xts)
require(gdata)
source('~/Rproject/Rhelpers/helperFuncts.r')

# set paths
mainPath <- '~/Rproject/usd/pce'
plotPath <- file.path(mainPath, 'pics')

# set RAstamps
RAstamp <- paste("ricardianambivalence.com  ", format(Sys.time() + 60*60*24, "%d-%b-%Y"))

# get trimmed mean PCE data
TMpceURL <- "http://www.dallasfed.org/assets/documents/research/pce/pce_hist.xls"
TMpce_df <- read.xls(TMpceURL, skip=2, as.is=TRUE)
names(TMpce_df) <- c('data', 'AR1m', 'AR6m', 'AR12m')

options(warn=-1)
TMpce_df[,(2:ncol(TMpce_df))] <- sapply(TMpce_df[,(2:(ncol(TMpce_df)))], as.numeric)
options(warn=0)

tt <- as.Date(paste("01-", TMpce_df[,1], sep = ""), format = "%d-%b-%y")
xmd <- xts(TMpce_df[,-c(1)], order.by=tt)
xmd$AR3m <- SMA(xmd$AR1m, n=3)

png(file.path(plotPath, 'UStmPCE.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(coredata(xmd[,3]['2008::'])~index(xmd['2008::']), type='l', lwd=3, col=1, las=1, xlab="", ylab="%YoY",
     main="US Trimmed Mean PCE", ylim=c(0,4))
lines(coredata(xmd[,2]['2008::'])~index(xmd['2008::']), lwd=2, col=2)
lines(coredata(xmd[,4]['2008::'])~index(xmd['2008::']), lwd=1, col=3)
lines(coredata(xmd[,1]['2008::'])~index(xmd['2008::']), lwd=1, lty=2, col=1)
abline(h = seq(0, 3.5, 0.5), col=8, lty=2)
legend('top', legend=c('YoY', '6mma AR', '3mma AR', '1m AR'), lwd=c(3,2,1,1), col=c(1,2,3,1),
       lty = c(1, 1, 1, 2), bg='lightgrey', horiz=TRUE)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: Dallas Fed", cex=0.75, side=1, adj=0, outer=T)
dev.off()

# get data from FRED

# corePCE, realPCap, nomPcap, realXtrn, realPersCons, realPCE_GDP, realGDP
fredSymbols <- c('PCEPILFE', 'A229RX0', 'A229RC0', 'W875RX1', 'PCEC96', 'PCECC96', 'GDPC1')
getSymbols(fredSymbols, src = 'FRED')

## adv months for the qtrly data (to end of qtr)
index(PCECC96) <- dateSwitch(index(PCECC96), lastDay = FALSE, adv=1)
index(GDPC1) <- dateSwitch(index(GDPC1), lastDay = FALSE, adv=1)

PCEdefl <- 100*((1 + diff(PCEPILFE, log=T))^12 - 1)
names(PCEdefl) <- 'AR1m'
PCEdefl$AR3m <- SMA(PCEdefl$AR1m, n=3)
PCEdefl$AR6m <- SMA(PCEdefl$AR1m, n=6)
PCEdefl$AR12m <- SMA(PCEdefl$AR1m, n=12)

png(file.path(plotPath, 'USxffPCE.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(coredata(PCEdefl[,4]['2008::'])~index(PCEdefl['2008::']), type='l', lwd=3, col=1, las=1, xlab="", ylab="%YoY",
     main="US PCE: excl. Food + Fuel", ylim=c(0,4))
lines(coredata(PCEdefl[,3]['2008::'])~index(PCEdefl['2008::']), lwd=2, col=2)
lines(coredata(PCEdefl[,2]['2008::'])~index(PCEdefl['2008::']), lwd=1, col=3)
lines(coredata(PCEdefl[,1]['2008::'])~index(PCEdefl['2008::']), lwd=1, lty=2, col=1)
abline(h = seq(0, 3.5, 0.5), col=8, lty=2)
legend('top', legend=c('YoY', '6mma AR', '3mma AR', '1m AR'), lwd=c(3,2,1,1), col=c(1,2,3,1),
       lty = c(1, 1, 1, 2), bg='lightgrey', horiz=TRUE)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

# income plots
png(file.path(plotPath, 'USrealPerCapInc.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(A229RX0['2001::'], las=1, main="Disposable Personal Income ($real, per-capita)", log='y')
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

png(file.path(plotPath, 'USnominalPerCapInc.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(A229RC0['2001::'], las=1, main="Disposable Personal Income ($nominal, per-capita)", log='y')
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

png(file.path(plotPath, 'USIncXTranfers.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(W875RX1['2001::'], las=1, main="Real Personal Income ex transfers (USDbn)", log='y')
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

png(file.path(plotPath, 'USIncXTranfers_PPtPeak.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(100*(log(W875RX1) - log(cummax(W875RX1))), las=1, main="Real Personal Income ex transfers (%of Peak)")
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

png(file.path(plotPath, 'USrealPerCapInc_PPtPeak.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(100*(log(A229RX0) - log(cummax(A229RX0))), las=1,
     main="Real Per-capita Disposable Income (%of Peak)")
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

# Income -- trend line
preGFCIncTrend <- lm(log(A229RX0['1970::2008']) ~ index(A229RX0['1970::2008']))

png(file.path(plotPath, 'realInc_vTrend.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(log(coredata(A229RX0['1970::'])) ~ index(A229RX0['1970::']), type = 'l', las=1,
     ylab='log USDbn', xlab='', main="(log) Real Per-capita Disposable income")
abline(preGFCIncTrend, col=2, lwd=2, lty=2)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

# consumption stuff -- PCEC96

## Cons v trend
preGFCConsTrend <- lm(log(PCEC96['1995::2008']) ~ index(PCEC96['1995::2008']))

png(file.path(plotPath, 'realPCE_vTrend.png'))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(log(coredata(PCEC96['1995::'])) ~ index(PCEC96['1995::']),
     type = 'l', las=1, col = 4,
     ylab='log USDbn', xlab='', main="(log) real personal consumption expenditure")
abline(preGFCConsTrend, col=2, lwd=2, lty=2)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

## PCE v. GDP-PCE

png(file.path(plotPath, 'realCons.png'))
par(mar=c(5.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(PCEC96['2004::'], las=2, major.format="%b-%y", log='y',
     main = "Real PCE & GDP-Consumption", ylab = "real USDbn (2005$)")
lines(PCECC96['2004::'], col=6, lwd=4)
lines(PCEC96['2004::'], col=4, type='o', pch=5, cex = 1.3)
legend('topleft', legend=c('Real PCE (monthly)', 'GDP-PCE (qtrly)'), lwd=c(1,4),
       col=c(4,6), pch = c(5, NA), bg='lightgrey', horiz=TRUE)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

## PCE v. GDP
midQtrs <- which(as.POSIXlt(index(PCEC96))$mon %in% c(1, 4, 7, 10))
PCEC96_q <- PCEC96[midQtrs,]
Cons_n_GDP <- merge(PCEC96_q, GDPC1, join = 'left')
Cons_n_GDP_scaled <- scale(log(Cons_n_GDP))
Cons_n_GDP_D <- 100*diff(Cons_n_GDP, log=TRUE)

# levels -- PCE and GDP
png(file.path(plotPath, 'PCE_n_GDP_lvl.png'))
par(mar=c(5.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot(Cons_n_GDP_scaled['2003::', 1], las=2, major.format="%b-%y",
     main = "Real PCE & GDP", ylab = "scaled log $2005 (mean = 0)")
lines(Cons_n_GDP_scaled['2003::', 2], col='orange', lwd=4)
lines(Cons_n_GDP_scaled['2003::', 1], col=4, type='o', pch=5)
legend('topleft', legend=c('Real PCE', 'GDP'), lwd=c(1,4), col=c(4, 'orange'),
       pch = c(5, NA), bg='lightgrey', horiz=TRUE)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()

# growth -- PCE and GDP
png(file.path(plotPath, 'PCE_n_GDP_growth.png'))
par(mar=c(5.5, 4, 2, 1), oma=c(1,0,0,0))
plot(Cons_n_GDP_D['2003::', 1], las=2, major.format="%b-%y", ylim = c(-2.5, 2.5),
     main = "Growth of real PCE & GDP", ylab = "%QoQ")
lines(Cons_n_GDP_D['2003::', 2], col='orange', lwd=4)
lines(Cons_n_GDP_D['2003::', 1], col=4, type='o', pch=5)
legend('topleft', legend=c('Real PCE', 'GDP'), lwd=c(1,4), col=c(4, 'orange'),
       pch = c(5, NA), bg='lightgrey', horiz=TRUE)
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()
