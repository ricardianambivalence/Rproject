require(quantmod)

fredNames <- c('SP500', 'DGS5', 'DGS10', 'DGS30','DFII5', 'DFII10', 'DFII30',
               'DFII30', 'BAMLH0A3HYC', 'MORTGAGE30US', 'DCPF3M', 'USD3MTD156N', 'WSWP10',
               'VIXCLS')
oandaNames <- c('EUR/USD', 'JPY/USD', 'EUR/CHF', 'AUD/USD', 'CLP/USD')
metalNames <- c('gold', 'plat', 'silver')

getSymbols(fredNames,src='FRED', return.class = 'xts')
getSymbols(oandaNames, src='oanda', from="2012-01-01")
getMetals(metalNames, src='oanda', from="2012-01-01")

SP500.dod <- 100*diff(SP500, lag=1, log=TRUE)
png("~/RA/pics/fins/SP500_dod.png")
barplot(SP500.dod['2012::'], col=7, las=1, main="S&P 500",
        names.arg=format(index(SP500.dod['2012::']), "%b-%y"))
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/EURUSD.png")
plot(EURUSD['2012::'], las=1, main="USDs per Euro (EURUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/USDJPY.png")
plot(JPYUSD['2012::'], las=1, main="USDs per Japanese Yen (JPYUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/AUDUSD.png")
plot(AUDUSD['2012::'], las=1, main="USDs per Australian Dollar (AUDUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/CLPUSD.png")
plot(CLPUSD['2012::'], las=1, main="USDs per Chilean Peso (CLPUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/XAUUSD.png")
plot(XAUUSD['2012::'], las=1, main="USDs per Ounce of Gold (XAUUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/XPTUSD.png")
plot(XPTUSD['2012::'], las=1, main="USDs per Ounce of Platinum (XPTUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

png("~/RA/pics/fins/XAGUSD.png")
plot(XAGUSD['2012::'], las=1, main="USDs per Ounce of Silver (XAGUSD)", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: Oanda', side=1, line=4, adj=0)
dev.off()

