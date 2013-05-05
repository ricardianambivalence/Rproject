require(xts)
require(gdata)
require(quantmod)
require(timsac)

rm(list=ls())
Sys.setenv(TZ='GMT')

mj_lastof <- function(tt){
    ot <- as.POSIXlt(tt)
    ot$mon <- ot$mon + 1
    ot$mday <- 1
    ot <- ot - (1*60*60*24)
    return(as.POSIXct(ot))
}

# get AUDUSD FX data and transform to month ave
dataNames <- c('DEXUSAL')
getSymbols(dataNames,src='FRED', return.class = 'xts')

AUDm <- apply.monthly(DEXUSAL, mean)
index(AUDm) <- mj_lastof(index(AUDm))

# RBA commodity price data
RBA_CPIx_xls <- "http://www.rba.gov.au/statistics/tables/xls/g05hist.xls"
R_CPIx_df <- read.xls(RBA_CPIx_xls, skip=6, as.is=TRUE)

R_CPIx_df <- R_CPIx_df[complete.cases(R_CPIx_df),]
names(R_CPIx_df) <- c("period", "all$A", "allSDR", "all$US", "rural$A", "ruralSDR", "rural$US", "nRural$A",
                      "nRuralSDR", "nRural$US", "base$A", "baseSDR", "base$US")

tt <- as.Date(paste("01-", R_CPIx_df[,1], sep = ""), format = "%d-%b-%Y")
ttl <- mj_lastof(tt)
R_CPIx_df_x <- xts(R_CPIx_df[,-1], order.by=ttl)

# vacancy data
deewr_ivi_xls  <- "/Users/mcooganj/Downloads/IVI_DATA.xls"

vac_df <- read.xls(deewr_ivi_xls, sheet=2, as.is=TRUE)

ttv <- mj_lastof(as.Date(paste("01-", colnames(vac_df)[-c(1:4)], sep = ""), format = "%d-%b.%y"))
Idx_nsa <- xts(t(vac_df[1,-c(1:4)]), order.by=ttv)
SA_vac <- baysea(log(coredata(Idx_nsa)), period=12, year=2006, month=1, plot=FALSE)
Idx_sa <- xts(exp(SA_vac$adjust), order.by=ttv)

ACVx <- cbind(R_CPIx_df_x[,c(3,6,9,12)], AUDm, Idx_sa)
colnames(ACVx) <- c('allUSD', 'ruralUSD', 'nonRuralUSD', 'baseUSD', 'AUDUSD', 'deewrSVI')

png("~/RA/pics/ACV/CAV.png")
plot(as.zoo(ACVx['2006::', c(1,5,6)]), col=1:3,
     main="Commodity Export Prices, AUDUSD and Vacancies", las=1)
dev.off()

png("~/RA/pics/ACV/dCAV.png")
plot(as.zoo(diff(ACVx['2006::', c(1,5,6)])), col=1:3,
     main="Commodity Export Prices, AUDUSD and Vacancies", las=1)
dev.off()

png("~/RA/pics/ACV/CnV.png")
pairs(coredata(ACVx['2006::', c(1,6)], main='Commodity prices and Job Vacancies'))
dev.off()

png("~/RA/pics/ACV/dCnV.png")
pairs(coredata(diff(ACVx['2006::', c(1,6)]), main='Commodity prices and Job Vacancies'))
dev.off()

fx_m <- lm(log(AUDUSD) ~ log(allUSD), data=ACVx['2006::'])
fx_resid <- xts(fx_m$residuals, order.by=index(ACVx['2006::']))
ACVx$fxResid <- fx_resid

png("~/RA/pics/ACV/fxMod.png")
plot(ACVx$AUDUSD['2006::'], las=1, main="AUDUSD -- Actual v. Commod Price Implied")
lines(exp(fx_m$fitted.values) ~ index(ACVx['2006::']), las=1, col=2, lwd=2)
dev.off()

png("~/RA/pics/ACV/fxResid.png")
barplot(fx_resid*100, las=1, main="% over- or under- valuation of AUDUSD")
dev.off()

svi <- ACVx$deewrSVI['20060101/20120930']
sviQ <- apply.quarterly(svi, mean)
lv <- last(svi)
index(lv) <- as.Date("2012-12-31")
sviQ <- rbind(sviQ, lv)
d_sviQ <- diff(sviQ)

fxResid <- ACVx$fxResid['20060101/20121031']
fxResidQ <- apply.quarterly(fxResid, mean)
index(fxResidQ)[length(fxResidQ)] <- as.Date("2012-12-31")
d_fxResidQ <- diff(fxResidQ)

AUDUSD <- ACVx$AUDUSD['20060101/20121031']
AUDUSDQ <- apply.quarterly(AUDUSD, mean)
index(AUDUSDQ)[length(AUDUSDQ)] <- as.Date("2012-12-31")
d_AUDUSDQ <- diff(AUDUSDQ)

allUSD <- ACVx$allUSD['20060101/20121031']
allUSD <- apply.quarterly(allUSD, mean)
index(allUSD)[length(allUSD)] <- as.Date("2012-12-31")
d_allUSDQ <- diff(allUSD)

ccf(as.ts(d_sviQ), as.ts(d_fxResidQ), na.action=na.pass)

fx2vac <- cbind(d_sviQ, d_fxResidQ, d_AUDUSDQ, d_allUSDQ)
colnames(fx2vac)  <- c('d_sviQ', 'd_fxResidQ', 'd_AUDUSD', 'd_allUSDQ')
fx2vac$L.d_sviQ <- lag(d_sviQ)
fx2vac$L.d_fxResidQ <- lag(d_fxResidQ)
fx2vac$L.d_AUDUSDQ <- lag(d_AUDUSDQ)
fx2vac$L.d_allUSDQ <- lag(d_fxResidQ)

png("~/RA/pics/ACV/ccf_fxpremVac.png")
ccf(as.ts(svi), as.ts(fxResid), main="Correlation between Vacancies and FX-premium")
dev.off()

tts <- index(svi)
png("~/RA/pics/ACV/yyploy_fxpremVac.png")
par(mar=c(5,4,4,5) + 0.1)
plot(coredata(ACVx$deewrSVI['20060101/20120930'])~ tts, las=1, type='l',
     xlab="", ylab="Job Vacancies", col=2)
par(new=TRUE)
plot(coredata(ACVx$fxResid['20060101/20120930']) ~ tts, type='l', xaxt='n',
     yaxt='n', xlab='', ylab='', col=3)
axis(4)
mtext('fxResiduals', side=4, line=3)
legend('bottomleft', col=c(2,3), lty=1, legend = c('vacancies', 'fxResiduals'))
dev.off()

