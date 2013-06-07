rm(list = ls()); gc()
Sys.setenv(TZ = 'UTC')

# packages and functions
require(gridExtra)
require(quantmod)
require(lattice)

## PATH stuff
workingPATH <- "~/R/usd/nfp"
plotPATH <- file.path(workingPATH, "Rpics")

# get data from FRED
dataNames <- c('PAYEMS', 'UNRATE', 'EMRATIO', 'AHETPI','UEMPMED', 'AWHI', 'AWHMAN',
               'USCONS', 'MANEMP', 'TEMPHELPS', 'USTRADE', 'CES0500000003', 'PCEPILFE',
               'CNP16OV', 'CE16OV', 'CIVPART')
getSymbols(dataNames,src='FRED', return.class = 'xts')

NFP_m <- diff(PAYEMS)
NFP_m_3mAve <- SMA(NFP_m, 3)
NFP_m_3mAve_L3 <- lag(NFP_m_3mAve, 3)

HHjobs_m <- diff(CE16OV)
HHjobs_m_3mAve <- SMA(HHjobs_m, 3)
HHjobs_m_3mAve_L3 <- lag(HHjobs_m_3mAve, 3)

xy3mNFPTitle <- paste0("NFP Survey: 3mma = ", round(last(NFP_m_3mAve), 0), "k jobs/ month")
L_3mNFPJobsXY <- xyplot(NFP_m_3mAve['1990::'] ~ NFP_m_3mAve_L3['1990::'],
       panel = function(x, y)
       {
           panel.xyplot(x,y)
           panel.points(last(x), last(y), pch=18, col=2, cex=1.25)
           panel.abline(lm(y ~ x), lty=2, col='orange', lwd=2)
           panel.abline(c(0, 1), lty=3, col = 1)
       },
       xlab = "3m LAG -- 3mma NFP Jobs ('k/mon)",
       ylab =  "3mma NFP Jobs ('k/mon)",
       main = xy3mNFPTitle
       )

xy3mEstHHsTitle <- paste0("NFP Survey v. HH Survey: 3mma k jobs/ month")
L_3mEstHHsJobsXY <- xyplot(HHjobs_m_3mAve['1990::'] ~ NFP_m_3mAve['1990::'],
       panel = function(x, y)
       {
           panel.xyplot(x,y)
           panel.points(last(x), last(y), pch=18, col=2, cex=1.25)
           panel.abline(lm(y ~ x), lty=2, col='orange', lwd=2)
           panel.abline(c(0, 1), lty=3, col = 1)
       },
       xlab = "3mma NFP Jobs ('k/mon)",
       ylab =  "3mma HH Survey Jobs ('k/mon)",
       main = xy3mEstHHsTitle
       )

mtex <- textGrob("Source: FRED                                     www.ricardianambivalence.com")

png(file.path(plotPATH, "NFP_3mNFPXY.png"))
grid.arrange(L_3mNFPJobsXY, sub=mtex)
dev.off()

png(file.path(plotPATH, "NFP_3mEstHHsXY.png"))
grid.arrange(L_3mEstHHsJobsXY, sub=mtex)
dev.off()

png(file.path(plotPATH, "HHjobs_MoMk.png"))
bpC <- barplot(HHjobs_m['2007::'], col=7, las=1,
               main="MoM change in household survey employment(k) + 3mma",
        names.arg=format(index(HHjobs_m['2007::']), "%b-%y"))
lines(bpC, coredata(HHjobs_m_3mAve['2007::']), col=4, lwd=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_MoMk.png"))
bpE <- barplot(NFP_m['2007::'], col=7, las=1,
               main="MoM change in non-farm payrolls(k) + 3mma",
        names.arg=format(index(NFP_m['2007::']), "%b-%y"))
lines(bpE, coredata(NFP_m_3mAve['2007::']), col=4, lwd=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_UR.png"))
plot(UNRATE['2007::'], las=1, main="US Unemployment rate", type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_EPlong.png"))
plot(EMRATIO, main="US Employment to Population Ratio", las=1)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_EPshort.png"))
plot(EMRATIO['2007::'], main="US Employment to Population Ratio", las=1, type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

AveEarn_yoy <- 100*diff(AHETPI, lag=12, arithmetic=FALSE, log=TRUE)

png(file.path(plotPATH, "NFP_AHEyLong.png"))
plot(AveEarn_yoy, main="Ave Hourly Earnings: YoY", las=1, pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_AHEyShort.png"))
plot(AveEarn_yoy['2007::'], main="Ave Hourly Earnings: YoY", las=1, type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

CorePCE_yoy <- 100*diff(PCEPILFE, lag=12, arithmetic=FALSE, log=TRUE)
mm <- cbind(AveEarn_yoy, CorePCE_yoy)

mykey <- list(space = 'inside',
              column = 1,
              text = list(c('Annual Wage Inflation', 'Annual Core PCE Inflation'), col = c(2,4))
              # points = list(pch = 0, col = c(2,4))
             )
dxPlot <- xyplot(coredata(mm$AHETPI) + coredata(mm$PCEPILFE) ~index(mm), type='l', lwd=c(3,2),
                 col=c(2,4), xlab="", ylab="", main="Wage Inflation & Core PCE",
                 key= mykey)
png(file.path(plotPATH, "NFP_WageCore.png"))
grid.arrange(dxPlot, sub=mtex)
dev.off()

png(file.path(plotPATH, "NFP_medDurUnLong.png"))
plot(UEMPMED, main="Median duration of Unemployment (weeks)", las=1, pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_medDurUnShort.png"))
plot(UEMPMED['2007::'], main="Median duration of Unemployment (weeks)", las=1, type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_partRateLong.png"))
plot(CIVPART, main="Civilian Labor Force Participation Rate (weeks)", las=1, pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_partRateShort.png"))
plot(CIVPART['2007::'], main="Civilian Labor Force Participation Rate (weeks)", las=1, type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_AggHrsShort.png"))
plot(AWHI['2000::'], main="Aggregate Hours Worked (index)", las=1, type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

sDF <- data.frame(coredata(AWHI['::2000']), tt=as.numeric(index(AWHI['::2000'])))
AWHI_TrendMod <- lm(log(AWHI)~tt, data=sDF)

png(file.path(plotPATH, "NFP_AggHrsVtrend.png"))
plot(log(coredata(AWHI))~index(AWHI), las=1, ylim=c(3.8, 5), type='l', lwd=2,
     xlab="", ylab="log hrs", main="US Agg. Hours Worked Index")
abline(coef(AWHI_TrendMod), col=2, lwd=2)
legend('top',  legend=c('Agg Hrs', 'pre-01 trend'), lwd=c(2,2), col=c(1,2), horiz=TRUE, bty='n')
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

newDF <- data.frame(tt=as.numeric(index(AWHI)))
longFit <- predict(AWHI_TrendMod, newDF)

outputGap <- xts(coredata(AWHI)/exp(longFit), order.by=index(AWHI))

png(file.path(plotPATH, "NFP_HrsOGap.png"))
plot(outputGap, main="Deviation from long term trendline", las=1)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_AveHrsManuLong.png"))
plot(AWHMAN['1946::'], main="Ave. Weekly Hrs in Manufacturing", las=1, pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFP_AveHrsManuShort.png"))
plot(AWHMAN['1999::'], main="Ave. Weekly Hrs in Manufacturing", las=1, type='o', pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

# plot NFP 3m/3m with the other 4 also on the chart

NFP_33 <- ((PAYEMS + lag(PAYEMS) + lag(PAYEMS, 2)) / (lag(PAYEMS, 3)+ lag(PAYEMS, 4) + lag(PAYEMS, 5)) - 1)*100
CST_33 <- ((USCONS + lag(USCONS) + lag(USCONS, 2)) / (lag(USCONS, 3)+ lag(USCONS, 4) + lag(USCONS, 5)) - 1)*100
MAN_33 <- ((MANEMP + lag(MANEMP) + lag(MANEMP, 2)) / (lag(MANEMP, 3)+ lag(MANEMP, 4) + lag(MANEMP, 5)) - 1)*100
TMP_33 <- ((TEMPHELPS + lag(TEMPHELPS) + lag(TEMPHELPS, 2)) / (lag(TEMPHELPS, 3)+ lag(TEMPHELPS, 4) + lag(TEMPHELPS, 5)) - 1)*100
RET_33 <- ((USTRADE + lag(USTRADE) + lag(USTRADE, 2)) / (lag(USTRADE, 3)+ lag(USTRADE, 4) + lag(USTRADE, 5)) - 1)*100

NFP_ind <- cbind(NFP_33, CST_33, MAN_33, TMP_33, RET_33)

xyplot(NFP_ind['1999::'])
NFP_ind_short <- NFP_ind['1999::']


CycIndKey <- list(space = 'inside',
              column = 1,
              text = list(c('All', 'Construction', 'Manu'), col = c(1,2,4))
             )

CycJobPlot <- xyplot(coredata(NFP_ind_short$PAYEMS) + coredata(NFP_ind_short$USCONS) +
                     coredata(NFP_ind_short$MANEMP) ~ index(NFP_ind_short),
                     type='l', xlab="", ylab="", main="US Employment Growth %3m/3m",
                     col = c(1,2,4), lwd=c(4,2,2),
                     key=CycIndKey)

png(file.path(plotPATH, "NFP_3m3mCyc.png"))
grid.arrange(CycJobPlot, sub=mtex)
dev.off()

# split this into two charts -- all on both, and Manu + const & Temp + retail
ServCycIndKey <- list(space = 'inside',
              column = 1,
              text = list(c('All', 'Temp Help'), col = c(1,3))
              # points = list(pch = 0, col = c(2,4))
             )

ServCycJobPlot <- xyplot(coredata(NFP_ind_short$PAYEMS) + coredata(NFP_ind_short$TEMPHELPS) ~ index(NFP_ind_short),
                     type='l', xlab="", ylab="", main="US Employment Growth %3m/3m",
                     col = c(1,3), lwd=c(4,2),
                     key=ServCycIndKey)

png(file.path(plotPATH, "NFP_3m3mTemp.png"))
grid.arrange(ServCycJobPlot, sub=mtex)
dev.off()

hrs_perPop <- AWHI/CNP16OV
hr_PopIdx <- scale(hrs_perPop)

png(file.path(plotPATH, "NFP_hrsPerPop.png"))
plot(hr_PopIdx, main="Hours per head Index (Ave=0)", las=1, pch=20)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "NFPk_6mmaD200k.png"))
plot(SMA(NFP_m, 6)['2010::'] - 200, las=1, main = 'diff. from 200k')
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "JobsPPTpeak.png"))
plot(100*(log(PAYEMS) - log(cummax(PAYEMS))), las=1, main = 'Est Employment: % difference from peak')
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: FRED', side=1, line=4, adj=0)
dev.off()
