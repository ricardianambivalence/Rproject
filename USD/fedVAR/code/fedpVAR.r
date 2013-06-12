#{{{set-up Packs and Funs
rm(list=ls()); gc()
Sys.setenv(TZ = 'UTC')
#
#packages and functions
require(gdata)
require(vars)
require(xts)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(quantmod)
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")
# }}}

# {{{ PATHstuff
projPATH <- file.path("~/R/usd/fedvar")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}

getData <- TRUE

if(getData)
{
    # data is CF_NAI, cf_fci, adj_cf_fci, unrate,
    # fftarget, upper, lower, 3mTbill, spot oil, gasoline
    dataNames <- c('CFNAI', 'NFCI', 'ANFCI', 'UNRATE', 'PCEPILFE',
                   'DFEDTAR', 'DFEDTARU', 'DFEDTARL', 'WGS3MO', 'OILPRICE',
                   'GASREGW', 'DCOILBRENTEU'
                   )
    getSymbols(dataNames,src='FRED', return.class = 'xts')

    save(CFNAI, NFCI, ANFCI, UNRATE, PCEPILFE, DFEDTAR, DFEDTARU, DFEDTARL,
         WGS3MO, OILPRICE, GASREGW, DCOILBRENTEU,
         file = file.path(dataPATH, "fedVARdata.rdata"))
} else {
    load(file = file.path(dataPATH, "fedVARdata.rdata"))
}

FCI <- apply.monthly(merge(NFCI, ANFCI), colMeans)
index(FCI) <- dateSwitch(index(FCI), lastDay = FALSE)

corePCE_m <- 100*(PCEPILFE / lag(PCEPILFE, 1) - 1)
corePCE_y <- 100*(PCEPILFE / lag(PCEPILFE, 12) - 1)
corePCE_6mAR <- ((1 + rollapplyr(corePCE_m/100, 6, mean))^12 - 1)*100
names(corePCE_y) <- 'corePCE_y'
names(corePCE_6mAR) <- 'corePCE_6mAR'

tbill3m <- apply.monthly(WGS3MO, mean, na.rm=T)
names(tbill3m) <- 'tbill3m'
index(tbill3m) <- dateSwitch(index(tbill3m), lastDay = FALSE)

# info crit -- set to FPE
icT <- "FPE"

# demand, FCI, inflatin, 3m bill
VAR4frame <- cbind(CFNAI, FCI$NFCI, corePCE_6mAR, tbill3m$tbill3m)
VAR4frame_3m <- rollapplyr(na.locf(VAR4frame), 3, colMeans)['19820301::20090331']

# demand, FCI, inflation, oil, 3m bill
VAR5frame <- cbind(UNRATE, FCI$NFCI, corePCE_y, _OIL_, tbill3m$tbill3m)
VAR5frame_3m <- rollapplyr(na.locf(VAR5frame), 3, colMeans)['19820301::20081231']

# ==> TODO -- email the dudes at the chicago fed about the FCI in a VAR ... adj or no?

# VAR modeling


## {{{ four part VAR
optLag4 <- findMaxVARLag(VAR4frame_3m, firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed4VAR.test6 <- testVar(scale(VAR4frame_3m), skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed4_6m <- errTstVar(fed4VAR.test6)
print(sumTestError.fed4_6m$r)

# fed4VAR.test12 <- testVar(scale(VAR4frame_3m), skip = 94, nAhead = 12, Vlag = 9, IC = icT)
# sumTestError.fed4_12m <- errTstVar(fed4VAR.test12)
# }}} close rmsfe
fed4VAR.mod <- VAR(scale(VAR4frame_3m), p = optLag, ic = icT)
fed4VAR.mod2 <- VAR((VAR4frame_3m), p = optLag, ic = icT)
fed4VAR.pp <- predict(VAR(VAR4frame_3m, p = optLag, ic = icT), n.ahead = 60)
fed4VAR.irf <- irf(fed4VAR.mod2, n.ahead=48)
# }}} close 4 part VAR

# {{{ five part VAR - add in Oil

optLag5 <- findMaxVARLag(VAR5frame_3m, firstMax = 9, crit = paste0(icT, "(n)")) # 5
# {{{VAR test stuff -- rmsfe etc
fed5VAR.test6 <- testVar(scale(VAR5frame_3m), skip = 94, nAhead = 6, Vlag = 9, IC = icT)
sumTestError.fed5_6m <- errTstVar(fed5VAR.test6)
#
fed5VAR.test12 <- testVar(scale(VAR5frame_3m), skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed5_12m <- errTstVar(fed5VAR.test12)
# }}} close rmsfe
fed5VAR.mod <- VAR(scale(VAR5frame_3m), p = optLag, ic = icT)
fed5VAR.mod2 <- VAR((VAR4frame_3m), p = optLag, ic = icT)
fed5VAR.pp <- predict(VAR(VAR5frame_3m, p = optLag, ic = icT), n.ahead = 60)
fed5VAR.irf <- irf(fed5VAR.mod2, n.ahead=48)
# }}} close 5 part VAR
