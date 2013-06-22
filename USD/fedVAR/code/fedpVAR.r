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
# {{{data stuff

if(getData)
{
    # data is CF_NAI, cf_fci, adj_cf_fci, unrate,
    # fftarget, upper, lower, 3mTbill, spot oil, gasoline
    # PPI crude energy index
    dataNames <- c('CFNAI', 'NFCI', 'ANFCI', 'UNRATE', 'PCEPILFE',
                   'DFEDTAR', 'DFEDTARU', 'DFEDTARL', 'WGS3MO', 'OILPRICE',
                   'GASREGW', 'DCOILBRENTEU', 'PPICEM'
                   )
    getSymbols(dataNames,src='FRED', return.class = 'xts')
    save(CFNAI, NFCI, ANFCI, UNRATE, PCEPILFE, DFEDTAR, DFEDTARU, DFEDTARL,
         WGS3MO, OILPRICE, GASREGW, DCOILBRENTEU, PPICEM,
         file = file.path(dataPATH, "fedVARdata.rdata"))
} else {
    load(file = file.path(dataPATH, "fedVARdata.rdata"))
}

# make a single FFR instrument - use mid after 15 Dec 2008
fedMid <- (coredata(DFEDTARU) + coredata(DFEDTARL))/2
names(fedMid) <- 'fedFunds'
FFR <- xts(rbind(coredata(DFEDTAR), fedMid), order.by = c(index(DFEDTAR), index(DFEDTARU)))
names(FFR) <- 'FFR'
FFR <- apply.monthly(na.locf(FFR), last)
index(FFR) <- dateSwitch(index(FFR), lastDay = FALSE)

FCI <- apply.monthly(merge(NFCI, ANFCI), colMeans)
index(FCI) <- dateSwitch(index(FCI), lastDay = FALSE)

corePCE_m <- 100*diff(PCEPILFE, log=TRUE)
corePCE_y <- 100*(PCEPILFE / lag(PCEPILFE, 12) - 1)
corePCE_6mAR <- ((1 + rollapplyr(corePCE_m/100, 6, mean))^12 - 1)*100
names(corePCE_y) <- 'corePCE_y'
names(corePCE_6mAR) <- 'corePCE_6mAR'

tbill3m <- apply.monthly(WGS3MO, mean, na.rm=TRUE)
names(tbill3m) <- 'tbill3m'
index(tbill3m) <- dateSwitch(index(tbill3m), lastDay = FALSE)

BRENT_m <- apply.monthly(DCOILBRENTEU, mean, na.rm=TRUE)
index(BRENT_m) <- as.Date(dateSwitch(index(BRENT_m), lastDay = FALSE))
LT_tt <- seq(index(first(OILPRICE)), index(last(BRENT_m)), by = 'mon')
LToil <- xts(rbind(coredata(OILPRICE['::19870401']),
                   coredata(BRENT_m['19870501::'])),
             order.by = LT_tt)
LToil_m <- 100*diff(LToil, log=TRUE)
LToil_y <- 100*(LToil / lag(LToil, 12) - 1)
LToil_6mAR <- ((1 + rollapplyr(LToil_m/100, 6, mean))^12 - 1)*100
LToil_3yrMax <- rollapplyr(LToil, 36, max)
LToil_shock <- xts(pmax(0, log(LToil / lag(LToil_3yrMax,6))), order.by = index(LToil))
names(LToil_shock) <- 'LToil_shock'

PPIenergy <- PPICEM
names(PPIenergy) <- 'PPIenergy'
if(index(index(last(PPIenergy)) < index(last(BRENT_m)))) {
    tt_extnd <- seq(index(first(PPIenergy)), index(last(LToil_m)), by = 'mon')
    PPIe_act <- coredata(PPIenergy)
    extndAfter <- dateSwitch(index(last(PPIenergy)), lastDay = FALSE, adv = 1)
    extndAfter_x <- paste0(extndAfter, "::")
    extnd_PPIe <- vector(mode= 'numeric', length = length(LToil_m[extndAfter_x]))
    for (i in seq_along(extnd_PPIe)) {
        if((i) == 1){
            extnd_PPIe[i] <- tail(PPIe_act,1) * (1 + coredata(LToil_m[extndAfter_x][i])/100)
        } else {
            extnd_PPIe[i] <- extnd_PPIe[(i-1)] * (1 + coredata(LToil_m[extndAfter_x][i])/100)
        }
    }
    PPIe_extnd <- xts(c(PPIe_act, extnd_PPIe), tt_extnd)
}

PPIe_m <- 100 * diff(PPIe_extnd, log=TRUE)
PPIe_6mAR <- ((1 + rollapplyr(PPIe_m/100, 6, mean))^12 -1) * 100

# gasoline prices
gas_3yrmax <- rollapplyr(GASREGW, 36, max)
gasShock <- xts(pmax(0, log(GASREGW / lag(gas_3yrmax, 6))), order.by = index(GASREGW))
names(gasShock) <- 'gasShock'

PPIe_3yrMax <- rollapplyr(PPIe_extnd, 36, max)
PPIe_shock <- xts(pmax(0, log(PPIe_extnd / lag(PPIe_extnd, 6))), order.by = tt_extnd)
names(PPIe_shock) <- 'PPIe_shock'

# plot(LToil_shock, las=1)
# lines(PPIe_shock, col=3)
# lines(gasShock, col=4)
# }}} close data stuff

# info crit -- set to FPE
icT <- "FPE"

# {{{ arrange data sets

startEst <- "19821101"
endEst <- "20081231"
estRange <- paste0(startEst, "::", endEst)

# any sensible strategy must have UR-cPCE-FFR as the core
# this is at the core of the policy rules we think the FOMC follows

# 3 variable VARs

# demand, FCI, FFR
VAR3frame <- cbind(CFNAI, FCI$NFCI, FFR)
VAR3frame_3m <- rollapplyr(na.locf(VAR3frame), 3, colMeans)

# demand, UR, FFR
VAR3urframe <- cbind(CFNAI, UNRATE, FFR)
VAR3urframe_3m <- rollapplyr(na.locf(VAR3urframe), 3, colMeans)

# demand, cPCE, FFR
VAR3pframe <- cbind(CFNAI, corePCE_6mAR, FFR)
VAR3pframe_3m <- rollapplyr(na.locf(VAR3pframe), 3, colMeans)

# FCI, UR, FFR
VAR3Fuframe <- cbind(FCI$NFCI, UNRATE, FFR)
VAR3Fuframe_3m <- rollapplyr(na.locf(VAR3Fuframe), 3, colMeans)

# ur, cPCE, FFR
VAR3upframe <- cbind(UNRATE, corePCE_6mAR, FFR)
VAR3upframe_3m <- rollapplyr(na.locf(VAR3upframe), 3, colMeans)

# 4 variable VARs

# demand, FCI, UR, FFR
VAR4frame <- cbind(CFNAI, FCI$NFCI, UNRATE, FFR)
VAR4frame_3m <- rollapplyr(na.locf(VAR4frame), 3, colMeans)

# demand, FCI, cPCE, FFR
VAR4pframe <- cbind(CFNAI, FCI$NFCI, corePCE_6mAR, FFR)
VAR4pframe_3m <- rollapplyr(na.locf(VAR4pframe), 3, colMeans)

# demand, UR, cPCE, FFR
VAR4urframe <- cbind(CFNAI, UNRATE, corePCE_6mAR, FFR)
VAR4urframe_3m <- rollapplyr(na.locf(VAR4urframe), 3, colMeans)

# PPIe, UR, cPCE, FFR
VAR4euframe <- cbind(PPIe_6mAR, UNRATE, corePCE_6mAR, FFR)
VAR4euframe_3m <- rollapplyr(na.locf(VAR4euframe), 3, colMeans)

# PPIe, FCI, UR, FFR
VAR4efuframe <- cbind(PPIe_6mAR, FCI$NFCI, UNRATE, FFR)
VAR4efuframe_3m <- rollapplyr(na.locf(VAR4efuframe), 3, colMeans)

# 5 variable VARs

# demand, FCI, UR, inflation, FFR
VAR5frame <- cbind(CFNAI, FCI$NFCI, UNRATE, corePCE_6mAR, FFR)
VAR5frame_3m <- rollapplyr(na.locf(VAR5frame), 3, colMeans)

# PPIe, Dmnd, FCI, UR, FFR
VAR5eframe <- cbind(PPIe_6mAR, CFNAI, FCI$NFCI, UNRATE, FFR)
VAR5eframe_3m <- rollapplyr(na.locf(VAR5eframe), 3, colMeans)

# PPIe, FCI, UR, inflation, FFR
VAR5efuframe <- cbind(PPIe_6mAR, FCI$NFCI, UNRATE, corePCE_6mAR, FFR)
VAR5efuframe_3m <- rollapplyr(na.locf(VAR5efuframe), 3, colMeans)

# PPIe, Dmnd, FCI, inflation, FFR
VAR5epframe <- cbind(PPIe_6mAR, CFNAI, FCI$NFCI, corePCE_6mAR, FFR)
VAR5epframe_3m <- rollapplyr(na.locf(VAR5epframe), 3, colMeans)

# 6 variable VAR
# PPIe, Dmnd, FCI, UR, inflation, FFR
VAR6frame <- cbind(PPIe_6mAR, CFNAI, FCI$NFCI, UNRATE, corePCE_6mAR, FFR)
VAR6frame_3m <- rollapplyr(na.locf(VAR6frame), 3, colMeans)
# }}}close data arrange

# ==> TODO -- email the dudes at the chicago fed about the FCI in a VAR ... adj or no?

# VAR modeling

## {{{ fed3VAR.mod + VAR3frame_3m :: D-FCI-FFR
optLag3 <- findMaxVARLag(VAR3frame_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)"))

# {{{VAR test stuff -- rmsfe etc
fed3VAR.test6 <- testVar(VAR3frame_3m[estRange], skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed3_6m <- errTstVar(fed3VAR.test6)
# print(sumTestError.fed3_6m$r)

fed3VAR.test12 <- testVar(VAR3frame_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed3_12m <- errTstVar(fed3VAR.test12)
# }}} close rmsfe
fed3VAR.mod <- VAR(scale(VAR3frame_3m[estRange]), p = optLag3, ic = icT)
fed3VAR.mod2 <- VAR((VAR3frame_3m[estRange]), p = optLag3, ic = icT)
fed3VAR.pp <- predict(fed3VAR.mod2, n.ahead = 60)
fed3VAR.irf <- irf(fed3VAR.mod2, n.ahead=48)
# }}} close 3 part VAR

## {{{ fed3urVAR.mod + VAR3urframe_3m :: D-cCPE-FFR
optLag3 <- findMaxVARLag(VAR3urframe_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)"))

# {{{VAR test stuff -- rmsfe etc
fed3urVAR.test6 <- testVar(VAR3urframe_3m[estRange], skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed3ur_6m <- errTstVar(fed3urVAR.test6)
# print(sumTestError.fed3_6m$r)

fed3urVAR.test12 <- testVar(VAR3urframe_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed3ur_12m <- errTstVar(fed3urVAR.test12)
# }}} close rmsfe
fed3urVAR.mod <- VAR(scale(VAR3urframe_3m[estRange]), p = optLag3, ic = icT)
fed3urVAR.mod2 <- VAR((VAR3urframe_3m[estRange]), p = optLag3, ic = icT)
fed3urVAR.pp <- predict(fed3urVAR.mod2, n.ahead = 60)
fed3urVAR.irf <- irf(fed3urVAR.mod2, n.ahead=48)
# }}} close 3ur part VAR

## {{{ fed3VAR.mod + VAR3pframe_3m :: UR-cCPE-FFR
optLag3p <- findMaxVARLag(VAR3pframe_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)"))

# {{{VAR test stuff -- rmsfe etc
fed3pVAR.test6 <- testVar(VAR3pframe_3m[estRange], skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed3p_6m <- errTstVar(fed3pVAR.test6)
# print(sumTestError.fed3_6m$r)

fed3pVAR.test12 <- testVar(VAR3pframe_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed3p_12m <- errTstVar(fed3pVAR.test12)
# }}} close rmsfe
fed3pVAR.mod <- VAR(scale(VAR3pframe_3m[estRange]), p = optLag3, ic = icT)
fed3pVAR.mod2 <- VAR((VAR3pframe_3m[estRange]), p = optLag3, ic = icT)
fed3pVAR.pp <- predict(fed3pVAR.mod2, n.ahead = 60)
fed3pVAR.irf <- irf(fed3pVAR.mod2, n.ahead=48)
# }}} close 3p part VAR

## {{{ fed4VAR.mod + VAR4frame_3m :: PPIe-D-FCI-FFR
optLag4 <- findMaxVARLag(VAR4frame_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed4VAR.test6 <- testVar(VAR4frame_3m[estRange], skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed4_6m <- errTstVar(fed4VAR.test6)
# print(sumTestError.fed4_6m$r)

fed4VAR.test12 <- testVar(VAR4frame_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed4_12m <- errTstVar(fed4VAR.test12)
# }}} close rmsfe
fed4VAR.mod <- VAR(scale(VAR4frame_3m[estRange]), p = optLag4, ic = icT)
fed4VAR.mod2 <- VAR((VAR4frame_3m[estRange]), p = optLag4, ic = icT)
fed4VAR.pp <- predict(fed4VAR.mod2, n.ahead = 60)
fed4VAR.irf <- irf(fed4VAR.mod2, n.ahead=48)
# }}} close 4 part VAR

## {{{ fed4pVAR.mod + VAR4pframe_3m :: D-FCI-cPCE-FFR
optLag4p <- findMaxVARLag(VAR4pframe_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed4pVAR.test6 <- testVar(VAR4pframe_3m[estRange], skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed4p_6m <- errTstVar(fed4pVAR.test6)
# print(sumTestError.fed4_6m$r)

fed4pVAR.test12 <- testVar(VAR4pframe_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed4p_12m <- errTstVar(fed4pVAR.test12)
# }}} close rmsfe
fed4pVAR.mod <- VAR(scale(VAR4pframe_3m[estRange]), p = optLag4p, ic = icT)
fed4pVAR.mod2 <- VAR((VAR4pframe_3m[estRange]), p = optLag4p, ic = icT)
fed4pVAR.pp <- predict(fed4pVAR.mod2, n.ahead = 60)
fed4pVAR.irf <- irf(fed4pVAR.mod2, n.ahead=48)
# }}} close 4p part VAR

## {{{ fed4urVAR.mod + VAR4urframe_3m :: D-UR-FCI-FFR
optLag4ur <- findMaxVARLag(VAR4urframe_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed4urVAR.test6 <- testVar(VAR4urframe_3m[estRange], skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed4ur_6m <- errTstVar(fed4urVAR.test6)
# print(sumTestError.fed4_6m$r)

fed4urVAR.test12 <- testVar(VAR4urframe_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed4ur_12m <- errTstVar(fed4urVAR.test12)
# }}} close rmsfe
fed4urVAR.mod <- VAR(scale(VAR4urframe_3m[estRange]), p = optLag4ur, ic = icT)
fed4urVAR.mod2 <- VAR((VAR4urframe_3m[estRange]), p = optLag4ur, ic = icT)
fed4urVAR.pp <- predict(fed4urVAR.mod2, n.ahead = 60)
fed4urVAR.irf <- irf(fed4urVAR.mod2, n.ahead=48)
# }}} close 4ur part VAR

# {{{ fed5VAR.mod + VAR5frame_3m :: PPIe-D-FCI-cCPE-FFR
optLag5 <- findMaxVARLag(VAR5frame_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed5VAR.test6 <- testVar(VAR5frame_3m[estRange], skip = 94, nAhead = 6, Vlag = 9, IC = icT)
sumTestError.fed5_6m <- errTstVar(fed5VAR.test6)
# print(sumTestError.fed5_6m$r)

fed5VAR.test12 <- testVar(VAR5frame_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed5_12m <- errTstVar(fed5VAR.test12)
# }}} close rmsfe
fed5VAR.mod <- VAR(scale(VAR5frame_3m[estRange]), p = 6, ic = icT)
fed5VAR.mod2 <- VAR(VAR5frame_3m[estRange], p = optLag5, ic = icT)
fed5VAR.pp <- predict(fed5VAR.mod2, n.ahead = 60)
fed5VAR.irf <- irf(fed5VAR.mod2, n.ahead=48)
# }}} close 5 part VAR

# {{{ fed5urVAR.mod + VAR5urframe_3m :: D-UR-FCI-cPCE-FFR
optLag5ur <- findMaxVARLag(VAR5urframe_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed5urVAR.test6 <- testVar(VAR5urframe_3m[estRange], skip = 94, nAhead = 6, Vlag = 9, IC = icT)
sumTestError.fed5ur_6m <- errTstVar(fed5urVAR.test6)
# print(sumTestError.fed5_6m$r)

fed5urVAR.test12 <- testVar(VAR5urframe_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed5ur_12m <- errTstVar(fed5urVAR.test12)
# }}} close rmsfe
fed5urVAR.mod <- VAR(scale(VAR5urframe_3m[estRange]), p = 6, ic = icT)
fed5urVAR.mod2 <- VAR(VAR5urframe_3m[estRange], p = optLag5, ic = icT)
fed5urVAR.pp <- predict(fed5urVAR.mod2, n.ahead = 60)
fed5urVAR.irf <- irf(fed5urVAR.mod2, n.ahead=48)
# }}} close 5 part VAR CF-UR-FCI-PCE-FFR

# {{{ fed6VAR.mod + VAR6frame_3m :: PPIe-D-UR-FCI-cPCE-FFR
optLag6 <- findMaxVARLag(VAR6frame_3m[estRange], firstMax = 9, crit = paste0(icT, "(n)")) #

# {{{VAR test stuff -- rmsfe etc
fed6VAR.test6 <- testVar(VAR6frame_3m[estRange], skip = 94, nAhead = 6, Vlag = 9, IC = icT)
sumTestError.fed6_6m <- errTstVar(fed6VAR.test6)
# print(sumTestError.fed6_6m$r)

fed6VAR.test12 <- testVar(VAR6frame_3m[estRange], skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed6_12m <- errTstVar(fed6VAR.test12)
# }}} close rmsfe
fed6VAR.mod <- VAR(scale(VAR6frame_3m[estRange]), p = 6, ic = icT)
fed6VAR.mod2 <- VAR(VAR6frame_3m[estRange], p = optLag6, ic = icT)
fed6VAR.pp <- predict(fed6VAR.mod2, n.ahead = 60)
fed6VAR.irf <- irf(fed6VAR.mod2, n.ahead=48)
# }}} close 6 part VAR PPIe-CF-UR-FCI-PCE-FFR

# {{{ spider POOS plots
## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3 VAR
pngMk("fed3VAR.pdf")
spiderPOOS(fed3VAR.test6, 'FFR', MAIN = "POOS FFR :: D-FCI-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3ur VAR
pngMk("fed3urVAR.pdf")
spiderPOOS(fed3urVAR.test6, 'FFR', MAIN = "POOS FFR :: D-UR-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3p VAR
pngMk("fed3pVAR.pdf")
spiderPOOS(fed3pVAR.test6, 'FFR', MAIN = "POOS FFR :: D-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3fu VAR
pngMk("fed3fuVAR.pdf")
spiderPOOS(fed3fuVAR.test6, 'FFR', MAIN = "POOS FFR :: FCI-UR-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3up VAR
pngMk("fed3upVAR.pdf")
spiderPOOS(fed3upVAR.test6, 'FFR', MAIN = "POOS FFR :: UR-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4 VAR
pngMk("fed4VAR.pdf")
spiderPOOS(fed4VAR.test6, 'FFR', MAIN = "POOS FFR :: D-FCI-cCPE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4p VAR
pngMk("fed4pVAR.pdf")
spiderPOOS(fed4pVAR.test6, 'FFR', MAIN = "POOS FFR :: D-FCI-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4ur VAR
pngMk("fed4urVAR.pdf")
spiderPOOS(fed4urVAR.test6, 'FFR', MAIN = "POOS FFR :: D-UR-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4eu VAR
pngMk("fed4euVAR.pdf")
spiderPOOS(fed4euVAR.test6, 'FFR', MAIN = "POOS FFR :: PPIe-UR-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4efu VAR
pngMk("fed4efuVAR.pdf")
spiderPOOS(fed4efuVAR.test6, 'FFR', MAIN = "POOS FFR :: PPIe-FCI-UR-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

# =-> 5 VAR
pngMk("fed5VAR.png")
spiderPOOS(fed5VAR.test6, 'FFR', MAIN = "POOS FFR :: D-FCI-UR-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

pngMk("fed5eVAR.png")
spiderPOOS(fed5eVAR.test6, 'FFR', MAIN = "POOS FFR :: PPIe-D-FCI-UR-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

pngMk("fed5fuVAR.png")
spiderPOOS(fed5fuVAR.test6, 'FFR', MAIN = "POOS FFR :: PPIe-FCI-UR-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

pngMk("fed5epVAR.png")
spiderPOOS(fed5epVAR.test6, 'FFR', MAIN = "POOS FFR :: PPIe-D-FCI-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

# =-> 6 VAR
pngMk("fed6VAR.png")
spiderPOOS(fed6VAR.test6, 'FFR', MAIN = "POOS FFR :: PPIe-D-FCI-UR-cPCE-FFR")
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

# }}}close spider plots

# {{{ prediction with new data

testProj <- varsPredictNewData(fed6VAR.mod2, VAR6frame_3m, projFWD = 36)

# }}} close prediction with new data

