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

# {{{data stuff
getData <- FALSE

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
endEst <- "20090331"
estRange <- paste0(startEst, "::", endEst)

# demand, FCI, 3m bill
VAR3frame <- cbind(CFNAI, FCI$NFCI, FFR)
VAR3frame_3m <- rollapplyr(na.locf(VAR3frame), 3, colMeans)

# demand, FCI, 3m bill
VAR3pframe <- cbind(CFNAI, corePCE_6mAR, FFR)
VAR3pframe_3m <- rollapplyr(na.locf(VAR3pframe), 3, colMeans)

# demand, FCI, 3m bill
VAR3urframe <- cbind(UNRATE, corePCE_6mAR, FFR)
VAR3urframe_3m <- rollapplyr(na.locf(VAR3urframe), 3, colMeans)

# PPIe_shock, demand, FCI, 3m bill
VAR4frame <- cbind(PPIe_shock, CFNAI, FCI$NFCI, FFR)
VAR4frame_3m <- rollapplyr(na.locf(VAR4frame), 3, colMeans)

# demand, FCI, cPCE, 3m bill
VAR4pframe <- cbind(CFNAI, FCI$NFCI, corePCE_6mAR, FFR)
VAR4pframe_3m <- rollapplyr(na.locf(VAR4pframe), 3, colMeans)

# PPIe_shock, demand, FCI, inflation, 3m bill
VAR5frame <- cbind(PPIe_shock, CFNAI, FCI$NFCI, corePCE_6mAR, FFR)
VAR5frame_3m <- rollapplyr(na.locf(VAR5frame), 3, colMeans)

# demand, UR, FCI, inflation, 3m bill
VAR5urframe <- cbind(CFNAI, UNRATE, FCI$NFCI, corePCE_6mAR, FFR)
VAR5urframe_3m <- rollapplyr(na.locf(VAR5urframe), 3, colMeans)

# PPIe_shock, demand, UR, FCI, inflation, 3m bill
VAR6frame <- cbind(PPIe_shock, CFNAI, UNRATE, FCI$NFCI, corePCE_6mAR, FFR)
VAR6frame_3m <- rollapplyr(na.locf(VAR6frame), 3, colMeans)
# }}}close data arrange
# ==> TODO -- email the dudes at the chicago fed about the FCI in a VAR ... adj or no?

# VAR modeling

## {{{ three part VAR
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

## {{{ three part VAR: UR p cash
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

## {{{ three part VAR: with p ex oil FCI
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

## {{{ four part VAR -- added oil shocks
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

## {{{ four part VAR -- corePCE not oilShocks
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

# {{{ five part VAR - added 6mAR core PCE
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

# {{{ five part VAR - CF-UR-FCI-PCE-FFR
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

# {{{ 6 part VAR - PPIe_shock-CF-UR-FCI-PCE-FFR
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
pdf(file.path(plotPATH, "fed3VAR.pdf"))
plot.zoo(fed3VAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed3VAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed3VAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed3VAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 3 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3ur VAR
pdf(file.path(plotPATH, "fed3urVAR.pdf"))
plot.zoo(fed3urVAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed3urVAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed3urVAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed3urVAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 3ur Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3p VAR
pdf(file.path(plotPATH, "fed3pVAR.pdf"))
plot.zoo(fed3pVAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed3pVAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed3pVAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed3pVAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 3p Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4 VAR
pdf(file.path(plotPATH, "fed4VAR.pdf"))
plot.zoo(fed4VAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed4VAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed4VAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed4VAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 4 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4p VAR
pdf(file.path(plotPATH, "fed4pVAR.pdf"))
plot.zoo(fed4pVAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed4pVAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed4pVAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed4pVAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 4p Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 5 VAR
pdf(file.path(plotPATH, "fed5VAR.pdf"))
plot.zoo(fed5VAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed5VAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed5VAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed5VAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 5 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 5ur VAR
pdf(file.path(plotPATH, "fed5urVAR.pdf"))
plot.zoo(fed5urVAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed5urVAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed5urVAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed5urVAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 5ur Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 6 VAR
pdf(file.path(plotPATH, "fed6VAR.pdf"))
plot.zoo(fed6VAR.test6$FFR['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed6VAR.test6$FFR)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed6VAR.test6$FFR) - 1)),
         type = c('s', rep('l', ncol(fed6VAR.test6$FFR) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 6 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

# }}}close spider plots

# {{{ prediction out of sample

varsNewData <- function(varsMODEL, varsDATA, projFWD = 1)
{
    varsDATA_pp <- varsDATA  # make a frame to extend
# make the coeff vectors
    for (i in seq_along(get('varresult', varsMODEL)))
         {
         assign(paste0('modCoeffs_v', i),
                get('varresult', varsMODEL)[[i]]$coeff
                )
         }
# find max lag length -- from coeff vector
    maxLag <- max(as.numeric(unlist(regmatches(names(modCoeffs_v1),
                                               gregexpr('\\(?[0-9]+',
                                                        names(modCoeffs_v1))))))
    n = 1
# make an expanded DF so you can %*% using coeff matrix
    while (n < (1+projFWD))
    {
        env4step <- new.env()
        expandedDF <- varsDATA_pp # the df we build up
        for (j in 1:(maxLag-1))
        {
            expandedDF <- merge(expandedDF, lag(varsDATA_pp, j))
        }
        expandedDF$const <- 1
        for (k in seq_along(get('varresult', varsMODEL)))
        {
            assign(paste0('v', k, '_pp'),
                   tail(expandedDF, 1) %*% get(paste0('modCoeffs_v', k)),
                   envir = env4step)
        }
# join the forecasts together
        vvlist = list() # an empty list to contain the variable names
        for (l in seq_along(get('varresult', varsMODEL)))
        {
            vvlist <- c(vvlist, paste0('v', l, '_pp'))
            l = l + 1
        } # this loop puts the names into the list ... use environments?
        print(vvlist)
# now put it together
        nextDate <- seq(last(index(varsDATA_pp)), by = 'mon', length.out = 2)[-1]
        newRow <- xts(do.call(cbind, lapply(vvlist, get, envir = env4step)),
                      order.by = nextDate)
        names(newRow) <- names(varsDATA_pp)
        varsDATA_pp <- rbind(varsDATA_pp, newRow)
        rm(expandedDF)
        rm(list = ls(env4step), envir = env4step)
        n = n + 1
    }
    return(varsDATA_pp)
}

testProj <- varsNewData(fed6VAR.mod2, VAR6frame_3m, projFWD = 36)

