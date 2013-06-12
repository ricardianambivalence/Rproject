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
fedMid <- (DFEDTARU + DFEDTARL)/2
names(fedMid) <- 'fedFunds'
FFR <- rbind(DFEDTAR, fedMid)
names(FFR) <- 'FFR'


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

# info crit -- set to FPE
icT <- "FPE"

# {{{ arrange data sets
# demand, FCI, 3m bill
VAR3frame <- cbind(CFNAI, FCI$NFCI, FFR)
VAR3frame_3m <- rollapplyr(na.locf(VAR3frame), 3, colMeans)['19820301::20090331']

# PPIe_shock, demand, FCI, 3m bill
VAR4frame <- cbind(PPIe_shock, CFNAI, FCI$NFCI, FFR)
VAR4frame_3m <- rollapplyr(na.locf(VAR4frame), 3, colMeans)['19820301::20090331']

# demand, FCI, cPCE, 3m bill
VAR4pframe <- cbind(CFNAI, FCI$NFCI, corePCE_6mAR, FFR)
VAR4pframe_3m <- rollapplyr(na.locf(VAR4pframe), 3, colMeans)['19820301::20090331']

# PPIe_shock, demand, FCI, inflation, 3m bill
VAR5frame <- cbind(PPIe_shock, CFNAI, FCI$NFCI, corePCE_6mAR, FFR)
VAR5frame_3m <- rollapplyr(na.locf(VAR5frame), 3, colMeans)['19820301::20090331']
# }}}close data arrange
# ==> TODO -- email the dudes at the chicago fed about the FCI in a VAR ... adj or no?

# VAR modeling

## {{{ three part VAR
optLag3 <- findMaxVARLag(VAR3frame_3m, firstMax = 9, crit = paste0(icT, "(n)"))

# {{{VAR test stuff -- rmsfe etc
fed3VAR.test6 <- testVar(VAR3frame_3m, skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed3_6m <- errTstVar(fed3VAR.test6)
# print(sumTestError.fed3_6m$r)

fed3VAR.test12 <- testVar(VAR3frame_3m, skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed3_12m <- errTstVar(fed3VAR.test12)
# }}} close rmsfe
fed3VAR.mod <- VAR(scale(VAR3frame_3m), p = optLag3, ic = icT)
fed3VAR.mod2 <- VAR((VAR3frame_3m), p = optLag3, ic = icT)
fed3VAR.pp <- predict(fed3VAR.mod2, n.ahead = 60)
fed3VAR.irf <- irf(fed3VAR.mod2, n.ahead=48)
# }}} close 3 part VAR

## {{{ four part VAR -- added oil shocks
optLag4 <- findMaxVARLag(VAR4frame_3m, firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed4VAR.test6 <- testVar(VAR4frame_3m, skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed4_6m <- errTstVar(fed4VAR.test6)
# print(sumTestError.fed4_6m$r)

fed4VAR.test12 <- testVar(VAR4frame_3m, skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed4_12m <- errTstVar(fed4VAR.test12)
# }}} close rmsfe
fed4VAR.mod <- VAR(scale(VAR4frame_3m), p = optLag4, ic = icT)
fed4VAR.mod2 <- VAR((VAR4frame_3m), p = optLag4, ic = icT)
fed4VAR.pp <- predict(fed4VAR.mod2, n.ahead = 60)
fed4VAR.irf <- irf(fed4VAR.mod2, n.ahead=48)
# }}} close 4 part VAR

## {{{ four part VAR -- corePCE not oilShocks
optLag4p <- findMaxVARLag(VAR4pframe_3m, firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed4pVAR.test6 <- testVar(VAR4pframe_3m, skip = 94, nAhead = 6, Vlag = 6, IC = icT)
sumTestError.fed4p_6m <- errTstVar(fed4pVAR.test6)
# print(sumTestError.fed4_6m$r)

fed4pVAR.test12 <- testVar(VAR4pframe_3m, skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed4p_12m <- errTstVar(fed4pVAR.test12)
# }}} close rmsfe
fed4pVAR.mod <- VAR(scale(VAR4pframe_3m), p = optLag4p, ic = icT)
fed4pVAR.mod2 <- VAR((VAR4pframe_3m), p = optLag4p, ic = icT)
fed4pVAR.pp <- predict(fed4pVAR.mod2, n.ahead = 60)
fed4pVAR.irf <- irf(fed4pVAR.mod2, n.ahead=48)
# }}} close 4p part VAR

# {{{ five part VAR - added 6mAR core PCE
optLag5 <- findMaxVARLag(VAR5frame_3m, firstMax = 9, crit = paste0(icT, "(n)")) # 5

# {{{VAR test stuff -- rmsfe etc
fed5VAR.test6 <- testVar(VAR5frame_3m, skip = 94, nAhead = 6, Vlag = 9, IC = icT)
sumTestError.fed5_6m <- errTstVar(fed5VAR.test6)
# print(sumTestError.fed5_6m$r)

fed5VAR.test12 <- testVar(VAR5frame_3m, skip = 94, nAhead = 12, Vlag = 9, IC = icT)
sumTestError.fed5_12m <- errTstVar(fed5VAR.test12)
# }}} close rmsfe
fed5VAR.mod <- VAR(scale(VAR5frame_3m), p = 6, ic = icT)
fed5VAR.mod2 <- VAR(VAR5frame_3m, p = optLag5, ic = icT)
fed5VAR.pp <- predict(fed5VAR.mod2, n.ahead = 60)
fed5VAR.irf <- irf(fed5VAR.mod2, n.ahead=48)
# }}} close 5 part VAR

# {{{ spider POOS plots
## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 3 VAR
pdf(file.path(plotPATH, "fed3VAR.pdf"))
plot.zoo(fed3VAR.test6$tbill3m['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed3VAR.test6$tbill3m)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed3VAR.test6$tbill3m) - 1)),
         type = c('s', rep('l', ncol(fed3VAR.test6$tbill3m) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 3 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4 VAR
pdf(file.path(plotPATH, "fed4VAR.pdf"))
plot.zoo(fed4VAR.test6$tbill3m['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed4VAR.test6$tbill3m)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed4VAR.test6$tbill3m) - 1)),
         type = c('s', rep('l', ncol(fed4VAR.test6$tbill3m) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 4 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 4p VAR
pdf(file.path(plotPATH, "fed4pVAR.pdf"))
plot.zoo(fed4pVAR.test6$tbill3m['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed4pVAR.test6$tbill3m)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed4pVAR.test6$tbill3m) - 1)),
         type = c('s', rep('l', ncol(fed4pVAR.test6$tbill3m) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 4p Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()

## note, as we have stopped estimation at Mar'09, need a way to thread new data into pred function
# =-> 5 VAR
pdf(file.path(plotPATH, "fed5VAR.pdf"))
plot.zoo(fed5VAR.test6$tbill3m['1993::'],
         screen=1,
         col=c(1, rep(8, ncol(fed5VAR.test6$tbill3m)-1)),
         las=1,
         lwd = c(3, rep(1, ncol(fed5VAR.test6$tbill3m) - 1)),
         type = c('s', rep('l', ncol(fed5VAR.test6$tbill3m) - 1)),
         main = "Pseudo Out of Sample (6m forecasts): 5 Part FED VAR",
         xlab = "",
         ylab = ""
         )
mtext(text="Source: FRED ", side=1, line=4, adj=1)
dev.off()
# }}}close spider plots
