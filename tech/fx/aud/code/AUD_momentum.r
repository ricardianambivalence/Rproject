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
require(PerformanceAnalytics)
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")
# }}}
# {{{ PATHstuff
projPATH <- file.path("~/R/tech/fx/aud")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}
getWEB <- FALSE
## {{{ Step 1: Get the data
if(getWEB)
{
    dataNames <- c('DEXUSAL')
    getSymbols(dataNames,src='FRED', return.class = 'xts')
    AUDUSD <- DEXUSAL['1984::']
    names(AUDUSD) <- 'AUDUSD'
    save(AUDUSD, file = file.path(dataPATH, "audusd.rdata"))
} else {
    load(file = file.path(dataPATH, "audusd.rdata"))
}

# }}} end get data

# a simple sharpe ratio mean return  div std dev
.sharpe <- function(rets, bmk = 0) { SR <- (mean(rets - bmk) / sd(rets - bmk)) }

# a switch for sharpe ratios of various frequencies
mj_sharpeRswitch <- function(eqty, bmk = 0, freq = 'weekly')
{
    permittedFreq <- c('daily', 'weekly', 'monthly', 'quarterly', 'yearly')
    if (!freq %in% permittedFreq) {
        stop('freq must be one of: daily, weekly, monthly, quarterly, yearly')
    }
    switch(freq,
           daily = { SR <- .sharpe(diff(eqty, log=TRUE, na.pad=FALSE)) * 252^0.5},
           weekly = { SR <- .sharpe(diff( apply.weekly(eqty, last),
                                         log=TRUE, na.pad=FALSE)) * 52^0.5},
           monthly = { SR <- .sharpe(diff( apply.monthly(eqty, last),
                                         log=TRUE, na.pad=FALSE)) * 12^0.5},
           quarterly = { SR <- .sharpe(diff( apply.quarterly(eqty, last),
                                         log=TRUE, na.pad=FALSE)) * 4^0.5},
           yearly = {SR <- .sharpe(diff(eqty, log=TRUE, na.pad=FALSE)) }
           )
    return(SR)
}

# this makes the signal using a MA crossover
sigMake_MA <- function(obj, shortMA, longMA, lagLen = 1)
{
    shortMave <- SMA(obj, shortMA)
    longMave <- SMA(obj, longMA)
    signal <- lag(ifelse(shortMave > longMave, 1, ifelse(shortMave < longMave, -1, 0)), lagLen)
    names(signal) <- paste0('maX_', shortMA, 'x', longMA)
    return(signal)
}

minePar_SMA <- function(obj, shortRange, longRange, bmk = 0, lagLen = 1)
{
    SRmtx <- matrix(NA, nrow = max(shortRange), ncol = max(longRange))
    for (i in min(shortRange):max(shortRange)) {
        for (j in min(longRange):max(longRange)) {
            if (i < j) {
                sig <- sigMake_MA(obj, i, j, lagLen)
                SR <- .sharpe(ROC(obj) * sig[!is.na(sig)])
                SRmtx[i,j] <- SR
            }
        }
    }
    return(SRmtx)
}

# TODO - find a which function to report row and col of max value in matrix
smaPAR <- minePar_SMA(AUDUSD, 1:20, 2:63, bmk = 0, lagLen = 1)

tradeMA <- function(obj, shortMA, longMA, bmk = 0, lagLen = 1)
{
    signal <- sigMake_MA(obj, shortMA, longMA, lagLen)
    returns <- ROC(obj) * signal[!is.na(signal)]
    names(returns) <- 'Returns'
    equity <- exp(cumsum(returns))
    on.exit(plot(equity, main = paste0(names(obj), ' -- Moving Average Crossover: ', shortMA, 'x',
                                       longMA)))
    retList <- list('Signal' = signal, 'Returns' = returns,
                    'Equity' = equity)
}

tt8x13 <- tradeMA(AUDUSD, 1, 3)
mret = merge(diff(AUDUSD, log=T), tt8x13$Returns)


table.Drawdowns(tt8x13, top=10)
table.DownsideRisk(tt8x13)
charts.PerformanceSummary(tt8x13)

# this function needs fixing ...
srByYr <- function(ret, bmk=0){
    # a function to return sharpe ratio by year
    aret <- ret - ((1+ coc)^(1/365) -1)
    yy <- unique(as.POSIXlt(index(ret))$year) + 1900
    SRlist <- list()

    for (i in 1:length(yy)){
        py <- paste(yy[i])
        SRyear <- sqrt(252) * apply(aret[py], 2, mean) / apply(aret[py], 2, sd)
        SRlist[[py]] <- SRyear
    }

    return(SRlist)
}

v2rByYr <- function(ret, coc=0){
    # a function to return the v^2 Ratio by year
    aret <- ret - ((1+ coc)^(1/365) -1)
    yy <- unique(as.POSIXlt(index(ret))$year) + 1900
    v2List  <- list()

    for (i in 1:length(yy)){
        py <- paste(yy[i])
        eq <- exp(cumsum(ret[py]))
        eqMax <- cummax(eq[py])
        v2Ratio <- (coredata(last(eq)) / coredata(first(eq)) - 1) /
            (sqrt(sum((eq/eqMax -1)^2)/nrow(eq) + 1))
        v2List[[py]] <- v2Ratio
    }
    return(v2List)
}

rmdByYr <- function(ret, coc=0){
    # a function to return the v^2 Ratio by year
    aret <- ret - ((1+ coc)^(1/365) -1)
    yy <- unique(as.POSIXlt(index(ret))$year) + 1900
    rmdList  <- list()

    for (i in 1:length(yy)){
        py <- paste(yy[i])
        eq <- exp(cumsum(ret[py]))
        eqMax <- cummax(eq[py])
        rmdRatio <- (coredata(last(eq)) / coredata(first(eq)) - 1) / (1 - min(eq/eqMax))
        rmdList[[py]] <- rmdRatio
    }
    return(rmdList)
}


