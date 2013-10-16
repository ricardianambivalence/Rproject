require(quantmod)
require(PerformanceAnalytics)
require(reshape2)

getData <- FALSE

# Step 1: Get the data
if(getData){
    dataNames <- c('DEXUSAL')
    getSymbols(dataNames,src='FRED', return.class = 'xts')
    AUDUSD <- DEXUSAL['1984::']
} else {
    load(file = "~/Rproject/tech/fx/aud/data/AUDUSD.RData")
}

# Step 2: Create your indicator
for (i in 2:100) {
    for (j in 1:i) {
        assign(paste0('AUD_ma', j), SMA(AUDUSD, j))
        assign(paste0('AUD_ma', i), SMA(AUDUSD, i))
#replace AUD_ma5 and AUD_ma10 with shortMA and longMA
        sig <- Lag(ifelse(AUD_ma5 > AUD_ma10, 1, ifelse(AUD_ma5 < AUD_ma10, -1, 0)))
    }
}
AUD_ma5 <- SMA(AUDUSD, 5)
AUD_ma10 <- SMA(AUDUSD, 10)

# Step 3: Construct your trading rule
sig <- Lag(ifelse(AUD_ma5 > AUD_ma10, 1, ifelse(AUD_ma5 < AUD_ma10, -1, 0)))

# Step 4: The trading rules/equity curve
ret <- ROC(AUDUSD)*sig
ret <- ret['19840201::']
eq <- exp(cumsum(ret))
eqMax <- cummax(eq)

# plot(eq, log='y')

# Step 5: Evaluate strategy performance
srByYr <- function(ret, coc=0){
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

yearSR <- melt(srByYr(ret))
yearV2 <- melt(v2rByYr(ret))[, -c(1:2)]
yearRMD <- melt(rmdByYr(ret))[, -c(1:2)]
names(yearSR) <- c('sharpeRatio', 'year')
names(yearV2) <- c('adjReturn', 'year')
names(yearRMD) <- c('returnMaxDrawdown', 'year')
barplot(yearSR$sharpeRatio, names.arg=yearSR$year, las=2, main="Sharpe Raitio by year") #


totalSR  <- sqrt(252)*apply(ret,2,mean)/apply(ret, 2,sd) #AR by sqrt trading days per yr
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)

v2Ratio <- ((Value(t) - Value(t0))^(p/n) - 1) / (sqrt())
