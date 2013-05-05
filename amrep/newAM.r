# the target -- a report summarising futures trading 6 charts on 1 A4 page

# the report would be sent out each morning and night summarising the prior session
# covering: XM, YM, IR, IB, TY, FV, and the mini

# containing:

# 1/ colored line chart showing four weeks of price action
# 2/ colored candles covering four weeks -- split 2 ways
# 3/ price range histogram - two ways
# 4/ volume histogram - two ways
# 5/ volume at each price - two way split
# 6/ net agressive flow at each price - two way split


rm(list=ls())
Sys.setenv(TZ='GMT')

# ytEnv <- new.env(parent = globalenv())
# load("~/data/ticPack/ytDat.RData", envir = ytEnv)

# load("~/data/ticPack/largeData.RData")
require(data.table)

# YM <- data.table(ym)
# YM[ , tstamp := as.POSIXct(paste(substr(time, 1, 10), substr(time, 12, 19)))]
# YM[(month(tstamp) <= 10 & mday(tstamp) < 7), tstamp:= tstamp + 60*60*10]
# YM[!(month(tstamp) <= 10 & mday(tstamp) < 7), tstamp:= tstamp + 60*60*11]
# YM[, time := NULL]

# YM[, intOrder:= as.integer(0:(nrow(YM)-1))]

# setkey(YM, intOrder)

# save(YM, file="~/data/ticPack/sYM.RData")
load("~/data/ticPack/sYM.RData")


YMtrade <- YM[type=='TRADE'][, TRADE := value][, value := NULL][, type:=NULL][, tstamp:= NULL]
YMbid <- YM[type=='BID_BEST'][, BID := value][, value := NULL][, type:=NULL][, tstamp:= NULL]
YMask <- YM[type=='ASK_BEST'][, ASK := value][, value := NULL][, type:=NULL][, tstamp:= NULL]

setnames(YMbid, "size", "bidSize")
setcolorder(YMbid, c('intOrder', 'BID', 'bidSize'))
setnames(YMask, "size", "askSize")
setcolorder(YMask, c('intOrder', 'ASK', 'askSize'))
setnames(YMtrade, "size", "tradeSize")
setnames(YMtrade, "TRADE", "lastTrade")
setcolorder(YMtrade, c('intOrder', 'lastTrade', 'tradeSize'))

# this sets up the time and date and other ordering variables -- including session info
YMord <- YM[ , type:=NULL][, value:=NULL][, size:=NULL]
YMord[, i.tt := as.ITime(tstamp)][, i.dd := as.IDate(tstamp)]
setcolorder(YMord, c('intOrder', 'tstamp', 'i.dd', 'i.tt'))

# name the session
YMord[i.tt %between% c(as.ITime("08:00:00"), as.ITime("11:59:59")), session := 'sydAM']
YMord[i.tt %between% c(as.ITime("12:00:00"), as.ITime("16:30:00")), session := 'sydPM']
YMord[i.tt %between% c(as.ITime("16:30:01"), as.ITime("23:59:59")), session := 'ldnAM']
YMord[i.tt %between% c(as.ITime("00:00:00"), as.ITime("07:59:59")), session := 'ldnPM']

YMord[, session := as.factor(session)]

YMord[session %in% c('sydAM', 'sydPM'), location:='syd']
YMord[session %in% c('ldnAM', 'ldnPM'), location:='ldn']

YMord[, location := as.factor(location)]

# attach the session day -- mon to fri == (2,6)
YMord[, sessDay:= wday(i.dd)]
YMord[, sessDate:= i.dd]
YMord[i.tt  %between% c(as.ITime("00:00:00"), as.ITime("07:59:59")), sessDay:= (wday(i.dd) -1L)]
YMord[i.tt  %between% c(as.ITime("00:00:00"), as.ITime("07:59:59")), sessDate:= (as.integer(i.dd -1L))]
YMord[, sessDay := as.factor(sessDay)]

YMbid_stretch <- YMbid[YMord, roll=TRUE]
YMask_stretch <- YMask[YMord, roll=TRUE]

m1 <- merge(YMbid_stretch, YMask_stretch[, c('intOrder', 'askSize', 'ASK'), with=FALSE])
m1[, mid := (BID+ASK)/2][, wMid:= (bidSize*BID+askSize*ASK)/(bidSize+askSize)]
setcolorder(m1, c("intOrder", "tstamp", "i.dd", "i.tt", "bidSize", "BID", "ASK",
                  "askSize", "mid", "wMid", "session", "sessDay", "sessDate", "location"))
t1 <- m1[YMtrade[, c('intOrder', 'lastTrade', 'tradeSize'), with=FALSE]]
t1[, side:= ifelse(lastTrade == BID, -1, ifelse(lastTrade == ASK, 1, 0))]

setnames(t1, c('lastTrade', 'tradeSize'), c('price', 'volume'))

# plots of subsets
require(ggplot2)

#0 how many traded?
volTPS <- t1[, sum(volume), by = list(i.dd, session)]
setnames(volTPS, c('i.dd', 'session', 'V1'), c('i.dd', 'session', 'volume'))

vollast4 <- tail(volTPS,4)
vvp <- qplot(volume, data=volTPS, color=session, fill=session, binwidth=2000) +
    facet_grid(session ~ .) + geom_vline(aes(xintercept = volume), vollast4)
print(vvp)

vvpT <- ggplot(volTPS, aes(x=i.dd, y=volume, fill=session)) +
    facet_grid(session~.) + theme_grey() +
    geom_bar(stat='identity')

print(vvpT)

#1 -- what was the range?
rangeTPS <- t1[, 100*(max(price) - min(price)), by = list(i.dd, session)]
setnames(rangeTPS, 'V1', 'bpsRange')

rangelast4 <- tail(rangeTPS)
rps <- qplot(bpsRange, data=rangeTPS, color=session, facets=session~., fill=session,
            geom='histogram', binwidth=1) + geom_vline(aes(xintercept = bpsRange), rangelast4)
print(rps)

rpTime <- ggplot(rangeTPS, aes(x=i.dd, y=bpsRange, fill=session)) +
    facet_grid(session~.) + theme_grey() +
    geom_bar(stat='identity')
print(rpTime)

# a function to stick the london PM session onto the initiating day
mj_futToHalves <- function(DT){
    adjSessions <- DT[, list(Open = head(price,1),
                             High = max(price),
                             Low = min(price),
                             Close = tail(price,1),
                             volume = sum(volume)),
                        by = list(i.dd, session)]

    adjLocation <- DT[, list(Open = head(price,1),
                             High = max(price),
                             Low = min(price),
                             Close = tail(price,1),
                             volume = sum(volume)),
                        by = list(i.dd, location)]

    adjLocation[, sessRet := 100*(Close - Open)]
    adjLocation[, sessRange := 100*(High - Low)]

    for (dd in unique(adjLocation$i.dd)) {
        ldnOpen <- adjSessions[i.dd == dd & session == 'ldnAM', Open]
        ldnClose <- adjSessions[i.dd == (dd + 1) & session == 'ldnPM', Close]
        ldnAMHigh <- adjSessions[i.dd == dd & session == 'ldnAM', High]
        ldnPMHigh <- adjSessions[i.dd == (dd + 1) & session == 'ldnPM', High]
        ldnAMLow <- adjSessions[i.dd == dd & session == 'ldnAM', Low]
        ldnPMLow <- adjSessions[i.dd == (dd + 1) & session == 'ldnPM', Low]

        if (length(ldnAMHigh) > 0 & length(ldnPMHigh) > 0){
            ldnMax <- max(ldnAMHigh, ldnPMHigh)
        }
        if (length(ldnAMLow) > 0 & length(ldnPMLow) > 0){
            ldnMin <- min(ldnAMLow, ldnPMLow)
        }

        if (length(ldnClose) > 0){
            adjLocation[i.dd == dd & location == 'ldn', sessRet := 100*(ldnClose - ldnOpen)]
            adjLocation[i.dd == dd & location == 'ldn', sessRange := 100*(ldnMax - ldnMin)]
            adjLocation[i.dd == dd & location == 'ldn', High := ldnMax]
            adjLocation[i.dd == dd & location == 'ldn', Low := ldnMin]
            adjLocation[i.dd == dd & location == 'ldn', Close := ldnClose]
        }
    }

    adjNoSat <- adjLocation[wday(i.dd) != 7, ]
    return(adjNoSat)

}

t1_sydLdn <- mj_futToHalves(t1)

locRngTime <- ggplot(t1_sydLdn, aes(x=i.dd, y=sessRange, fill=location)) +
    facet_grid(location~.) + theme_grey() +
    geom_bar(stat='identity')

print(locRngTime)

locRetTime <- ggplot(t1_sydLdn, aes(x=i.dd, y=sessRet, fill=location)) +
    facet_grid(location~.) + theme_grey() +
    geom_bar(stat='identity')

print(locRetTime)

#2 what traded at each price?
# this yields traded at each price per session tweak window to change wframe
wframe = 14
lastDate <- t1[, last(i.dd)]
startDate <- lastDate - wframe

sVAP <- t1[i.dd %between% c(startDate, lastDate), sum(volume), by = list(price, i.dd, session)]
setnames(sVAP, 'V1', 'volume')

vap <- ggplot(sVAP, aes(x = price, y = volume/1000, fill = as.factor(i.dd))) +
    facet_grid(session~.) +
    labs(title = 'volume traded at price', x=NULL, y="\'000 lots traded")

b <- vap + geom_bar(stat = 'identity', position = 'stack') + theme(legend.position = 'right')
print(b)

#3 agressive flow?
agFlow <- t1[, sum(volume*side), by = list(i.dd, session)]
setnames(agFlow, 'V1', 'netAgFlow')
afp <- qplot(netAgFlow, data=agFlow, color=session, facets=session~., fill=session,
             binwidth=1000, geom='histogram')
plot(afp)

afpt <- ggplot(agFlow, aes(x=i.dd, y=netAgFlow, fill=session)) +
    facet_wrap(~session) + theme_grey() +
    geom_bar(stat='identity')

print(afpt)

#4 candle sticks
canS <- t1[, list(Open = head(price,1),
                  High = max(price),
                  Low = min(price),
                  Close= tail(price,1)),
                  by = list(i.dd, session)]
canS[, dd := as.character(i.dd)]
ssx <- as.xts(canS[, list(Open, High, Low, Close)], order.by=as.Date(ss$dd))
chartSeries(ssx, type='candlesticks', up.col=3, dn.col=2, theme=chartTheme('white'))

#5 session returns
retTPS <- t1[, 100*(tail(price, 1) - head(price, 1)), by = list(i.dd, session)]
setnames(retTPS, 'V1', 'bpsRetrns')

rHp <- qplot(bpsRetrns, data=retTPS, color=session, facets=session~., fill=session,
             binwidth=1, geom='histogram')

print(rHp)

rtp <- ggplot(retTPS, aes(x=i.dd, y=bpsRetrns, fill=session)) +
    facet_wrap(~session) + theme_grey() +
    geom_bar(stat = 'identity')

plot(rtp)





## the bin ##
# subset example

library(reshape2)

.DT <- DT[,sum(volume),by = list(price,date,session)][, DATE := as.character(date)]
# reshape2 for casting to wide -- it doesn't seem to like IDate columns, hence
# the character DATE co
dcast(.DT, session + price ~ DATE, value.var = 'V1')

# end subset example

ag <- t1[, sum(tradeSize*side), by=as.IDate(tstamp)]
avP <- t1[, sum(tradeSize*lastTrade)/sum(tradeSize), by=as.IDate(tstamp)]

agp <- merge(ag, avP)
setnames(agp, c('V1.x', 'V1.y'), c('ag', 'vwap'))
agpx <- as.xts(agp[, -1, with=FALSE], as.POSIXct(agp$as.IDate))
agpx$L.ag <- lag(agpx$ag)
agpx$L2.ag <- lag(agpx$ag, 2)
agpx$dp <- diff(agpx$vwap)
mod <- lm(dp ~ 0 + ag + L.ag + L2.ag, data=agpx[-(1:2), ])

# chart 1: session returns


