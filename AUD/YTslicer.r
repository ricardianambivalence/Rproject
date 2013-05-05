rm(list=ls())
# at this point, i use :%s/foo/bar/g to change the code
# use :#(1st line),#(last line)s/foo/bar/g to replace sections of code
# A script to generate the overnight reports for the traders
# set timezone to GMT - this is a hack due to bugs with tz = -11-11
Sys.setenv(TZ='GMT')

require(xts)
require(chron)
# require(lubridate) # this masks some POSIX functions - make sure you want it!
require(quantmod)

setwd("~/work/")
# setwd("P:/R/am/") # for work

data <- read.table("~/work/data/ym1test.txt", header=TRUE, as.is=7:8)

t <- (paste(data$date, data$time)) # paste together
tP  <- as.POSIXlt(t, format="%Y-%m-%d %H:%M:%S") # set TZ to GMT - hack around bugs w -11-11 tz.
fullxdf <- xts(data[, -(7:8)], order.by=tP) # make into an xts object

names(fullxdf) = c("ym_open", "ym_high", "ym_low", "ym_last", "ym_trades", "ym_vol")

fullxdf[,5:6][is.na(fullxdf[,5:6])] <- 0 # re-code NA to 0

# restoring the O-H-L data may help later for the tx-candle charts...

fullxdf[,4]  <- na.locf(fullxdf[,4], na.rm=TRUE) # where an observation is missing, use LOCF

# remove weekends and other non-trading sessions and subset

# 1/ set trading times

StopAMHr <- 7 # the hr of the last morning trade
StopAMMin <- 30 # the morning close min
wdRestartAMHr <- 8 # the morning re-start hr
wdRestartAMMin <- 28 # the morning re-start min
wdStopPMHr <- 16 # the afternoon close hr
wdStopPMMin <- 29 # the afternoon close min
wdRestartPMHr <- 17 # the PM re-open hr
wdRestartPMMin <- 8 # the pm re-open min

# 2/ trim no trading periods

fullxdf <- fullxdf[!(.indexwday(fullxdf)==0)] # no sundays
fullxdf <- fullxdf[!((.indexwday(fullxdf)==6 & (.indexhour(fullxdf)==StopAMHr & .indexmin(fullxdf) >= StopAMMin))
         | (.indexwday(fullxdf)==6 & .indexhour(fullxdf) > StopAMHr))] #trim sat post close
fullxdf <- fullxdf[!((.indexwday(fullxdf)==1 & (.indexhour(fullxdf)==wdRestartAMHr & .indexmin(fullxdf) < wdRestartAMMin))
         | (.indexwday(fullxdf)==1 & .indexhour(fullxdf) < wdRestartAMHr))] #trim pre Mon open
fullxdf <- fullxdf[!(.indexhour(fullxdf)==StopAMHr & .indexmin(fullxdf) > StopAMMin)] # trim after am close
fullxdf <- fullxdf[!(.indexhour(fullxdf)==wdRestartAMHr & .indexmin(fullxdf) < wdRestartAMMin)] # trim before am re-open
fullxdf <- fullxdf[!(.indexhour(fullxdf)==wdStopPMHr & .indexmin(fullxdf) > wdStopPMMin)] # trim after pm close
fullxdf <- fullxdf[!(.indexhour(fullxdf)==wdRestartPMHr & .indexmin(fullxdf) < wdRestartPMMin)] # trim before pm re-open

#xdf <- fullxdf[, -(1:3)] # trim off open, high and low
# 3/ split the sessions: lndPM (00:00 -> 0828), sydAM (0828 -> 1200), sydPM (1200 to 1630), ldnPM (1630 -> 0000)

ldnPM <- fullxdf[(.indexhour(fullxdf)>=0 & (.indexhour(fullxdf)<=StopAMHr & .indexmin(fullxdf) <= StopAMMin))]
sydAM <- fullxdf[((.indexhour(fullxdf) >= wdRestartAMHr & .indexmin(fullxdf) >= wdRestartAMMin) & .indexhour(fullxdf) <12)]
sydPM <- fullxdf[(.indexhour(fullxdf) >= 12 & (.indexhour(fullxdf) <= wdStopPMHr & .indexmin(fullxdf) <= wdStopPMMin))]
ldnAM <- fullxdf[(.indexhour(fullxdf) > wdStopPMHr & .indexhour(fullxdf) <= 23)]
syd <- fullxdf[(.indexhour(fullxdf) >= wdRestartAMHr & .indexhour(fullxdf) <= wdStopPMHr)]
ldn <- fullxdf[(.indexhour(fullxdf) >= wdRestartPMHr | .indexhour(fullxdf) <= StopAMHr)]

udates <- unique(.indexday(fullxdf)) # this outputs the unique days as a number - as.Dates(udates) gets you formatted

daySplits <- function(df=fullxdf, udates=udates){
# returns a list with the sessions split into days: syd+ldn
# have checked the gaps in the output - they are intended and right
  outlist = list(NULL)
  counter = 1
  for (i in udates){
    daylist <- NULL
    if(length(df[.indexday(df) == i & .indexhour(df) > (StopAMHr+1)] != 0)) {
      mm <- rbind(df[.indexday(df) == i & .indexhour(df) > StopAMHr],
                   df[.indexday(df) == (i+1) & .indexhour(df) < StopAMHr])

      # you should be able to use aggregate here!

      priceList <- sort(unique(mm[,4]))

      PxMat <- matrix(c(priceList, rep(NA, length(priceList))), ncol=2)
      cc = 1
      for (px in priceList){
        PxMat[cc,2] <- sum(mm[mm[,4] == px][,6])
        cc = cc +1
      }
      OHLC <- data.frame(marks=c('open', 'high', 'low', 'close'), prices=rep(NA,4))
      firstOpen <- head(which(!is.na(mm[,1])),1) # locate first not-NA openPX
      lastClose <- tail(which(!is.na(mm[,1])),1) # locate last non-NA lastPX
      OHLC[1,2] <- mm[firstOpen,1]
      OHLC[2,2] <- max(mm[,2], na.rm=TRUE)
      OHLC[3,2] <- min(mm[,2], na.rm=TRUE)
      OHLC[4,2] <- mm[lastClose,4]

      daylist <- list(PxMat, OHLC)
  }
  outlist[[counter]] <- daylist
  counter = counter + 1
  }
  cc <- 1
  finalList <- NULL
  ll <- length(outlist)
  for (l in 1:ll){
    if (length(outlist[[l]]) != 0){
      datestring <- as.character(as.Date(udates[cc]))
      finalList[[datestring]] <- outlist[[l]]
      cc = cc + 1
    }
  }
  return(finalList)
}

splitter <- daySplits(fullxdf, udates=udates)

plot5dStack <- function(df, ssns=5){
  len <- length(df) # add a test that rejects a df < 5 and stops.
  store <- NULL
  for (i  in ((len-ssns+1):len)){
    store <- c(store, df[[i]][[1]][,1])
  }
  plotValues <- sort(unique(store))
  preM <- as.data.frame(matrix(NA, ncol=ssns, nrow=length(plotValues)))
  preDF <- cbind(preM, plotValues)
  counter = 1
  for (px in plotValues){
    for (i in ((len-ssns+1):len)){
      cc <- 0
      if (length(df[[i]][[1]][df[[i]][[1]][,1] == px][2]) != 0) {
        preDF[counter, (i-len+ssns)] <- df[[i]][[1]][df[[i]][[1]][,1] == px][2]}
      else
        preDF[counter, (i-len+ssns)] <- 0
                   }
    counter = counter + 1
  }
  preDF[is.na(preDF)] <- 0
  return(preDF)
}

pp <- plot5dStack(splitter, ssns=5)
ppm <- as.matrix(pp)
barplot(t(ppm[,-6]), names.arg=ppm[,6], las=2, col=c(2,3,4,5,6))

# note that xts has period apply functions that will help you do what you'd like
# to do with the time splitting of the data sets - min, max, mean, sum etc ...

# Analytics:

# Session returns - what was the px change across a session?

# 1/ find first and last sessions

mj_SessionID <- function(x){
  session <- NULL
  if (.indexhour(x) >= 0 & .indexhour(x) < wdRestartAMHr) session <- 'ldnPM'
  else if (.indexhour(x) >= wdRestartAMHr & .indexhour(x) < 12) session <- 'sydAM'
  else if (.indexhour(x) >= 12 & .indexhour(x) <= wdStopPMHr) session <- 'sydPM'
  else if (.indexhour(x) > wdStopPMHr & .indexhour(x) <= 23) session <- 'ldnAM'
  return (session)
}

f1 <- head(fullxdf,1)
l1 <- tail(fullxdf,1)

f1_session <- mj_SessionID(f1)
l1_session <- mj_SessionID(l1)


mj_dateTail <- function(frame, date){
  price <- NA
  if (length(frame[(.indexday(frame) == date)]) !=0)
    price <- coredata(tail(frame[(.indexday(frame) == date)],1)[,1])
  return(price)
}

mj_dateTrades <- function(frame, date){
  trades <- NA
  if (length(frame[(.indexday(frame) == date)]) !=0)
    trades <- sum(frame[(.indexday(frame) == date)][,2])
  return(trades)
}

mj_dateVolms <- function(frame, date){
  volms <- NA
  if (length(frame[(.indexday(frame) == date)]) !=0)
    volms <- sum(frame[(.indexday(frame) == date)][,3])
  return(volms)
}

mj_datePxRange <- function(frame, date, lpm=0){
  PxRange <- NA
  if (lpm) {
    if (length(frame[(.indexday(frame) == date)]) != 0 &
        length(frame[.indexday(frame) == (date+1)]) != 0) {
      maxpx <- max(frame[.indexday(frame) == date][,1], frame[.indexday(frame)
                   == (date+1)][,1])
      minpx <- min(frame[.indexday(frame) == date][,1], frame[.indexday(frame)
                   == (date+1)][,1])
      PxRange <- maxpx - minpx
    }
  }
  else if (length(frame[(.indexday(frame) == date)]) !=0)
    PxRange <- max(frame[(.indexday(frame) == date)][,1]) - min(frame[(.indexday(frame) == date)][,1])
  return(PxRange)
}

mj_dateSesPx <- function(frame, date, lpm=0){
  priceList <- NA
  if (lpm) {
    if (length(frame[(.indexday(frame) == date)]) !=0 &
      length(frame[(.indexday(frame) == date+1)]) !=0) {
      ldnAM_priceList <- sort(unique(frame[.indexday(frame) == date &
                                     .indexhour(frame) >= wdRestartPMHr][,1]))
      ldnPM_priceList <- sort(unique(frame[.indexday(frame) == (date+1) &
                                     .indexhour(frame) <= StopAMHr][,1]))
      priceList <- sort(unique(c(ldnAM_priceList, ldnPM_priceList)))
    }
    PxMat <- matrix(c(priceList, rep(NA, length(priceList))), ncol=2)
    cc = 1
    for (px in priceList){
      ldnAM <- sum(frame[.indexday(frame) == date & frame[,1] == px][,3])
      ldnPM <- sum(frame[.indexday(frame) == (date+1) & frame[,1] == px][,3])
      PxMat[cc,2] <- ldnAM + ldnPM
    }
  }
  if (length(frame[(.indexday(frame) == date)]) !=0)
    priceList <- sort(unique(frame[.indexday(frame) == date][,1]))
  PxMat <- matrix(c(priceList, rep(NA, length(priceList))), ncol=2)
  cc = 1
  for (px in priceList){
    PxMat[cc,2] <- sum(frame[frame[,1] == px & .indexday(frame) == date][,3])
    cc = cc+1
  }
  return(PxMat)
}




mj_SessionReturns <- function(a, b, c, d, udates){
  sydAM_rets <- sydPM_rets <- ldnAM_rets <- ldnPM_rets <- syd_rets <- ldn_rets <-
  data.frame(day = as.Date(udates), rets = rep(0, length(udates)))
  counter = 1
  for (i in udates) {
    lastSydAM <- mj_dateTail(a[,-(1:3)], date=i)
    lastSydPM <- mj_dateTail(b[,-(1:3)], date=i)
    lastLdnAM <- mj_dateTail(c[,-(1:3)],date=i)
    prior_lastLdnAM <- mj_dateTail(c[,-(1:3)], date=(i-1))
    lastLdnPM <- mj_dateTail(d[,-(1:3)], date=i)

    sydAM_rets[counter,2] <- lastSydAM - lastLdnPM
    sydPM_rets[counter,2] <- lastSydPM - lastSydAM
    ldnAM_rets[counter,2] <- lastLdnAM - lastSydPM
    ldnPM_rets[counter,2] <- lastLdnPM - prior_lastLdnAM
    syd_rets[counter,2] <- sydAM_rets[counter,2] + sydPM_rets[counter,2]
    ldn_rets <- ldnAM_rets[counter,2] + ldnPM_rets[counter,2]
    counter <- counter + 1

    # need to make it a little more complex, to account for the prior session being 'hard to find'
  }
  outlist <- list(sydAM_rets = sydAM_rets, sydPM_rets = sydPM_rets, ldnAM_rets =
                  ldnAM_rets, ldnPM_rets = ldnPM_rets, syd_rets = syd_rets,
                  ldn_rets = ldn_rets)
  return(outlist)
}

rrr <- mj_SessionReturns(sydAM, sydPM, ldnAM, ldnPM, udates)

mj_sessionTrades <- function(a=sydAM, b=sydPM, c=ldnAM, d=ldnPM, udates){
  sydAM_trades <- sydPM_trades <- ldnAM_trades <- ldnPM_trades <- syd_trades <-
    ldn_trades <- data.frame(day = as.Date(udates), vol= rep(0, length(udates)))
  counter = 1
  for (i in udates){
    sydAM_trades[counter,2] <- mj_dateTrades(a[,-(1:3)], date=i)
    sydPM_trades[counter,2] <- mj_dateTrades(b[,-(1:3)], date=i)
    ldnAM_trades[counter,2] <- mj_dateTrades(c[,-(1:3)], date=i)
    ldnPM_trades[counter,2] <- mj_dateTrades(d[,-(1:3)], date=i)
    syd_trades[counter,2] <- sydAM_trades[counter,2] + sydPM_trades[counter,2]
    ldn_trades[counter,2] <- ldnAM_trades[counter,2] + mj_dateTrades(d[,-(1:3)], date= (i+1))
    counter <- counter + 1
  }
  outlist  <- list(sydAM_trades = sydAM_trades, sydPM_trades = sydPM_trades,
                   ldnAM_trades = ldnAM_trades, ldnPM_trades = ldnPM_trades,
                   syd_trades = syd_trades, ldn_trades = ldn_trades)
  return(outlist)
}

mj_sessionVolumes <- function(a=sydAM, b=sydPM, c=ldnAM, d=ldnPM, udates){
sydAM_volms <- sydPM_volms <- ldnAM_volms <- ldnPM_volms <- syd_volms <-
  ldn_volms <- data.frame(day = as.Date(udates), vol= rep(0, length(udates)))
  counter = 1
  for (i in udates){
    sydAM_volms[counter,2] <- mj_dateVolms(a[,-(1:3)], date=i)
    sydPM_volms[counter,2] <- mj_dateVolms(b[,-(1:3)], date=i)
    ldnAM_volms[counter,2] <- mj_dateVolms(c[,-(1:3)], date=i)
    ldnPM_volms[counter,2] <- mj_dateVolms(d[,-(1:3)], date=i)
    syd_volms[counter,2] <- sydAM_volms[counter,2] + sydPM_volms[counter,2]
    ldn_volms[counter,2] <- mj_dateVolms(c[,-(1:3)], date=i) + mj_dateVolms(d[,-(1:3)], date=(i+1))
    counter <- counter + 1
  }
  outlist  <- list(sydAM_volms = sydAM_volms, sydPM_volms = sydPM_volms,
                   ldnAM_volms = ldnAM_volms, ldnPM_volms = ldnPM_volms,
                   syd_volms = syd_volms, ldn_volms=ldn_volms)
  return(outlist)
}

mj_sessionPxRange <- function(a, b, c, d, e, f, udates){
  sydAM_PxRange <- sydPM_PxRange <- ldnAM_PxRange <- ldnPM_PxRange <-
    syd_PxRange  <- ldn_PxRange <- data.frame(day = as.Date(udates), vol= rep(0, length(udates)))
  counter = 1
  for (i in udates){
    sydAM_PxRange[counter,2] <- mj_datePxRange(a[,-(1:3)], date=i)
    sydPM_PxRange[counter,2] <- mj_datePxRange(b[,-(1:3)], date=i)
    ldnAM_PxRange[counter,2] <- mj_datePxRange(c[,-(1:3)], date=i)
    ldnPM_PxRange[counter,2] <- mj_datePxRange(d[,-(1:3)], date=i)
    syd_PxRange[counter,2] <- mj_datePxRange(e[,-(1:3)], date=i)
    ldn_PxRange[counter,2] <- mj_datePxRange(f[,-(1:3)], date=i, lpm=1)
    counter <- counter + 1
  }
  outlist  <- list(sydAM_PxRange = sydAM_PxRange, sydPM_PxRange = sydPM_PxRange,
                   ldnAM_PxRange = ldnAM_PxRange, ldnPM_PxRange = ldnPM_PxRange,
                   syd_PxRange = syd_PxRange, ldn_PxRange = ldn_PxRange)
  return(outlist)
}

mj_sessionVolAtPx <- function(a, b, c, d, e, f, udates){
  sydAM_VolAtPx <- sydPM_VolAtPx <- ldnAM_VolAtPx <- ldnPM_VolAtPx <-
    syd_VolAtPx <- ldn_VolAtPx <- all_VolAtPx <- list(NULL)
  counter = 1
  for (i in udates){
    sydAM_VolAtPx[[counter]] <- mj_dateSesPx(a[,-(1:3)], i)
    sydPM_VolAtPx[[counter]] <- mj_dateSesPx(b[,-(1:3)], i)
    ldnAM_VolAtPx[[counter]] <- mj_dateSesPx(c[,-(1:3)], i)
    ldnPM_VolAtPx[[counter]] <- mj_dateSesPx(d[,-(1:3)], i)
    syd_VolAtPx[[counter]] <- mj_dateSesPx(e[,-(1:3)], i)
    ldn_VolAtPx[[counter]] <- mj_dateSesPx(f[,-(1:3)], i, lpm=1)
    counter = counter + 1
  }
  outlist  <- list(sydAM_VolAtPx = sydAM_VolAtPx, sydPM_VolAtPx = sydPM_VolAtPx,
                   ldnAM_VolAtPx = ldnAM_VolAtPx, ldnPM_VolAtPx = ldnPM_VolAtPx,
                   syd_VolAtPx = syd_VolAtPx, ldn_VolAtPx = ldn_VolAtPx)
  return(outlist)
}

ssnVolAtPx <- mj_sessionVolAtPx(sydAM, sydPM, ldnAM, ldnPM, syd, ldn, udates)
# note that this is a multi-dim array (sort of)
# for example, call 1st date of sydAm set via: ssnVolAtPx$sydAM_VolAtPx[[1]]

barplot(ssnVolAtPx$sydAM_VolAtPx[[1]][,2],
        names.arg=ssnVolAtPx$sydAM_VolAtPx[[1]][,1], col=2, las=1,
        main="YM1 volume traded at each price: Syd AM session", xlab="",
        ylab="")

# TODO: a hist function that shows the final 5 days in 5 col




ssnTrades <- mj_sessionTrades(sydAM, sydPM, ldnAM, ldnPM, udates)
ssnVolumes <- mj_sessionVolumes(sydAM, sydPM, ldnAM, ldnPM, udates)
ssnPxRanges <- mj_sessionPxRange(sydAM, sydPM, ldnAM, ldnPM, syd, ldn, udates)

barplot(ssnTrades[[1]]$vol, names.arg=ssnTrades[[1]]$day, col=2, las=1,
        main="YM1 no. of trades: Syd AM session", xlab="", ylab="")

barplot(ssnVolumes[[1]]$vol, names.arg=ssnVolumes[[1]]$day, col=2, las=1,
         main="YM1 Volume traded: Syd AM Session", xlab="", ylab="")

barplot(ssnVolumes[[1]]$vol / ssnTrades[[1]]$vol, names.arg=ssnTrades[[1]]$day,
        col=2, las=1, main="YM1 Ave volume per trade: Syd AM session", xlab="",
        ylab="")

barplot(ssnPxRanges[[1]]$vol, names.arg=ssnPxRanges[[1]]$day, col=2, las=1,
        main="YM1 Px Range: Syd AM session", xlab="", ylab="")


# ideas for charts:
# 1/ session returns - bar and hist
# 2/ volume traded - bar and hist ... volume at each price? ie. sum(sydAM[sydAM$ym_last==96.37][,2])
# 3/ price range - unique(sydAM$ym_last) gets you a list of each of the prices that trades
# 4/ volume ratios between ib, ir, ym, xm, ty
# 5/ week in review bar-chart, with days coloured - bar height = number of lots traded
# 6/ overnight in review - split the session into four parts, and color-bars
# MJ ideal chart - a 4-split chart of candles with volumes below.

# DH: price range across each session
# volume at each price traded
# total volumes traded -
# ratios of ym to xm -- get some idea of what the flows are
# possibly ratios of AU 10yrs to US 10yrs ...
# something about overnight options ...

# the dated stacked bars chart - a one week version with 5 colours

# the dated stacked bars chart - a four week version with 4 colours

# the times stacked bars chart - an o/n version with 4 colours made at syd open

# the times stacked bars chart - a close version with 4 colours made syd close

pdf("pics/ym1returns_828to12.pdf")
b828 <- barplot(rrr[[1]]$rets, names.arg=rrr[[1]]$day, las=1, main="ym returns: 08:28 -> 12:00", col=2, las=1)
u828 <- rep(median(rrr[[1]]$rets, na.rm=TRUE), length(b828))
lines(x=b828, u828, col=6, lwd=3)
dev.off()

hist(rrr[[1]]$rets, col=2, main="Syd AM Session: returns", xlab="", ylab="", las=1,
     breaks= (max(rrr[[1]]$rets, na.rm=TRUE) - min(rrr[[1]]$rets, na.rm=TRUE))*100 )
abline(v=median(rrr[[1]]$rets, na.rm=TRUE), col=1, lwd=4)

pdf("pics/ym1returns_12to1630.pdf")
b828 <- barplot(rrr[[2]]$rets, names.arg=rrr[[2]]$day, las=1, main="ym returns: 12:00 -> 16:30", col=3, las=1)
u828 <- rep(median(rrr[[2]]$rets, na.rm=TRUE), length(b828))
lines(x=b828, u828, col=6, lwd=3)
dev.off()

hist(rrr[[2]]$rets, col=3, main="Syd PM Session: returns", xlab="", ylab="", las=1,
     breaks= (max(rrr[[2]]$rets, na.rm=TRUE) - min(rrr[[2]]$rets, na.rm=TRUE))*100 )
abline(v=median(rrr[[2]]$rets, na.rm=TRUE), col=1, lwd=4)

pdf("pics/ym1returns_1630to00.pdf")
b828 <- barplot(rrr[[3]]$rets, names.arg=rrr[[3]]$day, las=1, main="ym returns: 16:30 -> 00:00", col=4, las=1)
u828 <- rep(median(rrr[[3]]$rets, na.rm=TRUE), length(b828))
lines(x=b828, u828, col=6, lwd=3)
dev.off()

hist(rrr[[3]]$rets, col=4, main="ldn AM Session: returns", xlab="", ylab="", las=1,
     breaks= (max(rrr[[3]]$rets, na.rm=TRUE) - min(rrr[[2]]$rets, na.rm=TRUE))*100 )
abline(v=median(rrr[[3]]$rets, na.rm=TRUE), col=1, lwd=4)

pdf("pics/ym1returns_00to828.pdf")
b828 <- barplot(rrr[[4]]$rets, names.arg=rrr[[4]]$day, las=1, main="ym returns: 00:00 -> 08:28", col=1, las=1)
u828 <- rep(median(rrr[[4]]$rets, na.rm=TRUE), length(b828))
lines(x=b828, u828, col=6, lwd=3)
dev.off()

hist(rrr[[4]]$rets, main="ldn PM Session: returns", xlab="", ylab="", las=1,
     breaks= (max(rrr[[4]]$rets, na.rm=TRUE) - min(rrr[[2]]$rets, na.rm=TRUE))*100 )
abline(v=median(rrr[[4]]$rets, na.rm=TRUE), col=1, lwd=4)


retDF <- data.frame(rrr[[1]]$ret, rrr[[2]]$ret, rrr[[3]]$ret, rrr[[4]]$ret)
retDF[is.na(retDF)] <- 0
retMtx <- as.matrix(retDF)

# stacked bar chart - fix legend so that it's outside the chart
pdf("pics/ym1_stackedbar.pdf")
barplot(t(retMtx), col=c(1,2,3,4), las=1, main="ym1 returns by period")
legend(x="bottom",legend= c("SydAM","SydPM", "LdnAM", "LdnPM"),fill=c(2,3,4,1),horiz=T, box.lty=0)
dev.off()

# grouped bar chart - fix legend
pdf("pics/ym1_groupedbar.pdf")
yMin <- min(retMtx, na.rm=TRUE)*1.25
yMax <- max(retMtx, na.rm=TRUE)*1.25
barplot(t(retMtx), col=c(1,2,3,4), las=1, main="ym1 returns by period", ylim=c(yMin, yMax), beside=TRUE)
legend(x="top",legend= c("SydAM","SydPM", "LdnAM","LdnPM"),fill=c(2,3,4,1),horiz=T, box.lty=0)
dev.off()

# plot by color

mk <- merge(sydAM$ym_last, sydPM$ym_last, ldnAM$ym_last, ldnPM$ym_last)

pdf("pics/ym1shadyPX.pdf")
yMin <- min(fullxdf$ym_last)
yMax <- max(fullxdf$ym_last)
plot(index(mk), as.zoo(mk[,1]), type='l', col=2, ylim=c(yMin,yMax), xlab="", ylab="", main="ym1", las=1)
lines(index(mk), as.zoo(mk[,2]), type='l', col=3)
lines(index(mk), as.zoo(mk[,3]), type='l', col=4)
lines(index(mk), as.zoo(mk[,4]), type='l', col=1)
legend(x="top",legend= c("SydAM","SydPM", "LdnAM", "LdnPM"),fill=c(2,3,4,1),horiz=T, box.lty=0)
dev.off()
