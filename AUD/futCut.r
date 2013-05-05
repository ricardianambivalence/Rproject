rm(list=ls())
Sys.setenv(TZ='GMT')

require(xts)
require(plyr)

setwd("~/work/")

## library functions

mj_firstNew <- function(oldDF, newDF){
  outDF <- NULL
  max <- nrow(newDF)
  k <- 1
  while (k <= max){
    if (nrow(match_df(oldDF, newDF[k,])) == 0){
      outDF <- rbind(oldDF, newDF[k:max, ])
      k <- max + 1
    }
    else {
      k <- k +1
    }
  }
  return(outDF)
}

## this bit at the top is how i created the initial history file

dat1 <- read.csv("~/data/firstFut.csv", header=TRUE, as.is=TRUE)
ydf <- dat1[which(!is.na(dat1[,2])),c(1:3)]

ydfx <- xts(ydf[,c(2,3)], order.by=as.POSIXlt(ydf[,1], tz="GMT", format="%d/%m/%y %H:%M"))

YM1hist <- ydfx
save(YM1hist, file="YM1hist.RData")

xdf <- dat1[which(!is.na(dat1[,5])), c(4:6)]
xdfx <- xts(xdf[,c(2,3)], order.by=as.POSIXlt(xdf[,1], tz='GMT',
                                              format="%d/%m/%y %H:%M"))
XM1hist <- xdfx
save(XM1hist, file="XM1hist.RData")

tdf <- dat1[which(!is.na(dat1[,8])), c(7:9)]
tdfx <- xts(tdf[,c(2,3)], order.by=as.POSIXlt(tdf[,1], tz='GMT',
                                              format="%d/%m/%y %H:%M"))
TY1hist <- tdfx
save(TY1hist, file="TY1hist.RData")

irdf <- dat1[which(!is.na(dat1[,11])), c(10:12)]
irdfx <- xts(irdf[,c(2,3)], order.by=as.POSIXlt(irdf[,1], tz='GMT',
                                              format="%d/%m/%y %H:%M"))
IR1hist <- irdfx
save(IR1hist, file="IR1hist.RData")

ibdf <- dat1[which(!is.na(dat1[,14])), c(13:15)]
ibdfx <- xts(ibdf[,c(2,3)], order.by=as.POSIXlt(ibdf[,1], tz='GMT',
                                              format="%d/%m/%y %H:%M"))
IB1hist <- ibdfx
save(IB1hist, file="IB1hist.RData")
## this section is where you'd start in practice

# 1/ load the existing data

load("YM1hist.RData")
load("XM1hist.RData")
load("TY1hist.RData")
load("IR1hist.RData")
load("IB1hist.RData")


# 2/ load the new data and convert to xts format and clean up

newdat <- read.csv("~/data/secondFut.csv", header=TRUE, as.is=TRUE)

# YM1
yndf <- newdat[which(!is.na(newdat[,2])),c(1:3)]
ydfp <- data.frame(timeStamp=as.POSIXlt(yndf[,1], tz="GMT", format="%d/%m/%y %H:%M"),
                      px_ym1=yndf[,2], vol_ym1=yndf[,3])

YMhistDF <- data.frame(timeStamp = index(YM1hist), coredata(YM1hist))
addYMDF <- mj_firstNew(YMhistDF, ydfp)

YM1hist <- xts(addYMDF[,-1], order.by=addYMDF[,1])

save(YM1hist, file="YM1hist.RData")

# XM1
xndf <- newdat[which(!is.na(newdat[,5])), c(4:6)]
xdfp <- data.frame(timeStamp=as.POSIXlt(xndf[,1], tz="GMT", format="%d/%m/%y %H:%M"),
                      px_xm1=xndf[,2], vol_xm1=xndf[,3])

XMhistDF <- data.frame(timeStamp = index(XM1hist), coredata(XM1hist))
addXMDF <- mj_firstNew(XMhistDF, xdfp)

XM1hist <- xts(addXMDF[,-1], order.by=addXMDF[,1])

save(XM1hist, file="XM1hist.RData")

# TY1
tyndf <- newdat[which(!is.na(newdat[,8])), c(7:9)]
tdfp <- data.frame(timeStamp=as.POSIXlt(tyndf[,1], tz="GMT", format="%d/%m/%y %H:%M"),
                      px_ty1=tyndf[,2], vol_ty1=tyndf[,3])

TYhistDF <- data.frame(timeStamp = index(TY1hist), coredata(TY1hist))
addTYDF <- mj_firstNew(TYhistDF, tdfp)

TY1hist <- xts(addTYDF[,-1], order.by=addTYDF[,1])

save(TY1hist, file="TY1hist.RData")

# IR1
irndf <- newdat[which(!is.na(newdat[,11])), c(10:12)]
irdfp <- data.frame(timeStamp=as.POSIXlt(irndf[,1], tz="GMT", format="%d/%m/%y %H:%M"),
                      px_ir1=irndf[,2], vol_ir1=irndf[,3])

IRhistDF <- data.frame(timeStamp = index(IR1hist), coredata(IR1hist))
addIRDF <- mj_firstNew(IRhistDF, irdfp)

IR1hist <- xts(addIRDF[,-1], order.by=addIRDF[,1])

save(IR1hist, file="IR1hist.RData")

# IB1
ibndf <- newdat[which(!is.na(newdat[,14])), c(13:15)]
ibdfp <- data.frame(timeStamp=as.POSIXlt(ibndf[,1], tz="GMT", format="%d/%m/%y %H:%M"),
                      px_ib1=ibndf[,2], vol_ib1=ibndf[,3])

IBhistDF <- data.frame(timeStamp = index(IB1hist), coredata(IB1hist))
addIBDF <- mj_firstNew(IBhistDF, ibdfp)

IB1hist <- xts(addIBDF[,-1], order.by=addIBDF[,1])

save(IB1hist, file="IB1hist.RData")

rm(addYMDF, YMhistDF, ydfp)

rm(newdat, yndf)

# 3/ append the new data -- so step 1 is checking if it's new

# convert to a df so you can use match_df

# now spec the DF up to max possible size, and then add the rows as they pass
# this is stupidly slow in R - use C ... also without finer grained time, it's going to
# drop some apparently duplicated observations ... even use of seconds might not solve this!
# try the package fmatch - as it has very fast matching for repeated cases.
#

# mj_checkAllNew <- function(oldDF, newDF){
#   oldrow <- nrow(oldDF)
#   newrow <- nrow(newDF)
#   outDF <- data.frame(time_ym1 = c(oldDF[,1], rep(NA, nrow(newDF))), px_ym1 =
#                       c(oldDF[,2], rep(NA, nrow(newDF))), vol_ym1 =
#                       c(oldDF[,3], rep(NA, nrow(newDF))))
#   nextrow <- oldrow +1
#   i <- 1
#   while (i <= newrow){
#     if (!nrow(match_df(outDF, newDF[i,]))){
#       outDF[nextrow,] <- newDF[i,]
#       nextrow = nextrow + 1
#     }
#     i = i + 1
#   }
#   return(outDF)
# }
#
# addDF <- mj_checkAllNew(YMhistDF, ydf)

# given that the data is stricly ordered in time, wouldn't it do to find the
# first non-overlapping observation? do we need to check them all??



# 3/ now order the data by date & session

# 3.1/ set trading times

StopAMHr <- 7 # the hr of the last morning trade
StopAMMin <- 30 # the morning close min
wdRestartAMHr <- 8 # the morning re-start hr
wdRestartAMMin <- 28 # the morning re-start min
wdStopPMHr <- 16 # the afternoon close hr
wdStopPMMin <- 29 # the afternoon close min
wdRestartPMHr <- 17 # the PM re-open hr
wdRestartPMMin <- 8 # the pm re-open min

# 3.2/ get the unique dates

udates <- unique(.indexday(fullxdf)) # this outputs the unique days as a number - as.Dates(udates) gets you formatted

# 3.3/ split the data up by session

daySplits <- function(df=YM1hist, udates=udates){
# returns a list with the sessions split into days: syd+ldn
# have checked the gaps in the output - they are intended and right
  outlist = list(NULL)
  counter = 1
  for (i in udates){
    daylist <- NULL
    if(length(df[.indexday(df) == i & .indexhour(df) > (StopAMHr+1)] != 0)) {
      mm <- rbind(df[.indexday(df) == i & .indexhour(df) > StopAMHr],
                   df[.indexday(df) == (i+1) & .indexhour(df) < StopAMHr])

      # TODO: replace the name with a templated version so it ops w xm/ty etc...
      PxMat <- aggregate(.~px_ym1, data=mm, sum)

      OHLC <- data.frame(marks=c('open', 'high', 'low', 'close'), prices=rep(NA,4))
      firstOpen <- head(which(!is.na(mm[,1])),1) # locate first not-NA openPX
      lastClose <- tail(which(!is.na(mm[,1])),1) # locate last non-NA lastPX
      OHLC[1,2] <- mm[firstOpen,1]
      OHLC[2,2] <- max(mm[,1], na.rm=TRUE)
      OHLC[3,2] <- min(mm[,1], na.rm=TRUE)
      OHLC[4,2] <- mm[lastClose,1]

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

# this command will return the volume traded at each price in a xts data-frame
adf <- aggregate(.~px_ym1, data=YM1hist, sum)

# and you plot it as follows ...
barplot(adf[,2], names.arg=adf[,1], las=2)

# note, you are going to need seconds data to identify the observations - for df
# building up over time.
