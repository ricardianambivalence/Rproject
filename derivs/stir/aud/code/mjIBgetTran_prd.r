
# IB adjustment and analysis
rm(list=ls()); gc()
Sys.setenv(TZ = "GMT")

## TODO -- add a section to getMeetingExp() which shows bps difference from current rate
## also, the curve problem is solved each day afresh just now -- this is a waste, you need only solve it once!
## figure out how to add to the existing dump

# load required packages
require(xts)
require(Rbbg)
require(reshape2)
require(data.table)
source("S:/Rates Research/autotools/Rhelpers/helperFunctions.r")
source("S:/Rates Research/derivs/stir/aud/code/ibFunctions.r")

# a section that loads the old frame - and then updates it
projectPATH <- "S:/Rates Research/derivs/stir/aud"
dataPATH <- file.path(projectPATH, "data")
plotPATH <- file.path(projectPATH, "pics")


# data heap
IBheapFile <- file.path(dataPATH, "IBframe.RData")

if (file.exists(IBheapFile)) {
  load(file = IBheapFile)
  ib_x <- ib_x[-nrow(ib_x),]
  start.date <- as.POSIXct(last(index(ib_x)))
} else {
  start.date <- as.POSIXct("2003-01-01")
}

today <- as.Date(as.POSIXlt(Sys.time(), tz='Australia/Sydney')) # end date is today
end.date <- today

# what we want!
securities <- c("RBACTRD Index", "IB1 Comdty", "IB2 Comdty", "IB3 Comdty", "IB4 Comdty", "IB5 Comdty", "IB6 Comdty", 
                "IB7 Comdty", "IB8 Comdty", "IB9 Comdty", "IB10 Comdty", "IB11 Comdty", "IB12 Comdty", 
                "IB13 Comdty", "IB14 Comdty", "IB15 Comdty", "IB16 Comdty", "IB17 Comdty", "IB18 Comdty")

fields <- c("PX_LAST")
midFields <- c("BID", "ASK")

# connect to bbg and get data into usable format
conn <- blpConnect()
bbg_data <- bdh(conn, securities, fields, start.date, end.date)
currentMkt <- bdp(conn, securities[-1], midFields)
currentMkt$MID <- (currentMkt[,1] + currentMkt[,2])/2

currentRBA <- bdp(conn, "RBACTRD Index", "PX_LAST")

blpDisconnect(conn) # now close the connection

# order the data
bbg_data_unstack <- unstack((bbg_data), PX_LAST~ticker)
bbg_data_unstack$date <- as.Date(unique(bbg_data$date))
bbg_data_unstack <- bbg_data_unstack[which(!is.na(bbg_data_unstack[,1])),]  

bbg_data_unstack <- dotRenamer(bbg_data_unstack)
bbg_data_unstack <- bbg_data_unstack[, c('date', 'RBACTRD', 'IB1', 'IB2', 'IB3', 'IB4', 'IB5', 'IB6', 'IB7', 'IB8', 'IB9', 'IB10', 'IB11', 
                                       'IB12', 'IB13', 'IB14', 'IB15', 'IB16', 'IB17', 'IB18')]

# stick mids together with RBA rate
midList <- as.list(c(currentRBA$PX_LAST, t(currentMkt$MID)))
mid_x <- xts(as.data.frame(midList), order.by = today)
colnames(mid_x) <- colnames(bbg_data_unstack)[-1]

# kill off today's "PX_LAST" -- edit this so it only happens until 5pm (needs testing)
sydTime <- as.POSIXlt(Sys.time(), tz="Australia/Sydney")
if (sydTime < as.POSIXlt(paste(as.character(today), "17:15:00"), tz='Australia/Sydney')){
  todayRow <- which(as.character(bbg_data_unstack$date) == as.character(today))
  if (length(todayRow)) {
    bbg_data_unstack <- bbg_data_unstack[-todayRow,]
  }
}

# newIB xts object 
newIBx <- xts(bbg_data_unstack[,-1], order.by=as.Date(bbg_data_unstack[,1]))
if(exists('ib_x')) {
  dupeRow <- which(index(newIBx) == last(index(ib_x)))
  # if NOT-zero rows of dupes, trim newIBx, if we trim all, use <- newIBx[NULL,]
  if (length(dupeRow)) {
    if (length(dupeRow) == nrow(newIBx)){
      newIBx <- newIBx[NULL,]
    } else {
      newIBx <- invisible(newIBx[-dupeRow,])
    }
  }
  ib_x <- rbind(ib_x, newIBx)
} else ib_x <- newIBx

save(ib_x, file = file.path(dataPATH, "IBframe.RData"))

# add on today's mids
if (last(index(ib_x)) != index(mid_x)) { 
  ibM_x <- rbind(ib_x, mid_x)
} else ibM_x <- ib_x

ibM_fillx <- na.locf(ibM_x)

# now all of it is a rate (ie 100 - IBprice)
ibR_x <- ibM_fillx
ibR_x[,-1] <- (100 - ibR_x[,-1])

# this bit adjusts the IBs for dates 
dateframe <- firstTuesday(index(ibM_x), numcol=(ncol(ibM_x)-2))
adjIBRates <- ibAdjusteR(ibR_x, dateframe)
lastDay <- last(index(ibR_x))
dateStrings <- xts(dateframe[[3]], order.by=index(ibR_x))

# date stuff: meeting data, days before and after
meetingDates <- as.Date(unique(melt(dateframe[[3]])[,2]))
predDates <- seq(min(meetingDates), (max(meetingDates)+1), by='day')
weekends <- as.POSIXlt(predDates)$wday %in% c(0,6) # find the weekends

# make a large empty rectangular matrix: rows all weekdays and columns the IBs
predFrameRows <- predDates[!weekends] # and cut them off
predFrameCols <- index(ibR_x)

# predFrame is a very large square matrix -- mostly NA with daily implied rates the diagonal 
predFrame <- matrix(NA, nrow = length(predFrameRows), ncol = (length(predFrameCols) + 1))
predFrame_x <- xts(predFrame, order.by=predFrameRows)
colnames(predFrame_x) <- c('rbaRate', as.character(predFrameCols))

# put the RBA observations into the first column, fill gaps, and clean up
predFrame_x[index(ibR_x),1] <- ibR_x[,1]
predFrame_x[1:5,1] <- 4.75
predFrame_x[,1] <- na.locf(predFrame_x[,1])
ds <- paste0(substr(as.character(today + 1), 1,4), substr(as.character(today + 1), 6, 7), substr(as.character(today + 1), 9, 10), "::")
predFrame_x[ds, 1] <- NA

# now match the daily IB-rates to the MOVE date -- not the meeting dates
for (i in 1:nrow(dateStrings)) {
  moveDates <- as.Date(unclass(dateStrings[i,])) + 1
  predFrame_x[moveDates,(i+1)] <- coredata(adjIBRates[i, ])
}
mdRows <- match(as.character(meetingDates), rownames(predFrame))

## {{{ chart stuff 
# how to get an RBA meeting's history
getMeetingExp("Dec-13")

spread_JulFeb14 <- -100*(getMeetingExp("Jul-13") - getMeetingExp("Feb-14"))
plot(spread_JulFeb14, las=2, main="July v. Feb 2014 OIS spread (bps)", major.format = "%b-%y")
lines(spread_JulFeb14, lwd=2)

spread_MayDec13 <- -100*(getMeetingExp("May-13") - getMeetingExp("Dec-13"))
plot(spread_MayDec13, las=2, main="May v. Dec 2013 OIS spread (bps)", major.format = "%b-%y")
lines(spread_MayDec13, lwd=2)

spread_AprSep13 <- -100*(getMeetingExp("Apr-13") - getMeetingExp("Oct-13"))
plot(spread_AprSep13, las=1, main="Apr v. Oct 2013 OIS spread (bps)")
lines(spread_AprSep13, lwd=2)

spread_MarAug13 <- 100*(getMeetingExp("Mar-13") - getMeetingExp("Aug-13"))
plot(spread_MarAug13, las=1, main="Mar v. Aug 2013 OIS spread (bps)")
lines(spread_MarAug13, lwd=2)

refrow <- match(meetingDates, rownames(predFrame))

# historical OIS step-Curve printing
#
plot(t(adjIBRates[lastDay]) ~ as.Date(t(dateStrings[lastDay])), las=2, xaxt='n', type='s', lwd=2, 
     main="Market implied RBA policy", xlab="", ylab="", ylim=c(2.25,3.25))
#
axis(1, dsFn(dateStrings, lastDay), format(dsFn(dateStrings, lastDay), "%b-%y"), las=2)
lines(t(adjIBRates[(lastDay-56L),])~ dsFn(dateStrings, lastDay, 56L), type='s', col=8)
lines(t(adjIBRates[(lastDay-28L),])~ dsFn(dateStrings, lastDay, 28L), type='s', col=6)
lines(t(adjIBRates[(lastDay-21L),])~ dsFn(dateStrings, lastDay, 21L), type='s', col=4)
lines(t(adjIBRates[(lastDay-14L),])~ dsFn(dateStrings, lastDay, 14L), type='s', col=3)
lines(t(adjIBRates[(lastDay-7L),]) ~ dsFn(dateStrings, lastDay, 7L), type='s', col=2)
lines(t(adjIBRates[(lastDay- 0L),]) ~ dsFn(dateStrings, lastDay, 0L), type='s', col=1, lwd=3)
#
legStrings <- c(
                paste(lastDay - 56L), 
                paste(lastDay - 28L), paste(lastDay - 21L), 
                paste(lastDay - 14L), paste(lastDay - 7L), paste(lastDay)
                )
legend('bottomleft', legStrings, col = c(8,6,4,3,2,1), lwd = c(1,1,1,1,1,3))

# figure out how to show a cob-web plot ... you want a plot.zoo( ... , screens=1, ...)
# with the cash rate a dark black line, and policy expectations a web of grey lines.

rbaPred <- na.locf(ib_x$RBA)
predFrame <- as.data.frame(matrix(NA, nrow(rbaPred), nrow(rbaPred)))
filledAdjIBs <- na.locf(adjIBRates)

## }}} end plotting 

