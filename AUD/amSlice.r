## already done in YTslicer

# A: get the data in and split it up
# 
## required output -- 
## 1/ session returns - may need to := a session number to make session returns robust to holidays...
## 2/ session volumes @price
## 3/ session PX-ranges
## 4/ session VWAP
## 5/ session number of trades
## 6/ session give/lift

# clean house, and set environent variables
rm(list=ls())

Sys.setenv(TZ='GMT')

source('~/R/wd/MUte.r')

# load required packages
require(xts)
require(data.table)

# file and path variables
datadir <- '~/data/'
outdir <- '~/R/amrep/'
inFile <- 'ymTic.csv'

dataPath <- paste(datadir, inFile, sep="")

# get the data and set it
rawD <- read.csv(dataPath, header=TRUE, as.is=TRUE)
names(rawD)[1] <- 'timeStamp'
rawD[,1] <- as.POSIXct(rawD[,1], TZ='GMT', format="%Y-%m-%d %H:%M")

raw.dt <- data.table(rawD)

# time notes

# raw.dt[format(timeStamp, "%M") == "16", ] # subset by minute
# raw.dt[format(timeStamp, "%H") == "11", ] # subset by hour
# raw.dt[format(timeStamp, "%d") == 11,] # subset by day

# Set trading times and add session marker to the dt

StopAMHr <- "06" # the hr of the last morning trade
StopAMMin <- "59" # the morning close min
wdRestartAMHr <- "08" # the morning re-start hr
wdRestartAMMin <- "20" # the morning re-start min
wdStopPMHr <- "16" # the afternoon close hr
wdStopPMMin <- "29" # the afternoon close min
wdRestartPMHr <- "17" # the PM re-open hr
wdRestartPMMin <- "00" # the pm re-open min
ldnBreakHr <- "23"
ldnBreakMin <- "59"
sydBreakHr <- "12"
sydBreakMin <- "30"

R1 <- function() {
#  A function that does the 'dressing' of our data

raw.dt[((mjF.PXct.H(timeStamp) > wdRestartAMHr & mjF.PXct.H(timeStamp) < sydBreakHr) |
        (mjF.PXct.H(timeStamp) == wdRestartAMHr & mjF.PXct.M(timeStamp) >= wdRestartAMMin) |
        (mjF.PXct.H(timeStamp) == sydBreakHr & mjF.PXct.M(timeStamp) <=sydBreakMin)), session:='SydAM']

raw.dt[((mjF.PXct.H(timeStamp) > sydBreakHr & mjF.PXct.H(timeStamp) < wdStopPMMin) |
        (mjF.PXct.H(timeStamp) ==sydBreakHr & mjF.PXct.M(timeStamp) >=sydBreakMin) |
        (mjF.PXct.H(timeStamp) == wdStopPMHr & mjF.PXct.M(timeStamp) <= wdStopPMMin)), session:='SydPM']

raw.dt[(mjF.PXct.H(timeStamp) >= wdRestartPMHr & (mjF.PXct.H(timeStamp) <= ldnBreakHr )),
         session:='LdnAM']

raw.dt[(mjF.PXct.H(timeStamp) >= "00" & (mjF.PXct.H(timeStamp) <= StopAMHr & mjF.PXct.M(timeStamp) <= StopAMMin)),
         session:='LdnPM']

# add a column with session-day
raw.dt[session!='LdnPM', sessDay:=weekdays(timeStamp, abbreviate=TRUE)][session!='LdnPM', 
                                                                       sessDate:= format(timeStamp, '%Y-%m-%d')]

raw.dt[session=='LdnPM', 
       sessDay:=weekdays(mjF.PXct.dayadd(timeStamp, -1), abbreviate=TRUE)][session=='LdnPM', 
       sessDate:= format(mjF.PXct.dayadd(timeStamp, -1), '%Y-%m-%d')]

# later, you could also add an event column - i.e. FOMC, RBA, NFP, ISM ...

}

R1() # the dressing up...

raw.dt[Type=='TRADE', sum(Size), by=list(sessDate, Price)]

# subset data: from TRADES, minTrade, MaxTrade, wAveTrade per session
raw.dt[Type=='TRADE', list(maxT= max(Price),
                           minT= min(Price),
                           wAve= (sum(Price*Size))/sum(Size)
                           ), by=sessDate
      ]

# find the open and close prices for each session in raw.dt

raw.dt[Type=='TRADE', openT:=tail(.SD,1), by=list(session, sessDate), .SDcols='Price']

## test this logic with a simple example

timS <- timeBasedSeq('20110101 /20110401')
t.dt <- data.table(tStamp=timS, Type=gl(3, k=1, length=length(timS), labels=c('BID', 'ASK', 'TRADE')), 
                                        Price = sample(50:100, size=length(timS), replace=TRUE), 
                                        Size = sample(1:5000, size=length(timS), replace=TRUE))

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

