rm(list=ls())
Sys.setenv(TZ='GMT')

# load required packages
require(xts)
require(Rbbg)

##timestamps is the tricky bit. enviro is GMT and bberg has to use UTC
##GMT is 11 hours behind Sydney, so 08:30 on 2 Nov in SYD is 21:30 on 1 Nov in GMT and 21:30 on 1 Nov in UTC

sydstart <- paste(as.Date(Sys.Date()-1),"21:30:00.000")
sydend <- sydstart + (22.5*60*60) #22.5 hours of trading time - includes previous day trades & o/night

####required functions

##extract the UTC times from the very strange bbg format, and convert that back to sydney time.
al_rbbgtotimes <- function(x){
  y <- strptime(paste(substr(x$time, 1,10),substr(x$time, 12,19)), format="%Y-%m-%d  %H:%M:%S")
  y <- y + 11*60*60
  return(y)
}

##make xts object - pull in the prices and trading size
al_bbgtoxts <- function(x){
  minutes <- al_rbbgtotimes(x)
  xtsobj <- xts(x[,3:4],order.by=as.POSIXct(minutes))
  return(xtsobj)
}

##graph ticks with cutoff times
##function should be called as follows al_marketgraph(xts_object, "security name", optional y axis (default="price"), optional size=1)
##if you want a pdf, call al_marketgraph_pdf(xts_object, "security name", optional y axis (default="price"), optional size=1)

##note to self- may want to combine these two into one function where a flag "save=1" is called to save as a pdf object

al_marketgraph <- function(x,sec_name,yaxis_name="price",size=0){
  onstart_time <- as.POSIXct(paste(as.Date(index(x[1]))," 16:30:00"))
  onend_time <- onstart_time + (14*60*60)
  if (size==1){
    size_day <- round(sum(x[,2][paste("/",onstart_time,sep="")]/1000),0)
    size_night <- round(sum(x[,2][paste(onstart_time,"/")]/1000),0)
    plot(x[,1], type='l', ylab=yaxis_name, main=sprintf("%s :: %sK/%sK traded day/night", sec_name, size_day, size_night))   
  } else {
    plot(x[,1], type='l', ylab=yaxis_name, main=sprintf("%s ", sec_name))   
  }
  abline(v=index(x[1]), col="red", lwd=2,lty=2)
  abline(v=onstart_time, col="red", lwd=2,lty=2)
  abline(v=onend_time, col="red", lwd=2,lty=2)
  text(c(index(x[1]),onstart_time),c(max(x[,1])+0.0015,max(x[,1])+0.0015),c("Sydney session","London/New York session"),col=c("red","red"),pos=c(4,4))
}

al_marketgraph_pdf <- function(x,sec_name,yaxis_name="price",size=0){
  pdf(sprintf("S:/Rates Research/chartpacks/amwrap/pics/%s.pdf", sec_name)) 
  onstart_time <- as.POSIXct(paste(as.Date(index(x[1]))," 16:30:00"))
  onend_time <- onstart_time + (14*60*60)
  if (size==1){
    size_day <- round(sum(x[,2][paste("/",onstart_time,sep="")]/1000),0)
    size_night <- round(sum(x[,2][paste(onstart_time,"/")]/1000),0)
    plot(x[,1], type='l', ylab=yaxis_name, main=sprintf("%s :: %sK/%sK traded day/night", sec_name, size_day, size_night))   
  } else {
    plot(x[,1], type='l', ylab=yaxis_name, main=sprintf("%s ", sec_name))   
  }
  abline(v=index(x[1]), col="red", lwd=2,lty=2)
  abline(v=onstart_time, col="red", lwd=2,lty=2)
  abline(v=onend_time, col="red", lwd=2,lty=2)
  text(c(index(x[1]),onstart_time),c(max(x[,1])+0.0015,max(x[,1])+0.0015),c("Sydney session","London/New York session"),col=c("red","red"),pos=c(4,4))
  dev.off()
}

####establish connection to bbg

conn <- blpConnect(log.level = "finest")

#####edit beyond here to get more securities

##ticks

#bond
ym <- tick(conn, "YM1 Comdty","TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
xm <- tick(conn, "XM1 Comdty", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
xmym <- tick(conn, "XMYM Comdty", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
us22 <- tick(conn, "USGG10YR Index", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
spain22 <- tick(conn, "GSPG10YR Index", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
germany22 <-  tick(conn, "GDBR10 Index", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

#stir
IR1 <- tick(conn, "IR1 Comdty", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
IR4 <- tick(conn, "IR5 Comdty", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
ED4 <- tick(conn, "ED4 Comdty",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
ADSO1 <- tick(conn, "ADSO1 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
USSO3 <- tick(conn, "USSO3 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

#swaps
ADSW3Q <- tick(conn, "ADSW3 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
ADSW10 <- tick(conn, "ADSW10 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
ADSF3Q <- tick(conn, "ADSF3Q Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
ADSF10Q <- tick(conn, "ADSF10 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
NDSW3 <- tick(conn, "NDSW3 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
NDSW10 <- tick(conn, "NDSW10 Curncy",  "TRADE",  paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

#inflation
US10YBEI <- tick(conn, "USGGBE10 Index", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

#credit

#equities
sandp_us <- tick(conn, "ESA Index", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

#currency
audusd <- tick(conn, "AUDUSD Curncy", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))
audnzd <- tick(conn, "AUDNZD Curncy", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

#volatility
vix <- tick(conn, "VIX Index", "TRADE", paste(as.character(sydstart),".000",sep=""), paste(as.character(sydend),".000",sep=""))

##make xts objects

#bond
ymx <- al_bbgtoxts(ym)
xmx <- al_bbgtoxts(xm)
xmymx <- al_bbgtoxts(xmym)
us22x <- al_bbgtoxts(us22)
spain22x <- al_bbgtoxts(spain22)
germany22x <- al_bbgtoxts(germany22)

#stir
IR1x <- al_bbgtoxts(IR1)
IR4x <- al_bbgtoxts(IR4)
ED4x <- al_bbgtoxts(ED4)
AUDOIS12MOx <- al_bbgtoxts(ADSO1)
USOIS3yx <- al_bbgtoxts(USSO3)

#swaps
ADSW3Qx <- al_bbgtoxts(ADSW3Q)
ADSW10x <- al_bbgtoxts(ADSW10)
ADSF3Qx <- al_bbgtoxts(ADSF3Q)
ADSF10Qx <- al_bbgtoxts(ADSF10Q)

#inflation
US10YBEIx <- al_bbgtoxts(US10YBEI)

#equities
sandp_usx <- al_bbgtoxts(sandp_us)

#currency
audusdx <- al_bbgtoxts(audusd)
audnzdx <- al_bbgtoxts(audnzd)

#volatility
vixx <- al_bbgtoxts(vix)

##graph objects

#bond
al_marketgraph_pdf(ymx, "3y bond future", size=1)
al_marketgraph_pdf(xmx, "10y bond future", , size=1)
al_marketgraph_pdf(xmymx, "3y-10y bond spread", size=0)
al_marketgraph_pdf(us22x, "10y US Treasury", "yield (%)")
al_marketgraph_pdf(spain22x, "10y Spain Govt Bond", "yield(%)")
al_marketgraph_pdf(germany22x, "10y Germany Govt Bond", "yield(%)")

#stir
al_marketgraph_pdf(IR1x, "90d Bank Bill Future (1st)", size=1)
al_marketgraph_pdf(IR4x, "90d Bank Bill Future (4th)", size=1)
al_marketgraph_pdf(ED4x, "Eurodollar Contract (4th)", size=1)
al_marketgraph_pdf(AUDOIS12MOx, "AUD 12Month OIS", "yield")
al_marketgraph_pdf(USOIS3yx, "US 3y OIS", "yield")

#swaps
al_marketgraph_pdf(ADSW3Qx, "AUD 3y Swap", "yield")
al_marketgraph_pdf(ADSW10x, "AUD 10y Swap", "yield")
al_marketgraph_pdf(ADSF3Qx, "AUD 3y Swap Spread", "bps")
al_marketgraph_pdf(ADSF10Qx, "AUD 10y Swap Spread", "bps")

#inflation
al_marketgraph_pdf(US10YBEIx, "US Breakevens (10yr)","inflation rate (%)")

#equities
al_marketgraph_pdf(sandp_usx, "S&P 500 e-mini", "index")

#currency
al_marketgraph_pdf(audusdx, "AUDUSD", "USD per AUD")
al_marketgraph_pdf(audnzdx, "AUDNZD", "NZD per AUD")

#volatility
al_marketgraph_pdf(vixx, "VIX", "index")
