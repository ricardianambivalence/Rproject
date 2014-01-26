## IB library functions

# now find the first Tuesday
firstTuesday <- function(tt, numcol=6){
  preday <- data.frame(matrix(ncol=numcol, nrow=length(tt)))
  lastday <- data.frame(matrix(ncol=numcol, nrow=length(tt)))
  meetDay <- data.frame(matrix(ncol=numcol, nrow=length(tt)))
  dd <- as.Date(tt)
  lt <- as.POSIXlt(tt)
  for (i  in 0:numcol){
    firstOf <- as.POSIXlt(dd - (lt$mday - 1))
    firstOf$mon <- firstOf$mon + i
    firstOf <- as.POSIXlt(as.Date(firstOf))
    firstTue <- as.Date(firstOf)
    idx <- firstOf$wday > 2
    noJan <- firstOf$mon == 0
    firstTue[idx]  <- as.Date(firstOf[idx]) + (9 - firstOf$wday[idx])
    firstTue[!idx]  <- as.Date(firstOf[!idx]) + (2 - firstOf$wday[!idx])
    meetDay[,(i+1)] <- as.Date(firstTue)
    preday[,(i+1)] <- as.POSIXlt(firstTue)$mday
    preday[noJan, (i+1)] <- 0 # sets jan to 0 as there's no meeting
    lastday[,(i+1)] <- as.POSIXlt(toLastDay(firstTue))$mday 
  }
  outList <- list()
  outList[[1]] <- preday
  outList[[2]] <- lastday - preday
  outList[[3]] <- meetDay
  names(outList) <- c('preday', 'postday', 'meetDay')
  return(outList)
}

# the day of the month
ibAdjusteR <- function(rates, dds){
  #adj rates has one less column than 'x' as it does not include current RBA rate
  adjRates <- data.frame(matrix(ncol = (ncol(rates)-1), nrow = nrow(rates)))
  adjRates  <- rates[,-1]
  RBAbymon <- apply.monthly(rates[,1], last)
  index(RBAbymon) <- toLastDay(index(RBAbymon))
  for (i in 2:ncol(rates)){
    if (i == 2) {
      # need to test if we are past the RBA date ... if so fix IB rate at RBA rate
      # this can be broadened later with a switch to deal with 'emergency cuts'
      afterMeeting <- as.POSIXlt(index(rates))$mday > dds[[1]][1] # logical indexing dates postRBA
      adjRates[afterMeeting,1] <- rates[afterMeeting,1] # set postMeeting IB1 rates == RBA rate
      priorMonthEnd <- index(rates)[!afterMeeting] - as.POSIXlt(index(rates)[!afterMeeting])$mday
      rbaMatches <- match(priorMonthEnd, index(RBAbymon))
      # this ection below is req'd on dates prior to the RBA meeting ... 
      #       if(is.na(rbaMatches)) {
      #           RBAprior <- xts(coredata(rates)[1], order.by = priorMonthEnd)
      #           names(RBAprior) <- 'RBACTRD'
      #           RBAbymon <- rbind(RBAprior, RBAbymon)
      #           rbaMatches <- match(priorMonthEnd, index(RBAbymon))
      #       }
      adjRates[!afterMeeting,1] <- (rates[!afterMeeting,i] * (dds$preday[!afterMeeting, (i-1)] + dds$postday[!afterMeeting, (i-1)]) - 
                                    (coredata(RBAbymon[rbaMatches]) * dds$preday[!afterMeeting, (i-1)])) / dds$postday[!afterMeeting,(i-1)]
    }
    else {
      # need to use prior column == (i-2) of adjRates
      adjRates[, (i-1)] <- (rates[,i] * (dds$preday[,(i-1)] + dds$postday[,(i-1)]) - (adjRates[,(i-2)] * dds$preday[,(i-1)])) / dds$postday[,(i-1)]
    }
  }
  return(adjRates)
}

# get meeting expectations function
getMeetingExp <- function(fixingDate, dataMatrix = predFrame_x, startWindow=NULL, endWindow=NULL, plotArg=TRUE) {
  # note there is a problem with start and end dates that aren't in the sample -- fix this .. take next

  if (length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", fixingDate))>0) {
    if (fxD <- as.Date(fixingDate)) {
      if (fxD %in% meetingDates) {
        print('meeting date entered, using EFFECTIVE date (the next day)')
        fxD <- fxD + 1
      } else { 
        if (fxD %notin% (meetingDates + 1)) stop ('not a valid date - enter an RBA meeting date')
      }
    } else stop ('not a valid date: use yyyy-mm-dd number string')
  } else {
    if (length(grep("[A-Z][a-z]{2}-[0-9]{2}", fixingDate)) > 0) {
      fxD <- meetingDates[grep(fixingDate, format(meetingDates, "%b-%y"))] 
      fxD <- fxD + 1
    } else stop ('must use Capitilised Mar-12 string')
  }

  if (is.null(startWindow)) {
    sw <- min(which(!is.na(dataMatrix[fxD,]))[-1])
    startWindow <- colnames(dataMatrix)[sw-1]
  } else {
    sw <- which(colnames(dataMatrix) == startWindow)
  }

  if (is.null(endWindow)) {
    ew <- max(which(!is.na(dataMatrix[fxD,]))[-1])
    endWindow <- colnames(dataMatrix)[ew-1]
  } else {
    ew <- which(colnames(dataMatrix) == endWindow)
  }

  dd <- dataMatrix[fxD, c(sw:ew)]
  ddx <- xts(as.vector(dd), order.by = as.Date(colnames(dd)))
  colnames(ddx) <- paste0(as.character(as.Date(fxD)), 'IB')
  if(plotArg) {
    par(oma = c(1.25,0,0,0))
    plotTitle <- paste0(format(fxD, "%b-%y"), ' RBA expectation: ', startWindow, ' --> ', endWindow)
    plot(ddx, type='l', las=2, xlab="", ylab="", main=plotTitle, 
         major.format = "%b-%y")
    abline(h = dataMatrix[fxD, 1], col=2, lty=2)
    lines(ddx[-1,], col=1, lwd=2)
    points(last(ddx), pch=10, col=6)
    mtext("Source: UBS, Bloomberg", side=1, outer=TRUE, cex=0.75, line = 0.25, adj=0)
  }
  return(ddx)
}

# a library function to fetch the matching dates
dsFn <- function(ds, dd, dtime=0)
{
  tt <- dd - as.integer(dtime)
  dsOut <- as.Date(t(ds[tt]))
  return(dsOut)
}
#
