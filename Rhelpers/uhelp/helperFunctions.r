# NOTE: this needs to be sorted by type, and possbly built into a package
# NOTE: need a pull-date function: load in a few bonds, and build a curve and pull interped point

"%||%" <- function(a, b) if(!is.null(a)) a else b

Xfreq <- function(XTS) {
    switch(periodicity(XTS)$scale,
           daily = 'day',
           weekly = 'week',
           monthly = 'month',
           quarterly = '3 month',
           yearly = 'year')
}

# t-note fractions to decimals
tFrac2Dec <- function(x) {
  hyphSplit <- strsplit(x, "-")
  ch1 <- sapply(hyphSplit,`[`, 1)
  ch2 <- sapply(hyphSplit,`[`, 2)
  x32 <- as.integer(substring(ch2, 1, 2))
  x128 <- rep(0, length(ch1))
  x128[grep("\\+$", ch2)] <- 2
  x128[grep("1/4", ch2, fixed=TRUE)] <- 1
  x128[grep("3/4", ch2, fixed=TRUE)] <- 3
  dec <- x32/32 + x128/128
  dec[is.na(dec)] <- 0
  as.integer(ch1) + dec
}

# remove whitespace from a string
removeSpaces <- function(S) gsub("[[:space:]]","", S)

# detach multiple packages at once
mdetach <- function(..., unload = FALSE, character.only = FALSE, force = FALSE) {
    path <- search()
    locs <- lapply(match.call(expand=FALSE)$..., function(l) {
        if(is.numeric(l))
            path[l]
        else l
    })
    lapply(locs, function(l)
        eval(substitute(detach(.l, unload=.u, character.only=.c, force=.f),
        list(.l=l, .u=unload, .c=character.only, .f=force))))
    invisible(NULL)
}

# builds a list with the data.frame behaviour for names
listN <- function(...){
    dots <- list(...)
    inferred <- sapply(substitute(list(...)), function(x) deparse(x)[1])[-1]
    if(is.null(names(inferred))){
        names(dots) <- inferred
    } else {
        names(dots)[names(inferred) == ""] <- inferred[names(inferred) == ""]
    }
    dots
}

# require-install-package
reqFun <- function(pack) {
    if(!suppressWarnings(suppressMessages(require(pack, character.only = TRUE)))) {
        message(paste0("unable to load package ", pack,
                       ": attempting to download & then load"))
        install.packages(pack)
        require(pack, character.only = TRUE)
    }
}

# multiple require-install-package
mrip <- function(..., install = TRUE) lapply(..., reqFun)

# clean up, but don't remove the helpEnv which contains all the helper functions!
cleanUp <- function() {
    rm(list = ls(envir = .GlobalEnv)[-grep("helpEnv", ls(envir = .GlobalEnv))],
       envir = .GlobalEnv)
    gc()
}

# makes a vector Num long using a sliding / widening window, applying summary function `ff`
slideFill <- function(X, Num, ff, topOnly = TRUE) {
    fillNum <- vector(mode = 'numeric', length=Num)
    for (i in 1:Num) {
        fillNum[i] <- ff(X[1:i])
    }
    if(topOnly) {
        fillNum
    } else {
        X[1:Num, ] <- fillNum
        X
    }
}

# check to see if a package is attached, and load it if it's not
pckReq <- function(pckName, install = FALSE) {
    if(!paste0('package:', pckName) %in% search())
        mrip(pckName)
}

pngMk <- function(pngName, Wwidth = 480, Hheight = 480)
{
    png(file.path(plotPATH, pngName), width = Wwidth, height = Hheight)
}

pdfMk <- function(pdfName, Wwidth = 7, Hheight = 7)
{
    pdf(file.path(plotPATH, paste0(pdfName, ".pdf")), width = Wwidth, height = Hheight)
}

# for when when you have plotPATH set, and a plot function
plot2pdf <- function(plotFun, nameString, ...) {
    pdfTitle <- if(exists("beginPlot")) {
        paste0(nameString, substr(beginPlot, 1, 4))
    } else nameString
    pdfMk(pdfTitle)
    do.call(plotFun, args = list(...))
    dev.off()
}

# return the object with only the complete cases
comCaseRet <- function(X) X[complete.cases(X),]


# system switch -- returns call, matched to how the vim-r-plugin works
knitSysSwitch <- function()
{
   switch(Sys.info()[['sysname']],
           Windows = 'source',
           Darwin = 'base::source'
           )
}

# turn xts period to 'month' or '3 month'
xPeriodToMon <- function(XTS)
{
    switch(periodicity(XTS)$scale,
           monthly = 'month',
           quarterly = '3 month'
           )
}

# take a wide data-frame, turn into xts, fill in blanks using an (curry function to use approx as well)
wideAndSpline <- function(X, funList)
{
    na.spline(as.xts(do.call(acast, c(quote(X), funList))), xout=as.Date(index(X)))
}

wideAndFill <- function(X, filler, funList)
{
# the funList is a acast formula such as: acast(X, rowvar ~ colvar, value.var=value)
    na.approx(as.xts(do.call(filler, list(c(quote(X), funList))), xout=as.Date(index(X))))
}

# push a series fwd k periods: differs from lag(X,k) as it moves the endpoiny fwd in time
lead <- function(X, k)
{
    tt <- c(index(X[-nrow(X)]), seq(last(index(X)), by = 'mon', length.out=(k+1)))
    dd <- c(rep(NA, k), coredata(X))
    dx <- xts(dd, order.by = tt)
    names(dx) <- names(X)
    return(dx)
}

# find the date difference of an xts date index
dayDiff <- function(X)
{
    as.numeric(as.Date(index(X))) - c(NA, as.numeric(as.Date(index(X[-nrow(X)]))))
}

# find bps per day
bpsDay <- function(X) { as.numeric(diff(X)) / dayDiff(X) }

# lapply across an xts object
na.extend <- function(obj, extend='both'){
    stopifnot(is.xts(obj))
		if(extend %in% c('both','start','end','none')){
			#interpolate the inner part
			interped <- xts(na.approx(obj, xout = index(obj), na.rm=FALSE), order.by=as.Date(index(obj)))
			#get the slope of the interped curve (interpolating needs to be done first)
			slope <- bpsDay(interped)
			#where would extrapolation end/start at the short/long end of the curve
			naBounds <- c(range(which(!is.na(obj)))[1] - 1, range(which(!is.na(obj)))[2] + 1)
			#what slope for the extrapolation
			slopeFirstLast <- range(which(!is.na(slope)))
			slopeStartEnd <- slope[slopeFirstLast]
			#get the extrapolations
			if ((extend %in% c('both','start')) && (slopeFirstLast[1] > 1)){
			interped[1:naBounds[1],] <-
				as.numeric(index(interped)[1:naBounds[1]]-index(interped)[naBounds[1]+1])*slopeStartEnd[1]+
				as.numeric(obj[naBounds[1]+1,])
			}
			if ((extend %in% c('both','end')) && (slopeFirstLast[2] < nrow(interped))) {
			interped[naBounds[2]:nrow(interped),] <-
				as.numeric(index(interped)[naBounds[2]:nrow(interped)]-index(interped)[naBounds[2]-1])*slopeStartEnd[2]+
				as.numeric(obj[naBounds[2]-1,])
			}
			return(interped)
		}
		else {
			warning("Error: use argument extend = 'both' or 'start' or 'end' or 'none'. Default behaviour is 'both'")
		}
}

# add an observation to the end of an XTS vector --
# to do this use a function rather than the lm() object
addObvAR <- function(XTS, iter = 1)
{
    # takes an XTS vector and returns the next obsevation as calc'd by AR1
    if(iter == 0) {
        rbind(XTS, xts())
    } else {
        nextObvX <- xts(lm(XTS ~ lag(XTS))$coefficients %*% c(1, last(XTS)),
                        order.by = nextDate(XTS))
        XTS <- rbind(XTS, nextObvX)
        addObvAR(XTS, (iter - 1))
    }
}

# extend a series of dates which index an xts object
addDates <- function(Xts, N) {
    # objective: to extend a series of dates
    # definitions and tests
    stopifnot(is.xts(Xts))
    stopifnot(N > 0)
    index(Xts) <- as.Date(index(Xts))
    all1st <- all(unique(as.POSIXlt(index(Xts))$mday) == 1)
    # functions
    ext.day <- function(N, dString) index(last(Xts)) + 1:N
    ext.week <- function(N, dString) index(last(Xts)) + 7 * (1:N)
    ext.months <- function(N, dString) {
        # test to see if all first dates are 1
        if(!all1st) {
            dd <- toLastDay(index(last(Xts)), toFirst=TRUE)
            toLastDay(seq(dd, by = dString, length.out = (N + 1))[-1])
        } else {
            seq(index(last(Xts)), by = dString, length.out = (N + 1))[-1]
        }
    }
    addPeriods <- function(Xts.period) {
        ext.call <- Xts.period
        if(ext.call %in% c('month', '3 month')) ext.call <- 'months'
        extras <- xts(NULL,
                      order.by = get(paste0("ext.",
                                            ext.call))(N, Xts.period)
                      )
        merge(Xts, extras)
    }
    if(Xfreq(Xts) %in% c('month', '3 month') && !all1st) {
        warning("As mon-date was not uniformly 1st, ALL mon-dates were coerced to last of month")
        toLastDay(addPeriods(Xfreq(Xts)))
    } else {
        addPeriods(Xfreq(Xts))
    }
}

# extend the date by one period
nextDate <- function(XTS) { addDates(XTS, 1) }

# fill and extend an xts object using AR methods
na.ARextend <- function(Xts, ARorder = 1, ADD = 0, pWindow = 120){
# takes an xts object and extends using AR of ARorder, forecasting forward by ADD periods,
# using max(pWindow, available) observations to find the paramters.
# to iterate over cols of an xts
# -> do.call(cbind, lapply(1:ncol(xxd_ext), function(N) na.ARextend(xxd_ext[,N])))
    pckReq("xts")
    stopifnot(ncol(Xts) == 1)
# make the regression matrix to regress on
    makeRegMatrix <- function() {
        dataWindow <- do.call(cbind, lapply(0:ARorder,
                                            function(X) tail(lag(Xts_trim, X),
                                                             pWindow)))
        regMatrix <- cbind(dataWindow[,1],
                           rep(1, nrow(dataWindow)),
                           dataWindow[, 2:ncol(dataWindow)]
                           )[-c(1:ARorder), ]
    }
# some definitions
    Xts_filled <- na.approx(Xts, na.rm=FALSE)
    Xts_trim <- Xts_filled[complete.cases(Xts_filled),]
    regMat <- makeRegMatrix()
    coeffs <- lm.fit(y = regMat[, 1], x = regMat[, -1])$coefficients
# add on NAs if ADD is not 0
    if(ADD) Xts_filled <- addDates(Xts_filled, N = ADD)
    firstNonNA <- min(which(!is.na(Xts_filled)))
    locNAs <- which(is.na(Xts_filled)) # find the NA elements (if there are any)
    locNAs <- locNAs[locNAs > firstNonNA]
    for (n in seq_along(locNAs)) {
        vals <- c(1, sapply(1:ARorder, function(i) Xts_filled[locNAs[n] - i, ]))
        Xts_filled[locNAs[n],] <- coeffs %*% vals
    }
    return(Xts_filled)
}

na.ARextend_c <- {
    pckReq("compiler")
    cmpfun(na.ARextend)
}

# {{{ stats functions

# find mode
Mode <- function(x)
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# close stats functions }}}

# from source function -- for scripted main behaviour in R
fromSource <- function()
{
    knitSysSwitch <- function()
    {
        switch(Sys.info()[['sysname']],
               Windows = 'base::source',
               Darwin = 'base::source')
    }
    length(sys.frame()) >= 4 && sys.call(1)[[1]] == knitSysSwitch()
}

# calculate the expected and realised roll on a swap
# - note LHS there is no checking for concordance between LHS and RHS
calcRoll <- function(XDF, LHS, RHS, period = 91, SCL=1)
{
    # the function uses 5 day medians to calc realised v expected carry
    XDFa <- rollapply(XDF, 1, median, by.column=T, align='right')
    mt <- xts(NULL, seq(as.Date(index(first(XDFa))), as.Date(last(index(XDFa))), by = 'day'))
    XDF_stretch <- na.locf(merge(XDFa, mt))
    xr <- SCL * (get(LHS, XDFa) - get(RHS, XDFa))
    rr <- vector('numeric', length = nrow(get(LHS, XDFa)))
    idx = as.Date(index(XDF))
    for (i in 1:length(rr)) {
        futureYield <- as.numeric(get(RHS, XDF_stretch)[idx[i] + period])
        if (length(futureYield) != 1) {
            rr[i] <- NA
        } else {
            rr[i] <- SCL*(as.numeric(get(LHS, XDFa)[idx[i]]) - futureYield)
        }
    }
    roll <- merge(xts(rr, order.by = idx), xr)
    names(roll) <- c('realised', 'expected')
    return(roll[, c(2,1)])
}

# between function - check to see if x is between l and u
is.between <- function(x, l, u) {
    b <- c(l, u)
    x %between% b
}

`%between%` <- function(x, b) x >= b[1] & x <= b[2]

StretchXts <- function(Xts, dateStep) {
# takes an xts object and fill in the missing dates by day step
# add check to make sure that dateStep is shorter than periodicity(Xts)$scale
    stopifnot(is.xts(Xts))
    origTime <- index(Xts)
    filledTime <- seq(origTime[1], last(origTime), by = dateStep)
    emptyX <- xts(NULL, order.by = filledTime)
    stretchedX <- merge(emptyX, Xts)
}

DayDiffXts <- function(Xts, dayDiff = 1) {
# takes an xts object and difference it by n days, rolling back where t-n day is missing
    stopifnot(is.xts(Xts))
    diff(na.locf(StretchXts(Xts, 'day')), dayDiff)[index(Xts)]
}

RewindXts <- function(Xts, dayRew = 1) {
# rewinds XTS by dayRew days -- use last(RewindXts(Xts, dayRew)) for function of rewindX()
    stopifnot(is.xts(Xts))
    na.locf(StretchXts(Xts, 'day'))[index(Xts) - dayRew]
}

# rewind an xts object by n days
rewindX <- function(Xts, dayRew=1, fillNA = TRUE, last = TRUE, oldDates = TRUE){
    # rewinds an Xts object by dayRew days
    stopifnot(is.xts(Xts))
    index(Xts) <- as.Date(index(Xts))
    lastFlip <- function(X) {
        if(last) {
            last(X)
        } else X
    }
    newDates <- index(Xts) - dayRew
    rewindRows <- findInterval(newDates, index(Xts))
    Xts_rew <- xts(matrix(rep(NA, length(Xts)), ncol=ncol(Xts)),
                   order.by = if(oldDates) index(Xts) else newDates)
    nonZeros <- rewindRows[rewindRows > 0]
    Xts_rew[rewindRows > 0,] <- if(fillNA) {
        na.locf(Xts, na.rm = FALSE)[nonZeros,]
    } else {
        Xts[nonZeros, ]
    }
    lastFlip(Xts_rew)
}

dateCompX <- function(Xts, lagNum = 7, fillNA = TRUE, Yts = NULL){
    fillTest <- function(X){if(fillNA) na.locf(X) else X }
    stopifnot(is.xts(Xts),
              if(!is.null(Yts)) is.xts(Yts) else TRUE
              )
    if(is.null(Yts)) Yts <- Xts
    Xts - rewindX(Yts,
                     dayRew = lagNum,
                     fillNA = fillNA,
                     last = FALSE,
                     oldDates = TRUE)
}

# fetch data from UBS Strategy MDB
fetchUDB <- function(DBs, TABLEs, META)
{
    #preallocate the data frame list
    datDb <- list()
    #fill the data frame list
    for (i in 1:length(dblist)){
        db.ch <- odbcConnectAccess(dblist[i])
        for (j in 1:nrow(tbllist[tbllist$path == dblist[i],])){
            Tquery <- paste0('SELECT ',
                             paste(dbMeta[dbMeta$table == tbllist$table[j],]$ticker, collapse=","),
                             ' FROM ',
                             tbllist$table[j]
                             )
            datDb[[tbllist$table[j]]] <- sqlQuery(db.ch, Tquery)
        }
        odbcClose(db.ch)
    }
    return(datDb)
}

dateChurn <- function(ds)
{
    if(nchar(head(ds,1)) == 9) {
        pp <- paste0(substr(ds, 6, 9), "-", substr(ds, 3, 4), "-01")
        ds_Pct <- as.POSIXct(pp)
    }
    return(ds_Pct)
}

# Date Functions
toLastDay <- function(dateObj, monAdv=0, toFirst = FALSE) {
    # takes date object, transforms and returns a dat object
    dateWarp <- function(dateObj) {
        tt <- as.POSIXlt(dateObj)
        tt$mon <- tt$mon + monAdv # moves the month
        tt$mday <- 1L             # make date the first
        if(toFirst) {
            tt <- as.Date(tt)
        } else {
            tt$mon <- tt$mon + 1L # go to the first of the next month
            tt <- as.Date(tt) - 1L # subtract one day, yielding the last of prior month
        }
    }
    if(class(dateObj)[1] %in% c("POSIXct", "POSIXt", "Date")) {
        dateWarp(dateObj)
    } else {
        message("input class NOT %in% {POSIXt, POSIXct, Date}; if is.xts(dateObj) toLastDay() uses object")
        stopifnot(is.xts(dateObj))
        index(dateObj) <- dateWarp(index(dateObj))
        return(dateObj)
    }
}

# make dates on Qtr ends - note similarity with toLastDay -> abstraction!
toQtrMons <- function(dateObj, toFirst=FALSE)
{
    tt <- as.POSIXlt(dateObj)
    tt$mday <- 1L # make first of month
    tt$mon <- tt$mon + (2 - tt$mon %% 3)
    if(toFirst) {
        tt <- as.Date(tt)
    } else {
        tt$mon <- tt$mon + 1L # go to the first of the next month
        tt <- as.Date(tt) - 1L # subtract one day, yielding the last of prior month
    }
    return(tt)
}

# takes qtrtly xts and converts to lower frequency: day, week or month
qtr2Lower <- function(QD, lowerFreq, monShift = 0L, filler = "na.approx")
{
    stopifnot(is.xts(QD), lowerFreq %in% c('day', 'week', 'mon'))
    QD_adj <- QD
    fromD <- toLastDay(index(first(QD)), -2L, toFirst = TRUE)
    if(lowerFreq == 'mon') {
        toD <- toLastDay(index(last(QD)), toFirst = TRUE)
        q2m_dates <- toLastDay(seq(fromD, toD, by = lowerFreq))
        index(QD_adj) <- toLastDay(index(QD), monAdv = monShift)
    } else {
        toD <- toLastDay(index(last(QD)))
        q2m_dates <- seq(fromD, toD, by = lowerFreq)
        index(QD_adj) <- toLastDay(index(QD), monAdv = monShift - 1)
    }
    emptyX <- xts(, q2m_dates)
    mm <- merge(emptyX, QD_adj)
    mm_filled <- do.call(filler, list(mm))
    if(lowerFreq == 'mon') {
        return(mm_filled)
    } else {
        return(lag(mm_filled))
    }
}

# make Qtrly monthly --> default is with obv in the mid qtr
qtr2Mon <- function(QD, monShift = -1L, filler = "na.approx") {
    qtr2Lower(QD, lowerFreq = 'mon', monShift, filler)
}

# this could be done faster -- see SO answer
matchDate <- function(searchDate, dateList, roundDown=FALSE){
    if(roundDown) {
        dist2date <- as.Date(dateList) - as.Date(searchDate)
        closest <- which(max(dist2date[dist2date<=0]) == dist2date)
    } else {
        dist2date <- as.Date(dateList) - as.Date(searchDate)
        closest <- which(min(dist2date[dist2date>=0]) == dist2date)
    }
    return(dateList[closest])
}

# and this might be done faster as well -- see SO answer
closestDate <- function(searchDate, dateList, roundDown=FALSE){
    # add something that checks to see if reshape2 is loaded
    cDates <- melt(lapply(searchDate, FUN = function(x) matchDate(x, dateList)))[,1]
    return(cDates)
}

# handy misc
completeSub <- function(Dd, colNames) { Dd[, colNames][complete.cases(Dd[, colNames])] }

mjLag <- function(X, llen = 1)
{
    lenX <- length(X)
    lagX <- c(rep(NA, llen), X[1:(lenX - llen)])
}

mjDiff <- function(X, llen = 1) {X - mjLag(X)}

`%notin%` <- function(x,y) !(x %in% y)

dirMaker <- function(mainDir="S:/Rates Research/R_share/", subDir="seasplots")
{
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

dotRenamer <- function(dd)
{
    names(dd) <- gsub("^\\.+|\\.[^.]*$", "", names(dd), perl=TRUE)
    return(dd)
}

getBeta <- function(Xd, LHS, RHS, Diff = 0)
{
    if(Diff) {Xd <- diff(Xd, Diff)}
    Bb <- coef(lm(as.formula(paste(LHS, "~", RHS)), data = Xd))[2]
}

mmths <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
MMTHS <- toupper(mmths)


error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# fill up or down a series using the difference in another (convert to logs if you need percentage changes)
fillXZ <- function(X, Z, LOG = TRUE, DOWN = TRUE, dateRange = NULL) {
# NOTE: this needs a single direction option, and a both method
    # helper functions
    returner <- function() {
        if(LOG) {
            return(exp(X))
        } else {
            return(X)
        }
    }
    fillDown <- function(X) {
        for (di in missingIndex) {
            X[xDates[di]] <- lag(X)[xDates[di]] + diffZ[xDates[di]]
        }
        X
    }
    fillUp <- function(X) {
        for (di in rev(missingIndex)) {
            X[xDates[di]] <- lag(X, -1)[xDates[di]] - lag(diffZ, -1)[xDates[di]]
        }
        X
    }
    # test and definitions
    if(LOG) {
        X <- log(X)
        Z <- log(Z)
    }
    if(is.null(dateRange)) {
        dateRange <- paste0(first(index(X)), "::", last(index(X)))
    }
    missingIndex <- which(is.na(X[dateRange]))
    if(length(missingIndex) == 0) return(returner())
    # if missing we have work to do
    xDates <- index(X[dateRange])
    diffZ <- diff(Z)
    if(DOWN) {
        X <- fillDown(X)
    } else {
       X <- fillUp(X)
    }
    return(returner())
}

# take an xts input and plot a fanPolygon + line
xtsFanPoly <- function(XTS, ymin = NULL, ymax = NULL, xlab = "", ylab="", main="",
                       col ='black', type = 'l') {
    stopifnot(is.xts(XTS))
    plot(coredata(XTS$fit) ~ index(XTS), type='n', las=1, ylim = range(ymin, ymax, XTS),
         xlab = xlab, ylab = ylab, main = main)
    polygon(c(index(XTS), rev(index(XTS))),
            c(coredata(XTS$upr), rev(coredata(XTS$lwr))),
            col = 'grey88',
            border = NA)
    grid(col = 'grey60')
    lines(coredata(XTS$fit) ~ index(XTS), col= col, lwd=2, type = type, lty=2)
}

# get data from clipboard
frXL <- function(headArg=T) read.table('clipboard', header=headArg, sep="\t", as.is=TRUE)

# paste data to the clipboard
toXL <- function(x, Xd=FALSE) {
    if(!Xd) {
        write.table(x, "clipboard", sep = "\t", row.names=F)
    } else {
        stopifnot(is.xts(x))
        xd <- data.frame(date = index(x), coredata(x))
        write.table(xd, "clipboard", sep = "\t", row.names=F)
    }
}

# turn a dataframe with the first column made up of dates into an xts

xtsF <- function(x)
{
    ddx <- xts(x[,-1], order.by = as.POSIXct(x[,1]))
    index(ddx) = make.index.unique(index(ddx))
    return(ddx)
}

frxx <- function() { xtsF(frXL())}

pfx <- function(tempD = NULL, mainString = "", cn = 1, tradeDeets = NULL, legLoc = 'topleft',
                major.format = "%b-%y")
{
    if(is.null(tempD)) tempD <- na.locf(frxx())
    stopifnot(
              is.xts(tempD),
              is.null(tradeDeets) | length(tradeDeets) == 4
              )
    if(!is.null(tradeDeets)) {
        tradeX <- xts(data.frame(entry = rep(NA, nrow(tempD)),
                                 target = rep(NA, nrow(tempD)),
                                 stop = rep(NA, nrow(tempD))),
                      order.by = as.Date(index(tempD)))
        startD <- as.Date(tradeDeets['start']) - 1
        tradeX[startD, 'entry'] <- as.numeric(tradeDeets['entry'])
        tradeX[startD, 'stop'] <- as.numeric(tradeDeets['stop'])
        tradeX[startD, 'target'] <- as.numeric(tradeDeets['target'])
        tradeX <- na.locf(tradeX)
        plot(tempD[, cn], main = mainString, las=1, major.format = major.format,
             ylim = c(min(as.numeric(tradeDeets[c('stop', 'target')])),
                      max(as.numeric(tradeDeets[c('stop', 'target')]))))
        lines(tempD[, cn], lwd=2)
        lines(tradeX$stop, col = 'red', lty = 'dashed', lwd=2)
        lines(tradeX$entry, col = 'blue', lty = 'dashed', lwd=2)
        lines(tradeX$target, col = 'green', lty = 'dashed', lwd=2)
        points(last(tempD[, cn]), col=3, pch=18, cex=1.5)
        legend(legLoc, c('entry', 'stop', 'target'), bg='gray95',
               lwd=c(2,2,2), col= c('blue', 'red', 'green'))
    } else {
        plot(tempD[, cn], main = mainString, las=1, major.format = major.format)
        lines(tempD[, cn], lwd=2)
        points(last(tempD[, cn]), col=3, pch=18, cex=1.5)
    }
    if(ncol(tempD) > 1) message("This function prints one column only: default is 1st (tweak cn = 1)")
}



# subset from the right side of string
substrRight <- function(x, n) { substr(x, nchar(x)-n+1, nchar(x)) }

# melt an xts
meltx <- function(dx, ID=1) {
    pckReq("reshape2")
    melt(data.frame(date = index(dx), dx), id.vars = ID)
}

# fast make an xy-point + regression plot
PointReg <- function(Xts, a=1, b=2, mainString = NULL, xLab = NULL, yLab = NULL)
{
    stopifnot(is.xts(Xts),
              ncol(Xts) >1)
    lapply(list("ggplot2", "reshape2", "RColorBrewer"), pckReq)
    gPlot <- ggplot(data = Xts,
                    aes_string(x = colnames(Xts)[1],
                       y = colnames(Xts)[2])) +
                geom_point(shape=5, color = 'red3', alpha = min(1, 7/(nrow(Xts)**0.45))) +
                geom_smooth(method = loess, se = FALSE, size = 1.3, color = 'lightblue', lty = 1) +
                geom_smooth(method = lm, se = FALSE, size = 1.3, color = 'blue', lty = 'dashed') +
                theme_gray(14) +
                geom_point(data = tail(Xts, 1),
                           aes_string(x = colnames(Xts)[1],
                                      y = colnames(Xts)[2]),
                           color = 'green',
                           shape = 19, size = 3)
    # optionals
    if(!is.null(mainString)) gPlot <- gPlot + labs(title = mainString)
    if(!is.null(xLab)) gPlot <- gPlot + labs(x = xLab)
    if(!is.null(yLab)) gPlot <- gPlot + labs(y = yLab)
    gPlot
}

## unstack a bbg data call and return an xts
bbgUnstacker <- function(bbgStack, noWE = FALSE, dotRename = TRUE) {
    if(is.null(bbgStack$ticker)) {
        return(bbgStack)
        stop
    }
    xlist = list()
    for (prodName in unique(bbgStack$ticker)) {
        dd <- subset(bbgStack, bbgStack$ticker == prodName)[,-1]
        xlist[[prodName]] <- xtsF(dd)
    }
    mergeLists <- do.call(merge.xts, lapply(xlist, FUN = function(X) X ))
    if(noWE) {
        outList <- mergeLists[!weekdays(index(mergeLists)) %in% c('Saturday', 'Sunday')]
    } else outList <- mergeLists
    if(dotRename) {outList <- dotRenamer(outList)}
    return(outList)
}


## Rolling regression stuff

# NOTE: the regCoeff requires you to pass in a model formuls as in below
# lmMod <- as.formula(paste0(Yvar, ' ~ AUDJPY + I(AUDJPY^2)'))

# roll reg functions
regCoef <- function(X) coef(summary(lm(lmMod, data=X)))[,1]

addPredError <- function(COeff, DF, Xx, Yy)
{
    act <- get(Yy, DF)
    pred <- COeff[,1] + COeff[,2] * get(Xx, DF) + COeff[,3] * get(Xx, DF)^2
    error <- pred - get(Yy, DF)
    outX <- cbind(act, pred, error)
    names(outX) <- c('actual', 'prediction', 'error')
    return(outX)
}

## roll the regCoeff and addPredError up into a single function
rollModel <- function(DF, wwidth = 160, xName, yName, ddiff=TRUE, ddifNum = 10, ffun = regCoef)
{
    if(ddiff) {
        DFprep = diff(DF, ddifNum)
    } else { DFprep = DF }
    #
    rollingCo <- rollapplyr(DFprep, width = wwidth, FUN = ffun, by.column=FALSE)
    rollingModel <- merge(rollingCo, addPredError(rollingCo, DFprep, xName, yName))
    return(rollingModel)
}

# handy viewport formatting function
vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

# clever SO post on string replacement methods
String <- function(x="") {
    x <- as.character(paste(x, collapse=""))
    class(x) <- c("String","character")
    return(x)
}

"[.String" <- function(x,i,j,...,drop=TRUE) {
    unlist(strsplit(x,""))[i]
}
"[<-.String" <- function(x,i,j,...,value) {
    tmp <- x[]
    tmp[i] <- String(value)
    x <- String(tmp)
    x
}
print.String <- function(x, ...) cat(x, "\n")
#
# y <- rnorm(500, mean=1)
# ymat <- matrix(y, 100, 5)
# y.means <- colMeans(ymat)
# y.sd <- apply(ymat, 2, sd)
# barx <- barplot(y.means, names.arg=1:5, ylim=c(0, 1.5), col=3, axis.lty=1, xlab="Reps", ylab="Value")
# error.bar(barx, y.means, 1.96*y.sd/10)

# {{{ SA functions

##seasonally adjust vectors of irregular NA lengths - MJs function
mj_SAmat_m <- function(X, PRD = 12, adj = 'adjust', to=1, BUG = FALSE)
{
    SAmat <- X
    for (i in seq_along(names(X))) {
        #         cat(names(X)[i], "\n")
        begin <- min(which(!is.na(X[(1:(nrow(X)-12)),i])))
        beginYr <- as.POSIXlt(index(X[begin]))$year + 1900
        beginMth <- as.POSIXlt(index(X[begin]))$mon + 1
        end <- nrow(X) - sum(is.na(tail(X[,i],12)))
        dat <- X[begin:end,i]
        if(BUG) print(paste('doing', names(X)[i])) # for bug testing
        sa <- baysea(coredata(dat), period=PRD, year=beginYr, month=beginMth, shift=1, plot=FALSE, out=0, zersum=1, trend.order=to)
        firstNA <- rep(NA, (begin-1))
        lastNA <- rep(NA, (nrow(X)-end))
        SAmat[,i] <- c(firstNA, get(adj, sa), lastNA)
    }
    return(SAmat)
}

##send an xts object of some seasonally adjusted data, some non-seasonally adjusted data -
##flag the non seasonally adjusted by adding suffix to vector name of "_nsa"
##function will seasonally adjust the subset that has "_nsa" and then rename suffix with "(sa)"
## Convention: _nsa is NSA, _t is trend, " (sa)" means we've done it & no _# means SA'd at source
al_subsetting_sa <- function(x, to=1){
    subset_adjusting <- x[,grep("_nsa$", names(x))]
    x[,grep("_nsa$", names(x))] <- mj_SAmat_m(subset_adjusting, to=to)
    x_names <- grep("_nsa$", names(x))
    names(x)[x_names] <- paste(substr(names(x)[x_names], 1, nchar(names(x)[x_names])-4), " (sa)", sep="")
    return(x)
}


##this wrapper looks to see if you have an xts object, if you do, s.a. it.
##if not, look to see if the first column of the dataframe is in POSIXct format,
##and then make it an xts object.
##
##lastly, if you enter some optional arguments, use those as
al_easySA <- function(x){
    if (class(x)[1] == "xts"){
        x <- al_subsetting_sa(x)
    } else {
        if (class(x[,1])[1] == "POSIXct"){
            x <- xts(x[,-1], order.by=as.Date(x[,1]))
            x <- al_subsetting_sa(x)
        }
    }
    return(x)
}

# end SA functions }}}

## {{{ VAR helpers require(vars)

# measure POOS performance of a VAR

# testVar <- function(dframe, nAhead = 6, IC = 'SC', periodicity = 'months', skip = NULL, Vlag = 12, RSTmtx = NULL)
# {
# setup
#     if(is.null(skip)) { skip <- nrow(dframe) %/% 2 }
#     dateList <- index(dframe)
#     ttlRows <- length(dateList) + nAhead
#     extraDate <- seq(index(dframe[1]), by=periodicity, length.out=ttlRows)
# make extended variables
#     testPack <- list()
#     for (i in 1:ncol(dframe))
#     {
#         testPack[[names(dframe)[i]]] <- as.xts(
#                                                cbind(c(coredata(dframe[,i]), rep(NA, nAhead)),
#                                                      matrix(rep(NA, ttlRows * (length(dateList) - skip)),
#                                                             nrow = ttlRows, ncol = (length(dateList) - skip)
#                                                             )
#                                                      ), order.by = extraDate
#                                                )
#     }
# the test loop
#     for (d in (skip+1):length(dateList))
#     {
#         testName <- paste0('from:', dateList[d]) # need to stick this on later
#         if (is.null(RSTmtx))
#         {
#             optLag <- findMaxVARLag(dframe[1:d,], firstMax= Vlag, crit = paste0(IC, "(n)"))
#             mod <- VAR(dframe[1:d,], p = optLag) # we may want to change the IC part later
#         } else {
#             Vlag <- (ncol(RSTmtx) - 1) / (nrow(RSTmtx))
#             mod <- VAR(dframe[1:d,], p = Vlag)
#             mod <- restrict(mod, method = 'manual', resmat = RSTmtx)
#         }
#         poosPred <- predict(mod, n.ahead=nAhead)

#         for (v in 1:ncol(dframe)) # loop across by column type
#         {
#             outvec <- rep(NA, ttlRows) # allocate the full length vector
#             outvec[(d + 1):(d + nAhead)] <- poosPred$fcst[[v]][,1] # replace with 4casts
#             testPack[[v]][, (d - skip + 1)] <- outvec # one variable at a time
#         }
#     }
#     return(testPack)
# }

# }}} close VAR functions

# {{{ AGL plot methods

# AL :: generic yield plots - with intelligent naming, yaxes, and other default formats (colors, linesizes, major.format etc.)


YieldPlot <- function(obj, main=paste0(names(obj), collapse=' '), major.format='%b-%y',
                      colset="Set1", legend.pos='top', ylab='bps', verbose = FALSE)
{
    pckReq("RColorBrewer")
    if(!verbose) {options(warn=-1); on.exit(options(warn = 0))} # comment out if you always want warnings
	plot(obj[,1], ylim = c(min(obj, na.rm=T), max(obj, na.rm=T)),
	     main=main, major.format=major.format, las=1, ylab=ylab)
    colPal <- brewer.pal(ncol(obj), colset)
	for (i in 1:ncol(obj)){
		lines(na.approx(obj[,i]), lwd=2, col=colPal[i])
	}
	legend(legend.pos, names(obj), lwd=2, col=colPal, box.col='white', bg='gray90')
}


## AL :: SPREAD CORRELATION CHARTS
## pass in an xts object with vectors of 10s IRS, 15s IRS, 10s15s spread and AUDJPY --- show the individual series, the spread and the AUDJPY, the regression analysis
plot_spreadcorr <- function(obj)
{
    #setup the plot
    par(mar=c(4,4,4,4))
    layout(matrix(c(1,3,2,3), 2, 2, byrow=T))

    #make the levels plot (the top one in bloomby)
    cols <- c('black','darkorchid4','skyblue3')
    ymax <- max(obj[,1:2])
    ymin <- min(obj[,1:2])
    charttitle <- paste(colnames(obj)[1],"and",colnames(obj)[2])
    plot(obj[,1], main=charttitle, xlab='', major.format='%b-%y', ylim=c(ymin,ymax), las=1)
    lines(obj[,1], lwd=2, col=cols[1])
    lines(obj[,2], lwd=2, col=cols[2])
    legend('topleft', c(colnames(obj)[1],colnames(obj)[2]), bg='gray90', lwd=c(2,2), col=cols)

    #make the spread plot(the bottom one in bloomby)
    cols <- c('black','darkorchid4','skyblue3')
    charttitle <- paste(names(obj)[3],"and",names(obj)[4])
    plot(obj[,3], main=charttitle, ylim=c(min(obj[,3]),max(obj[,3])), major.format='%b-%y', yaxt='n', ylab='bps', xlab='', las=1)
    axis(2, ylim=c(min(obj[,3]),max(obj[,3])), col=cols[2], lwd=2, las=1)
    lines(obj[,3], lwd=2, col=cols[2])
    par(new=T)
    plot(obj[,4], yaxt='n', xaxt='n', main='')
    lines(obj[,4], lwd=2, col=cols[3])
    axis(4, ylim=c(min(obj[,4]),max(obj[,4])), col=cols[3], lwd=2, las=1)
    legend('topleft', c(colnames(obj)[3],colnames(obj)[4]), bg='gray90', lwd=c(2,2), col=cols[2:3])

    #make the scatterplot regression (12 GO in bloomby)
    plot(coredata(obj[,3]) ~ coredata(obj[,4]), pch=20, col=cols[1], las=1,
         ylab=names(obj)[3], xlab=colnames(obj)[4], main=paste('Regression of Spread on Level of',colnames(obj)[4]))
    abline(rlm(coredata(obj[,3]) ~ coredata(obj[,4])), col=cols[2], lty=2, lwd=4)
    points(coredata(obj[nrow(obj),4]),coredata(obj[nrow(obj),3]),col='red',pch=20,cex=3)
}

## AL :: DUAL AXES CHARTS

## two axes plot using lattice and latticeExtra (mcj)
TwoAxesLat <- function(X, leg.string = NULL)
{
    pckReq('lattice'); pckReq('latticeExtra')
    if(is.null(leg.string)) { leg.string <- c(names(X)[1], names(X)[2]) }
    l1 <- xyplot(X[,1], type = 'l', xlab = '')
    l2 <- xyplot(X[,2], type = 'l', xlab = '')
    ll <- doubleYScale(l1, l2, text = leg.string)
    ll <- update(ll, par.settings = simpleTheme(col = c('red', 'black'),
                                                lty = 1:2))
    return(ll)
}
## pass in an xts object with two vectors of any size, adjust scale and plot them on the same chart with coloured axes.

TwoAxes <- function(obj, legLoc = 'topright', horizArg = TRUE, majorF = "%b-%y",
                    y1lim = c(min(obj[,1]),max(obj[,1])),
                    y2lim = c(min(obj[,2]),max(obj[,2])),
                    mainString = NULL)
{
    #make the spread plot(the bottom one in bloomby)
    cols <- c('black','darkorchid4','skyblue3')
    if(is.null(mainString)) {
        charttitle <- paste(names(obj)[1],"and",names(obj)[2])
    } else charttitle <- mainString
    par(mar = c(4, 4, 2, 4) + 0.1)
    plot(obj[,1], main=charttitle, ylim= y1lim, major.format=majorF, yaxt='n', ylab='', xlab='', las=1)
    axis(2, ylim=y1lim, col=cols[2], lwd=3, las=1)
    lines(obj[,1], lwd=2, col=cols[2])
    par(new=T)
    plot(obj[,2], yaxt='n', xaxt='n', main='')
    lines(obj[,2], lwd=2, col=cols[3])
    axis(4, ylim=y2lim, col=cols[3], lwd=3, las=1)
    legend(legLoc, c(colnames(obj)[1],colnames(obj)[2]), bg='gray90', lwd=c(2,2),
           col=cols[2:3], horiz = horizArg)
}

# AL :: Plot an XTS object as two yields and a spread (a la replicate bloomberg's HS function)
# Call bloom_hs(obj) where obj is a 2 vector xts object
# dependencies are xts and MASS

BloomHS <- function(obj, SCALE = 1, plotChar = 20, legLoc = 'topright', horizArg = TRUE)
{

    #setup the plot
    par(mar=c(4,4,4,4))
    layout(matrix(c(1,3,2,3), 2, 2, byrow=T))

    spread <- SCALE * (obj[,1] - obj[,2])
    scaledBase <- SCALE * obj[,2]
    cols <- c('black','darkorchid4')
    charttitle1 <- paste(colnames(obj)[1],"and",colnames(obj)[2])
    charttitle2 <- paste(colnames(obj)[1],"to",colnames(obj)[2],"spread")

    #make the levels plot (the top one in bloomby)
    ymax <- max(obj[,1:2])
    ymin <- min(obj[,1:2])
    plot(obj[,1], main=charttitle1, xlab='', major.format='%b-%y', ylim=c(ymin,ymax), las=1)
    lines(obj[,1], lwd=2, col=cols[1])
    lines(obj[,2], lwd=2, col=cols[2])
    legend(legLoc, c(colnames(obj)[1],colnames(obj)[2]), bg='gray90',
           lwd=c(2,2), col=cols, horiz = horizArg)

    #make the spread plot(the bottom one in bloomby)
    plot(spread, main=charttitle2, major.format='%b-%y', ylab='bps', xlab='', las=1)
    lines(spread, lwd=2, col=cols[2])
    abline(h=0, lwd=2, lty=2, col='red')

    #make the scatterplot regression (12 GO in bloomby)
    plot(coredata(spread) ~ coredata(scaledBase), pch = plotChar, col=cols[1], las=1,
         ylab=charttitle2, xlab=colnames(obj)[2],
         main= paste('Regression of Spread on Level of',colnames(obj)[2])
         )
    abline(rlm(coredata(spread) ~ coredata(scaledBase)),
           col=cols[2], lty=2, lwd=4
           )
    points(coredata(last(spread)) ~ coredata(last(scaledBase)),
           col='red',pch=20,cex=3
           )

}

BloomHSLeft <- function(obj, legLoc = 'topright', horizArg = TRUE)  {
    #setup the plot
    par(mar=c(4,4,4,4))
    layout(matrix(c(1,2), 1, 2, byrow=T))
    #make the levels plot (the top one in bloomby)
    cols <- c('black','darkorchid4')
    ymax <- max(obj[,1:2])
    ymin <- min(obj[,1:2])
    charttitle <- paste(colnames(obj)[1],"and",colnames(obj)[2])
    plot(obj[,1], main=charttitle, xlab='', major.format='%b-%y', ylim=c(ymin,ymax), las=1)
    lines(obj[,1], lwd=2, col=cols[1])
    lines(obj[,2], lwd=2, col=cols[2])
    legend(legLoc, c(colnames(obj)[1],colnames(obj)[2]), bg='gray90', lwd=c(2,2),
           col=cols, horiz = horizArg)
    #make the spread plot(the bottom one in bloomby)
    charttitle <- paste(colnames(obj)[1],"to",colnames(obj)[2],"spread")
    plot(100*(obj[,1]-obj[,2]), main=charttitle, major.format='%b-%y', ylab='bps', xlab='', las=1)
    lines(100*(obj[,1]-obj[,2]), lwd=2, col=cols[2])
    abline(h=0, lwd=2, lty=2, col='red')

}

LinkerHS <- function(obj) {
    par(mar=c(4,4,4,4))
    layout(matrix(c(1,2),2,1, byrow=T))

    #make the levels plot (the top one in bloomby)
    cols <- c('black','darkorchid4')
    ymax <- max(obj[,1:2])
    ymin <- min(obj[,1:2])
    charttitle <- "BEI 20-8-2015 vs. 3y Inflation ZCS"
    plot(obj[,1], main=charttitle, xlab='Date', major.format='%b-%y', ylim=c(ymin,ymax), ylab='Breakeven Inflation Rate (bps)')
    lines(obj[,1], lwd=2, col=cols[1])
    lines(obj[,2], lwd=2, col=cols[2])
    legend('bottomright', c('20-8-2015 BEI', '3y Inflation ZCS'), bg='gray90', lwd=c(2,2), col=cols)

    plot((obj[,2]-obj[,1]), main='Inflation Swap Rate Minus Breakeven Inflation', major.format='%b-%y', ylab='BEI Spread', xlab='Date')
    lines((obj[,2]-obj[,1]), lwd=2, col=cols[2])
}

# AL :: plot breakeven inflation

linker_temp <- function(obj) {
    cols <- c('black','darkorchid4','plum1')
    ymax <- max(obj[,1:5], na.rm=T)
    ymin <- min(obj[,1:5], na.rm=T)
    charttitle <- "Breakeven Inflation Yields"
    plot(obj[,1], main=charttitle, xlab='Date', major.format='%b-%y', ylim=c(ymin,ymax), ylab='Breakeven Inflation Rate (bps)')
    lines(obj[,1], lwd=2, col=cols[1])
    lines(obj[,3], lwd=2, col=cols[2])
    lines(obj[,5], lwd=2, col=cols[3])
    legend('bottomright', c('20-8-2015 BEI', '21-2-2022 BEI', '20-09-2030 BEI'), bg='gray90', lwd=c(2,2,2), col=cols)
}

linker_temp2 <- function(obj) {
    cols <- c('black','darkorchid4','plum1')
    ymax <- max(obj[,1:5], na.rm=T)
    ymin <- min(obj[,1:5], na.rm=T)
    charttitle <- "Real Yields"
    plot(obj[,1], main=charttitle, xlab='Date', major.format='%b-%y', ylim=c(ymin,ymax), ylab='Real Yield (bps)')
    lines(obj[,1], lwd=2, col=cols[1])
    lines(obj[,3], lwd=2, col=cols[2])
    lines(obj[,5], lwd=2, col=cols[3])
    legend('topright', c('20-8-2015', '21-2-2022', '20-09-2030'), bg='gray90', lwd=c(2,2,2), col=cols)
}
# }}}
