# MCJ's R-helper functions
## {{{ system functions
# clean up, but don't remove the helpEnv which contains all the helper functions!
# returns system name :: Darwin or Windows
sysName <- function() Sys.info()[['sysname']]

# returns TRUE if the head of the call is 'base::source'
fromSource <- function() {
    length(sys.frame()) >= 4 && sys.call(1)[[1]] == 'base::source'
}

# clean up .GlobalEnv except for helpEnv
cleanUp <- function() {
    rm(list = ls(envir = .GlobalEnv)[-grep("helpEnv", ls(envir = .GlobalEnv))],
       envir = globalenv())
    gc()
}

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

rip <- function(pack) {
    if(!require(pack, character.only = TRUE, quietly = TRUE)) {
        message(paste0("unable to load package ", pack,
                       ": attempting to download & then load"))
        install.packages(pack)
        require(pack, character.only = TRUE)
    }
}

mrip <- function(..., install = TRUE) lapply(..., rip)

pckReq <- function(pckName) {
    if(!paste0('package:', pckName) %in% search())
        rip(pckName)
}

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

# }}}
# {{{ general helpers

"%||%" <- function(a, b) if(!is.null(a)) a else b

"%notin%" <- function(x, y) x[!x %in% y]

`%between%` <- function(x, rng) x >= rng[1] & x <= rng[2]

is.between <- function(x, l, u) {
    b <- c(l, u)
    x %between% b
}

xtsF <- function(x) {
    # add a smart date switch: http://stackoverflow.com/questions/6194285/dealing-with-messy-dates/7975560#7975560
    pckReq('xts')
    ddx <- xts(x[,-1], order.by = as.POSIXct(x[,1]))
    index(ddx) = make.index.unique(index(ddx))
    return(ddx)
}

meltx <- function(dx) {
    pckReq('reshape2')
    melt(data.frame(date = index(dx), dx), id.vars = 1)
}

x2df <- function(XTS) {data.frame('date' = index(XTS), data.frame(coredata(XTS)))}

# read ABS sheet function
readABS <- function(XLS, SHEET = 'Data1', LineSkip = 9) {
    pckReq('gdata')
    dat <- read.xls(XLS, sheet = SHEET, as.is=TRUE, skip = LineSkip)
    dat[,1] <- XLdate(dat[,1])
    names(dat)[1] <- 'date'
    return(xtsF(dat))
}

readClvFed <- function(URL, SHEET = 'Sheet1', LineSkip = 0, dateType = 'b-y') {
    pckReq('gdata')
    dat <- read.xls(URL, sheet = SHEET, as.is=TRUE, skip = LineSkip)
    dat[,1] <- XLdate(dat[,1], type = dateType)
    names(dat)[1] <- 'date'
    return(xtsF(dat))
}

dfxColScl <- function(dfrm, pos=1, idx = 100) sweep(idx*dfrm, 2, dfrm[1,], "/")

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

Xfreq <- function(XTS) {
    switch(periodicity(XTS)$scale,
           daily = 'day',
           weekly = 'week',
           monthly = 'month',
           quarterly = '3 month',
           yearly = 'year')
}

# attach names to list elements -- using similar logic to data.frame
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

# find the names of un-named list elements
nameListObjects <- function(LIST, ENV = NULL, NAMES.ONLY = FALSE) {
    pckReq('digest')
    if(is.null(ENV)) ENV <- .GlobalEnv
    list.md5 <- sapply(LIST, digest)
    env.names <- ls(envir = ENV)
    env.md5 <- sapply(env.names, function(x) digest(get(x, envir = ENV)))
    list.names <- env.names[match(list.md5, env.md5)]
    if(NAMES.ONLY) list.names else setNames(LIST, list.names)
}

# remove whitespace from a string
removeSpaces <- function(S) gsub("[[:space:]]","", S)

# }}}
# stringHelpers {{{
dotRenamer <- function(dd) {
    names(dd) <- gsub("^\\.+|\\.[^.]*$", "", names(dd), perl=TRUE)
    return(dd)
}
# }}} close stringHelpers
# misc xts helpers # {{{
# fill and extend an xts object using AR methods
# find the date difference of an xts date index
dayDiff <- function(X) {
    stopifnot(is.xts(X))
    as.numeric(as.Date(index(X))) - c(NA, as.numeric(as.Date(index(X[-nrow(X)]))))
}

# find bps per day
bpsDay <- function(X) {
    stopifnot(is.xts(X))
    as.numeric(diff(X)) / dayDiff(X)
}

# lapply across an xts object
na.extend <- function(obj, extend='both') {
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
                (
                 as.numeric(index(interped)[1:naBounds[1]] - index(interped)[naBounds[1] + 1])
                 * slopeStartEnd[1]
                 + as.numeric(obj[naBounds[1]+1,])
                 )
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
addObvAR <- function(XTS, iter = 1) {
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
# }}}
# looping helpers {{{
# lapply across an xts object
mapXts <- function(Xts, cFUN, ...) {
    if(!is.xts(Xts)) stop("Must supply function with xts object")
    "[<-"(Xts, ,vapply(Xts, cFUN, ..., FUN.VALUE = numeric(nrow(Xts))))
}
# }}}
# {{{ plot helpers

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

pngMk <- function(pngName, Wwidth = 480, Hheight = 480)
{
    png(file.path(plotPATH, pngName), width = Wwidth, height = Hheight)
}

makeTwins <- function(Xx, title = 'twin') {
    options(warn = -1)
    png(file = file.path(plotPATH, paste0(title, '_', names(Xx), ".png")))
    print(paste0(title, '_', names(Xx), ".png"))
    #
    Xx_yy <- 100*(Xx / lag(Xx, 4) - 1)
    Xx_qq <- 100*(Xx / lag(Xx) - 1)
    lineTitle <- paste0(names(Xx)[1], ": %YoY & QoQ%")
    gp_RP_PXline <- ggplot(subset(meltx(Xx_yy), variable == names(Xx)[1]),
                           aes( x = date, y = value)) +
theme_grey() +
labs(y = NULL, x = NULL) +
labs(title = lineTitle) +
theme(legend.position = 'none') +
theme(legend.title = element_blank()) +
geom_line(color = 'blue')
#
gp_RP_diffbar <- ggplot(subset(meltx(Xx_qq), variable == names(Xx)),
                        aes( x = date, y = value)) +
theme_grey() +
labs(y = NULL, x = NULL) +
theme(legend.position = 'none') +
theme(legend.title = element_blank()) +
geom_bar(stat = 'identity', color = 'red', fill = 'red')
#
grid.arrange(gp_RP_PXline, gp_RP_diffbar, heights = c(2/3, 1/3),
             sub = textGrob('www.ricardianambivalence.com'))
dev.off()
}
# }}}
# {{{ pasteboard functions
fcb <- function(hh = TRUE)
{
    switch(Sys.info()[['sysname']],
           Darwin = { dd <- read.table(pipe('pbpaste'), sep = "\t", header=hh, as.is=TRUE)},
           Windows = {dd  <- read.table('clipboard', sep = "\t", header=hh, as.is=TRUE)}
           )
    return(dd)
}

fcbx <- function(hh = TRUE) { xtsF( fcb() ) }
## }}}
# {{{  Date functions
dateSwitch <- function(index, lastDay = FALSE, adv = 0)
{
    index_LT <- as.POSIXlt(index)
    if(lastDay) {
        index_LT$mon <- index_LT$mon + (adv + 1)
        index_LT$mday <- 1
        index_LT <- as.Date(index_LT) - 1
        return(index_LT)
    } else {
        index_LT$mon <- index_LT$mon + adv
        index_LT$mday <- 1
        return(index_LT)
    }
}

XLdate <- function(Xd, type = 'b-Y')
{
    switch(type,
           'b-Y' = as.Date(paste0(substr(Xd, 5, 9), "-", substr(Xd, 1, 3), "-01"),
                           format = "%Y-%b-%d"),
           'b-y' = as.Date(paste0(year1900(substr(Xd, 5, 6)), "-", substr(Xd, 1, 3), "-01"),
                           format = "%Y-%b-%d"),
           'Y-b' = as.Date(paste0(substr(Xd, 1, 3), "-", substr(Xd, 5, 9), "-01"),
                           format = "%Y-%b-%d"),
           'd1-m-y' = as.Date(paste0('01-', substr(Xd, 3, 4), '-', year1900(substr(Xd, 6,7))),
                              format = "%d-%m-%Y"),
           'Y-b-d' = as.Date(Xd)
           )
}

year1900 <- function(dd_y, yrFlip = 50)
{
    dd_y <- as.numeric(dd_y)
    dd_y[dd_y > yrFlip] <- dd_y[dd_y > yrFlip] + 1900
    dd_y[dd_y < yrFlip] <- dd_y[dd_y < yrFlip] + 2000
    return(dd_y)
}

rewindX <- function(Xts, dayRew=1, fillNA = TRUE, last = TRUE, oldDates = TRUE)
{
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

#
dateCompX <- function(Xts, lagNum = 7, fillNA = TRUE, Yts = NULL)
{
    fillTest <- function(X){if(fillNA) na.locf(X) else X }
    stopifnot(is.xts(Xts),
              if(!is.null(Yts)) is.xts(Yts) else TRUE
              )
    if(is.null(Yts)) Yts <- Xts
    Xts - rewindX_fi(Yts,
                     dayRew = lagNum,
                     fillNA = fillNA,
                     last = FALSE,
                     oldDates = TRUE)
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
toQtrMons <- function(dateObj, toFirst=FALSE) {
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
qtr2Lower <- function(QD, lowerFreq, monShift = 0L, filler = "na.approx") {
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

# end date stuff }}}
# {{{ SA a matrix
mj_SAmat_m <- function(dfx, TO = 1, outGet = 'adjust', BUG = FALSE){
    pckReq('timsac')
    SAmat <- dfx
    for (i in 1:ncol(dfx)){
        begin <- min(which(!is.na(dfx[,i])))
        beginYr <- as.POSIXlt(index(dfx[begin]))$year + 1900
        beginMth <- as.POSIXlt(index(dfx[begin]))$mon + 1
        end <- max(which(!is.na(dfx[,i])))
        dat <- dfx[begin:end,i]
        if(BUG) print(paste('adjusting', names(dfx)[i]))
        sa <- baysea(coredata(dat), period=12, year=beginYr, month=beginMth,
                     span=12, shift=1, out=0, trend.order = TO, plot=FALSE)
        firstNA <- rep(NA, (begin-1))
        lastNA <- rep(NA, (nrow(dfx)-end))
        SAmat[,i] <- c(firstNA, get(outGet, sa), lastNA)
    }
    return(SAmat)
}

## takes an xts with some sa and some nsa -- with _nsa marking the NSA data.
## adjusts the subset marked "_nsa" and renames with suffix "(sa)"
## Convention: _nsa is NSA, _t is trend, " (sa)" means we've done it & no _# means SA'd at source

al_subsetting_sa <- function(x, to=1){
    subset_adjusting <- x[,grep("_nsa$", names(x))]
    x[,grep("_nsa$", names(x))] <- mj_SAmat_m(subset_adjusting, to=to)
    x_names <- grep("_nsa$", names(x))
    names(x)[x_names] <- paste(substr(names(x)[x_names], 1, nchar(names(x)[x_names])-4), " (sa)", sep="")
    return(x)
}

## this wrapper looks to see if you have an xts object, if you do, s.a. it.
## if not, look to see if the first column of the dataframe is in POSIXct format,
## make it an xts object and SA.

al_easySA <- function(x){
    if (class(x)[1] == "xts"){
        x <- subsetting_sa(x)
    } else {
        if (class(x[,1])[1] == "POSIXct"){
            x <- xts(x[,-1], order.by=as.Date(x[,1]))
            x <- subsetting_sa(x)
        }
    }
    return(x)
}

# }}}
# VAR helpers {{{
## VAR helpers require(vars)
# find the lag length - crit in {'AIC(n)', 'HQ(n)', 'SC(n)', 'FPE(n)'}
findMaxVARLag <- function(varData, firstMax=12, crit = "SC(n)")
{
    pckReq('vars')
    maxLag <- firstMax
    repeat {
        lagSelect <- VARselect(varData, lag.max=maxLag)
        if (lagSelect$selection[crit] < maxLag) {
            maxLag <- maxLag - 1L
        } else break
    }
    return(maxLag)
}

# measure POOS performance of a VAR

testVar <- function(dframe, nAhead = 6, IC = 'SC',
                    periodicity = 'months',
                    skip = NULL, Vlag = 12, RSTmtx = NULL) {
    # setup
    if(is.null(skip)) { skip <- nrow(dframe) %/% 2 }
    dateList <- index(dframe)
    ttlRows <- length(dateList) + nAhead
    extraDate <- seq(index(dframe[1]), by=periodicity, length.out=ttlRows)
    # make extended variables
    testPack <- list()
    for (i in 1:ncol(dframe))
    {
        testPack[[names(dframe)[i]]] <-
            as.xts(cbind(c(coredata(dframe[,i]), rep(NA, nAhead)),
                         matrix(rep(NA, ttlRows * (length(dateList) - skip)),
                                nrow = ttlRows, ncol = (length(dateList) - skip)
                                )
                         ), order.by = extraDate
        )
    }
    # the test loop
    for (d in (skip+1):length(dateList)) {
        testName <- paste0('from:', dateList[d]) # need to stick this on later
        if (is.null(RSTmtx))
        {
            optLag <- findMaxVARLag(dframe[1:d,], firstMax= Vlag, crit = paste0(IC, "(n)"))
            mod <- VAR(dframe[1:d,], p = optLag) # we may want to change the IC part later
        } else {
            Vlag <- (ncol(RSTmtx) - 1) / (nrow(RSTmtx))
            mod <- VAR(dframe[1:d,], p = Vlag)
            mod <- restrict(mod, method = 'manual', resmat = RSTmtx)
        }
        poosPred <- predict(mod, n.ahead=nAhead)
        #
        for (v in 1:ncol(dframe)) # loop across by column type
        {
            outvec <- rep(NA, ttlRows) # allocate the full length vector
            outvec[(d + 1):(d + nAhead)] <- poosPred$fcst[[v]][,1] # replace with 4casts
            testPack[[v]][, (d - skip + 1)] <- outvec # one variable at a time
        }
    }
    testPack
}

# works with the above, to sum POOS rmse
errTstVar <- function(testRslt)
{
    rmsError <- testRslt # copy object to pre-allocate
    for (v in 1:length(testRslt))
    {
        for (i in 2:ncol(testRslt[[v]]))
        {
            rmsError[[v]][,i] <- sqrt((rmsError[[v]][,1] - rmsError[[v]][,i])**2)
        }
        rmsError[['rmsfe']][[names(testRslt)[v]]] <- sum(rmsError[[v]], na.rm=TRUE)
    }
    return(rmsError)
}

varsPredictNewData <- function(varsMODEL, varsDATA, projFWD = 1)
{
    varsDATA_pp <- varsDATA  # make a frame to extend
    # make the coeff vectors
    for (i in seq_along(get('varresult', varsMODEL)))
    {
        assign(paste0('modCoeffs_v', i),
               get('varresult', varsMODEL)[[i]]$coeff
               )
    }
    # find max lag length -- from coeff vector
    maxLag <- max(as.numeric(unlist(regmatches(names(modCoeffs_v1),
                                               gregexpr('\\(?[0-9]+',
                                                        names(modCoeffs_v1))))))
    n = 1
    # make an expanded DF so you can %*% using coeff matrix
    while (n < (1+projFWD))
    {
        env4step <- new.env()
        expandedDF <- varsDATA_pp # the df we build up
        for (j in 1:(maxLag-1))
        {
            expandedDF <- merge(expandedDF, lag(varsDATA_pp, j))
        }
        expandedDF$const <- 1
        for (k in seq_along(get('varresult', varsMODEL)))
        {
            assign(paste0('v', k, '_pp'),
                   tail(expandedDF, 1) %*% get(paste0('modCoeffs_v', k)),
                   envir = env4step)
        }
        # join the forecasts together
        vvlist = list() # an empty list to contain the variable names
        for (l in seq_along(get('varresult', varsMODEL)))
        {
            vvlist <- c(vvlist, paste0('v', l, '_pp'))
            l = l + 1
        } # this loop puts the names into the list ... use environments?
        print(vvlist)
        # now put it together
        nextDate <- seq(last(index(varsDATA_pp)), by = 'mon', length.out = 2)[-1]
        newRow <- xts(do.call(cbind, lapply(vvlist, get, envir = env4step)),
                      order.by = nextDate)
        names(newRow) <- names(varsDATA_pp)
        varsDATA_pp <- rbind(varsDATA_pp, newRow)
        rm(expandedDF)
        rm(list = ls(env4step), envir = env4step)
        n = n + 1
    }
    return(varsDATA_pp)
}

# VAR plot function -- POOS spider plots

spiderPOOS <- function(POOStest, series, startYr = 1993, MAINstring = NULL)
{
    startYrFlag <- paste0(startYr, '::')
    plot.zoo(get(series, POOStest)[startYrFlag],
             screen=1,
             col=c(1, rep(8, ncol(get(series, POOStest)-1))),
             las=1,
             lwd = c(3, rep(1, ncol(get(series, POOStest)) - 1)),
             type = c('s', rep('l', ncol(get(series, POOStest)) - 1)),
             main = MAINstring,
             xlab = "",
             ylab = ""
             )
}

# get the things you want from the VAR modelling step, and place in global env
varSuite <- function(VARframe, dateRange, initMax = 9, infoCrit = "FPE", castAhead = 6)
{
    assign(paste0(VARframe, ".optLag"),
           findMaxVARLag(get(VARframe, envir = globalenv())[dateRange],
                         firstMax = initMax, crit = paste0(infoCrit, "(n)")),
           envir = globalenv()
           )
    assign(paste0(VARframe, ".test", castAhead),
           testVar(get(VARframe, envir = globalenv())[dateRange], skip = 94,
                   nAhead = castAhead, Vlag = 6, IC = infoCrit),
           envir = globalenv()
           )
    assign(paste0('sumTestError.', VARframe),
           errTstVar(get(paste0(VARframe, ".test", castAhead),
                         envir = globalenv()
                         )
           ),
           envir = globalenv()
           )
    assign(paste0(VARframe, ".mod"),
           VAR(scale(get(VARframe, envir = globalenv())[dateRange]),
               p = get(paste0(VARframe, ".optLag"), envir = globalenv()),
               ic = infoCrit),
           envir = globalenv()
           )
    assign(paste0(VARframe, ".mod2"),
           VAR(get(VARframe, envir = globalenv())[dateRange],
               p = get(paste0(VARframe, ".optLag"), envir = globalenv()),
               ic = infoCrit),
           envir = globalenv()
           )
}
# }}} close VAR helpers
# bbgHelpers {{{
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
# }}} close bbgHelpers
