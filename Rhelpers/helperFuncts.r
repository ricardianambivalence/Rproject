# MCJ's R-helper functions

# {{{ general helpers

"%notin%" <- function(x, y) x[!x %in% y]

`%between%` <- function(x, rng) x >= rng[1] & x <= rng[2]

mrip <- function(..., install = TRUE){
    reqFun <- function(pack) {
        if(!require(pack, character.only = TRUE)) {
            install.packages(pack)
            require(pack)
        }
    }
    lapply(..., reqFun)
}

is.between <- function(x, l, u) {
    b <- c(l, u)
    x %between% b
}

pckReq <- function(pckName) {
    if(!paste0('package:', pckName) %in% search())
        require(pckName, character.only=TRUE, quietly = TRUE)
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
# {{{ VAR helpers
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

testVar <- function(dframe, nAhead = 6, IC = 'SC', periodicity = 'months', skip = NULL, Vlag = 12, RSTmtx = NULL)
{
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
    for (d in (skip+1):length(dateList))
    {
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
    return(testPack)
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
    startYrFlag <- paste0(startYR, '::')
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
# }}}
