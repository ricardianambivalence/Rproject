
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

# rewind an xts object by n days
rewindX_fi <- function(Xts, dayRew=1, fillNA = TRUE, last = TRUE){
    # rewinds an Xts object by dayRew days
    lastFlip <- function(X) { if(last) {last(X)} else X }
    stopifnot(is.xts(Xts))
    newDates <- index(Xts) - dayRew
    rewindRows <- findInterval(newDates, index(Xts))
    nonZeros <- rewindRows[rewindRows > 0]
    Xts_rew <- if(fillNA) {
        na.locf(Xts, na.rm = FALSE)[nonZeros,]
    } else {
        Xts_rew <- Xts[nonZeros, ]
    }
    lastFlip(merge(xts(NULL, newDates), Xts_rew))
}

# old rewindX function
rewindX <- function(Xts, dayRew = 1)
{
    flipDates <- function(dayRew)
    {
        newDate <- as.Date(index(last(Xts))) - dayRew
        assign('newX', Xts[newDate], envir = outXenv)
        if(!length(which(!is.na(outXenv$newX)))) 
        {
            dayRew <- dayRew + 1
            flipDates(dayRew)
        }
    }
    outXenv <- new.env(parent = .GlobalEnv)
    flipDates(dayRew)
    return(outXenv$newX)
}

# old dateCompX function
# use rewindX to diff by a number of days
dateCompX <- function(XDF, lagNum = 7, YDF = NULL)
{
    sameFrame <- function(i) 
    { 
        as.numeric(XDF[i,]) - as.numeric(rewindX(XDF[paste0("::", idx[i])], lagNum))
    }
    changeFrame <- function(i) 
    { 
        as.numeric(XDF[i,]) - as.numeric(rewindX(YDF[paste0("::", idx[i])], lagNum))
    }
    #
    diffFrame <- matrix(NA, nrow = nrow(XDF), ncol = ncol(XDF))
    idx = as.Date(index(XDF))
    for (i in 1:nrow(XDF)) {
        if (idx[i] < (head(idx,1) + lagNum)) next
        if (is.null(YDF)) { 
            diffFrame[i,] <- sameFrame(i) 
        } else {
            diffFrame[i,] <- changeFrame(i)
        }
    }
    DFx <- xts(diffFrame, order.by = idx)
    colnames(DFx) <- colnames(XDF)
    return(DFx)
}

# find the difference between two xts objects n days apart
dateCompX_fi <- function(Xts, lagNum = 7, fillNA = TRUE, Yts = NULL){
    fillTest <- function(X){if(fillNA) na.locf(X) else X }
    stopifnot(is.xts(Xts),
              if(!is.null(Yts)) is.xts(Yts) else TRUE
              )
    if(is.null(Yts)) Yts <- Xts
    newDates <- index(Xts) - lagNum
    targetRows_Y <- findInterval(newDates, index(Yts))
    zeros <- which(targetRows_Y == 0)
    nonZeros <- targetRows_Y[targetRows_Y > 0]
    nonZeroRowDiff <- coredata(fillTest(Xts[-zeros,])) -
                        coredata(fillTest(Yts[nonZeros,]))
    xts(nonZeroRowDiff, order.by = index(Xts)[-zeros])
}

# fill and extend an xts object using AR methods
na.ARextend <- function(Xts, ARorder = 1, ADD = 0, window = 120){
    stopifnot(ncol(Xts) == 1)
# make the regression matrix to regress on
    makeRegMatrix <- function() {
        dataWindow <- do.call(cbind, lapply(0:ARorder, 
                                            function(X) tail(lag(Xts_trim, X), 
                                                             window)))
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

require(xts)
set.seed(1)
#
ddf <- data.frame('1m' = rnorm(25), '3m' = rnorm(25))
xxd <- xts(ddf, seq(as.Date('2013-07-27'), length.out = 25, by='day'))
xxd[sample(1:25, 8), ] <- NA
xxd <- xxd[-sample(1:25, 3), ]
#
xxd_ext <- merge(
                 xts(NULL, order.by = index(last(xxd)) + 1:5),
                 xxd)

system.time(na.ARextend(xxd_ext[,1]))


do.call(cbind, lapply(1:ncol(xxd_ext), function(N) na.ARextend(xxd_ext[,N])))


# needs a fillUp method
set.seed(10)
tt <- seq(Sys.Date(), length.out=10, by='day')
x <- xts(seq(1, 10), order.by = tt)
x[sample(1:10, 3)] <- NA
z <- xts(seq(2, 20, 2) + rnorm(10), order.by=tt)
#
fillXZ_dev <- function(X, Z, LOG = TRUE, DOWN = TRUE) {
    # helper function
    returner <- function() {
        if(LOG) {
            return(exp(X))
        } else { 
            return(X)
        }
    }
    fillDown <- function(X) {
        for (dd in seq_along(missingDates)) {
            X[missingDates[dd]] <- lag(X)[missingDates[dd]] + diff(Z)[missingDates[dd]]
        }
        X
    }
    fillUp <- function(X) {
        for (dd in rev(seq_along(missingDates))) {
            X[missingDates[dd]] <- 
        X
    }
    if(LOG) {
        X <- log(X)
        Z <- log(Z)
    }
    missingDates <- index(X)[which(is.na(X))]
    if(length(missingDates) == 0) return(returner())
    if(DOWN) {
        X <- fillDown(X) 
    } else {
       X <- fillUp(X)
    }
    print('function ending')
    return(returner())
}
#
debug(fillXZ_dev)
x2 <- fillXZ_dev(x, z)
x2[1:3, ] <- NA


# needs a fillUp method
set.seed(10)
tt <- seq(Sys.Date(), length.out=10, by='day')
x <- xts(seq(1, 10), order.by = tt)
x[sample(1:10, 3)] <- NA
z <- xts(seq(2, 20, 2) + rnorm(10), order.by=tt)
#

fillXZ_dev <- function(X, Z, LOG = TRUE, DOWN = TRUE, dateRange = NULL) {
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
    xDates <- index(X)
    diffZ <- diff(Z)
    if(DOWN) {
        X <- fillDown(X)
    } else {
       X <- fillUp(X)
    }
    return(returner())
}

#
debug(fillXZ_dev)
x2 <- fillXZ_dev(x, z)
x2[1:3, ] <- NA
x3 <- fillXZ_dev(x2, z, DOWN = FALSE)
