require(xts)
set.seed(1)
#
ddf <- data.frame('1m' = rnorm(25), '3m' = rnorm(25))
xxd <- xts(ddf, seq(as.Date('2013-07-27'), length.out = 25, by='day'))
xxd[sample(1:25, 8), ] <- NA
xxd <- xxd[-sample(1:25, 3), ]

xxd_ext <- merge(
                 xts(NULL, order.by = index(last(xxd)) + 1:5),
                 xxd)

# roll back xts across missing and NA rows

# rewind an XTS object - returns most recent non-null row
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

tt <- sample(seq(Sys.Date(), by = 'day', length.out = 100), 20)
dd <- rnorm(length(tt))

xd <- xts(dd, tt)


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
na.ARextend <- function(Xts, ARorder = 1, ADD = NULL, window = 120){
    Xts_filled <- na.approx(Xts, na.rm=FALSE)
    Xts_trim <- Xts_filled[complete.cases(Xts_filled),]
    regMat <- do.call(cbind,
                      lapply(0:ARorder, function(X) tail(lag(Xts_trim, X), window)))
    regMat <- cbind(regMat[,1],
                    rep(1, nrow(regMat)),
                    regMat[, 2:ncol(regMat)]
                    )[-c(1:ARorder), ]
    coco <- lm.fit(x = regMat[,1], y = regMat[, -1])$coefficients
}

(na.ARextend(xxd_ext[,1]))


# rolling regression inside data.table
require(data.table)
set.seed(1)
x  <- rnorm(1000)
DT <- data.table(x)
DTin <- data.table(x)

lagDT <- function(DTin, varname, l=5)
{
    i = 0
    while ( i < l){
        expr <- parse(text =
                      paste0(varname, '_L', (i+1),
                             ':= c(rep(NA, (1+i)),',
                             varname, '[-((length(',
                             varname, ') - i):length(', varname, '))])'
                             ))
        DTin[, eval(expr)]
        i <- i + 1
    }
    return(DTin)
}

rollRegDT <- function(DTin, varname, k=20, l=5)
{
    adj <- k + l - 1
    .x <- 1:(nrow(DTin)-adj)
    DTin[, int:=1]
    dtReg <- function(dd) coef(lm.fit(y=dd[-c(1:l),1], x=dd[-c(1:l),-1]))
    eleNum <- nrow(DTin)*(l+1)
    outMatx <- matrix(rep(NA, eleNum), ncol = (l+1))
    colnames(outMatx) <- c('intercept', 'L1', 'L2', 'L3', 'L4', 'L5')
    for (i in .x){
        dt_m <- as.matrix(lagDT(DTin[i:(i+adj), ], varname, l))
        outMatx[(i+(adj)),] <- dtReg(dt_m)
    }
    return(outMatx)
}

rollCoef <- rollRegDT(DT, varname='x')

mm <- lm(xxd[2:11,1] ~ xxd[1:10,1])
ir = lm(Sepal.Length~ Petal.Length, data=iris)
