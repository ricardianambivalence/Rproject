require(xts)
set.seed(1)

ddf <- data.frame('1m' = rnorm(25), '3m' = rnorm(25))
xxd <- xts(ddf, seq(as.Date('2013-07-27'), length.out = 25, by='day'))
xxd[sample(1:25, 8), ] <- NA
xxd <- xxd[-sample(1:25, 3), ]

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


# try findInterval approach
rewindX_fi <- function(Xts, dayRew=1){
    stopifnot(is.xts(Xts))
    newDate <- index(last(Xts)) - dayRew
    rewindRow <- findInterval(newDate, index(Xts))
    na.locf(Xts)[rewindRow, ]
}

