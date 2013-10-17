# aim -- to make a function that fills in the gaps in the middle and end of an xts using AR(n)
tt <- seq(Sys.Date(), length.out=315, by='day')
ttp <- seq(Sys.Date(), length.out=325, by='day')

px1 <- xts(sin(0:314) + rnorm(315), tt)
px2 <- xts(sin(0:314) + rnorm(315), tt)
px3 <- xts(sin(0:314) + rnorm(315), tt)
px4 <- xts(sin(0:314) + rnorm(315), tt)

randomNA <- function(X, n){
    nn <- sample(1:nrow(X), n)
    X[nn,] <- NA
    X
}

px1 <- merge(randomNA(px1, 20), xts(NULL, ttp))
px2 <- merge(randomNA(px2, 20), xts(NULL, ttp))
px3 <- merge(randomNA(px3, 20), xts(NULL, ttp))
px4 <- merge(randomNA(px4, 20), xts(NULL, ttp))

rr <- Reduce(merge, list(px1, px2, px3, px4))

# you can apply across an xts object like a matrix
# apply(rr, 2, last, 20)

na.fillAR <- function(XTS){
# check it's an xts
    stopifnot(is.xts(XTS))
# remove leading NAs
# note for later that 1:(first_nonNA-1) is the leading NA part
    first_nonNA <- min(which(!is.na(XTS)))
    XTS_trim <- XTS[first_nonNA:nrow(XTS)]
# next find location of all non-na rows in the leading and non-leading parts
    earlyNAs <- which(is.na(XTS_trim))[which(is.na(XTS_trim)) <= 30]
    deep_NAs <- which(is.na(XTS_trim))[which(is.na(XTS_trim)) > 30]
# fill initial NA's with some method
    XTS_trim[1:30] <- na.locf(XTS_trim[1:30])
}


addObvAR <- function(XTS, iter = 1){
# takes an XTS and returns the next observation as calc'd using AR(1)
    stopifnot(is.xts(XTS))
    if(iter == 0) {
        rbind(XTS, xts())
    } else {
        nextObvX <- xts(lm(XTS ~ lag(XTS))$coefficients %*% c(1, last(XTS)),
                        order.by = nextDate(XTS))
        XTS <- rbind(XTS, nextObvX)
        addObvAR(XTS, (iter - 1))
    }
}

xPeriodtoMon <- function(XTS){
    stopifnot(is.xts(XTS))
    switch(periodicity(XTS)$scale,
           monthly = 'month',
           quarterly = ' 3month'
           )
}

nextDate <- function(XTS){
    stopifnot(is.xts(XTS))
    freq <- xPeriodtoMon(XTS)
    nextDate <- seq(last(index(XTS)), by = freq, length.out = 2)[2]
    nextDate
}
