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

na.arfill <- function(XTS){
    which(is.na(px1))[which(is.na(px1)) > 30]
}
