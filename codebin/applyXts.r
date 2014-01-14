# install packages
mrip(c('microbenchmark', 'rbenchmark'))
#
# function defs ::
mapXts <- function(Xts, cFUN, ...) {
    if(!is.xts(Xts)) stop("Must supply function with xts object")
    Z <- Xts
    for (j in 1:ncol(Xts)) {
        Z[,j] <- do.call(cFUN, list(Xts[,j], ...))
    }
    Z
}
#
mapXts_noC <- function(Xts, cFUN, ...) {
    if(!is.xts(Xts)) stop("Must supply function with xts object")
    for (j in 1:ncol(Xts)) {
        Xts[,j] <- do.call(cFUN, list(Xts[,j], ...))
    }
    Xts
}
#
mapXts_v <- function(Xts, cFUN, ...) {
    if(!is.xts(Xts)) stop("Must supply function with xts object")
    "[<-"(Xts, , vapply(Xts, cFUN, ..., FUN.VALUE = numeric(nrow(Xts))))
}
# "[<-"(xz, , vapply(xz, function(col) col + 100, FUN.VALUE = numeric(nrow(xz))))
# function for use in test
mff <- function(x, y) x + rnorm(y)
#
# benchmarking ::
set.seed(1)
xz <- xts(replicate(6, sample(c(1:100), 1000, rep = T)),
          order.by = Sys.Date() + 1:1000)
apFun <- function() apply(xz, 2, mff, 1)
sapFun <- function() sapply(xz, mff, 1)
mapXf <- function() mapXts(xz, mff, 1)
mapXf2 <- function() mapXts_noC(xz, mff, 1)
mapXv <- function() mapXts_v(xz, mff, 1)
vapFun <- function() "[<-"(xz, , vapply(xz, mff, 1,
                                        FUN.VALUE = numeric(nrow(xz))))
vapFun2 <- function() xts(vapply(xz, mff, 1,
                                 FUN.VALUE = numeric(nrow(xz))),
                          order.by = time(xz))
vapply(xz, function(col) col + 100, FUN.VALUE = numeric(nrow(xz)))
#
op <- microbenchmark(
                     app = apFun(),
                     sap = sapFun(),
                     mXf = mapXf(),
                     mXf2 = mapXf2(),
                     mXv = mapXv(),
                     vap = vapFun(),
                     vap2 = vapFun2(),
                     times = 1000L)
#
op2 <- benchmark(apFun(),
                 sapFun(),
                 mapXf(),
                 mapXf2(),
                 mapXv(),
                 vapFun(),
                 vapFun2(),
                 columns = c('test', 'elapsed', 'relative'),
                 replications = 1000,
                 order = 'elapsed')
