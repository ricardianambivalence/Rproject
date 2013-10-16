# aim -- to make a function that fills in the gaps in the middle and end of an xts using AR(n)
tt <- seq(Sys.Date(), length.out=315, by='day')
ttp <- seq(Sys.Date(), length.out=325, by='day')

px1 <- xts(sin(0:314) + rnorm(315), tt)
px2 <- xts(sin(0:314) + rnorm(315), tt)
px3 <- xts(sin(0:314) + rnorm(315), tt)
px4 <- xts(sin(0:314) + rnorm(315), tt)


