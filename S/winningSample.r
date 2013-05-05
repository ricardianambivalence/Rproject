library(quantmod)
library(multicore)

getSymbols("^GSPC", from = "1950-1-1")
ret <- ROC(GSPC)[-1,6]

set.seed(123)

winpct <- seq(0.5, 0.6, 0.01)
ret <- coredata(ret)

system.time(res <- simplify2array(mclapply(winpct, function(x) replicate(1000, drawsample(ret, x)))))
system.time(res <- simplify2array(mclapply(winpct, function(x) replicate(1000, drawsample2(ret, x)))))

drawsample <- function(ret, winpct){
  len = length(ret)
  ret = abs(ret)
  win = sample(1:len, round(winpct * len))
  a = c(ret[win], -ret[-win])
  return(prod(1 + a) ^ (252 / length(a)) - 1)
}

drawsample2 <- function(ret, winpct){
  len = length(ret)
  win = sample(c(-1,1), len, replace=TRUE, prob = c((1-winpct), winpct))
  ret <- abs(ret)
  bin <- ret*win
  return(exp(sum(bin))^(252/length(ret)) - 1)
}

bb <- benchmark(simplify2array(mclapply(winpct, function(x) replicate(1000, drawsample(ret, x)))),
           simplify2array(mclapply(winpct, function(x) replicate(1000, drawsample2(ret, x)))),
           columns =c('test', 'elapsed', 'relative'),
          replications = 10,
          order = 'elapsed')
