# fib benchmarks
require(rbenchmark)
require(lambda.r)
require(compiler)

Fib <- function(n)
{
    if(n < 2) return(n)
    Fib(n-1) + Fib(n-2)
}

cFib <- cmpfun(Fib)

FibG <- function(n)
{
    if (n == 0) return(0)
    if (n %in% c(1:2)) 1
    else {
        y <- rep(1, n)
        for (i in 3:n) y[i] <- y[i-1] + y[i-2]
        y[n]
    }
}
cFibG <- cmpfun(FibG)

Fib_lr(0) %as% 0
Fib_lr(1) %as% 1
Fib_lr(n) %as% {
    Fib_lr(n-1) + Fib_lr(n-2)
}

cFib_lr <- cmpfun(Fib_lr)

FibG_lr(0) %as% 0
FibG_lr(1) %as% 1
FibG_lr(2) %as% 1
FibG_lr(n) %as% {
    y <- rep(1, n)
    for (i in 3:n) {
        y[i] <- y[i-1] + y[i-2] }
    y[n]
}

cFibG_lr <- cmpfun(FibG_lr)

fl(0) %as% 0
fl(n) %as% {sum(choose(n - 1:n, 1:n - 1))}
seal(fl)

cfl <- cmpfun(fl)

f <- function(n){
    k=1:n
    ifelse(n<1,0,sum(choose(n-k,k-1)))
}

cf <- cmpfun(f)

bb <- benchmark(sapply(1:30, Fib),
                sapply(1:30, FibG),
                sapply(1:30, FibG_lr),
                sapply(1:30, f),
                sapply(1:30, cFib),
                sapply(1:30, cFibG),
                sapply(1:30, cFibG_lr),
                sapply(1:30, cf),
                sapply(1:30, fl),
                sapply(1:30, cfl),
          columns = c('test', 'replications', 'elapsed', 'relative'),
          order = 'elapsed',
          replications = 10
          )
print(bb)

                    # test replications elapsed  relative
# 8       sapply(1:30, cf)           10   0.003     1.000
# 6    sapply(1:30, cFibG)           10   0.003     1.000
# 4        sapply(1:30, f)           10   0.004     1.333
# 2     sapply(1:30, FibG)           10   0.011     3.667
# 3  sapply(1:30, FibG_lr)           10   0.044    14.667
# 7 sapply(1:30, cFibG_lr)           10   0.046    15.333
# 1      sapply(1:30, Fib)           10  82.471 27490.333
# 5     sapply(1:30, cFib)           10  92.327 30775.667
