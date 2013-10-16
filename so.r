require(rbenchmark)

set.seed(1)
Z<-matrix(sample(1:40),ncol=4)

colnames(Z)<-c("value","A","B","C")

process <- function(x) {
    (x[["value"]] - mean(c(x[["A"]], x[["B"]], x[["C"]]))) / sd(c(x[["A"]], x[["B"]], x[["C"]]))
}

p2 <- function(x) {
    (x[1] - mean(x[-1])) / sd(x[-1])
}

apply_fun <- function() apply(Z, 1, process)
apply_fun2 <- function() apply(Z, 1, p2)

apply_sd <- function() (Z[,1] - rowMeans(Z[,-1])) / apply(Z[, -1], 1, sd)

vapply_anon <- function() vapply(1:nrow(Z), FUN = function(X) (Z[X, 1] - mean(Z[X, -1])) / sd(Z[X, -1]),
                FUN.VALUE = numeric(1))


bb <- benchmark(apply_fun(), apply_fun2(), apply_sd(), vapply_anon(),
          columns = c('test', 'elapsed', 'relative'),
          replications = 100,
          order = 'elapsed')
