# mapply idea
set.seed(1)
x1 = 1:5
x2 = 5:1
x3 = seq(2, 10, 2)
xl <- list(x1, x2, x3)
y1 = rnorm(5)
y2 = runif(5)
y3 = seq(20, 12, -2)
yl <- list(y1, y2, y3)
z1 = rnorm(5)
z2 = runif(5)
z3 = seq(20, 12, -2) %% 3
zl <- list(z1, z2, z3)
LL <- list(xl, yl, zl)

mapply("-", LL[[1]], LL[[2]])
mapply("-", LL[[1]], LL[[3]])
mapply("-", LL[[2]], LL[[3]])

LLdiff <- list()
combs <- combn(1:length(LL), 2)
for (i in 1:ncol(combs)) {
    LLdiff[[i]] <- mapply("-", LL[[combs[,i][1]]], LL[[combs[,i][2]]])
}
