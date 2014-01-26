# hadley wickham advanced R exercises

# subsetting

# mtcars[mtcars$cyl = 4,]
mtcars[mtcars$cyl == 4,]

# mtcars[-1:4, ]
mtcars[-c(1:4), ]

# mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl <= 5,]

# mtcars[mtcars$cyl == 4 | 6, ]
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]

# why does x <- 1:5; x[NA] yield five missing values?
x <- 1:5; x[NA]
# 1/ a missing value always yields a missing value;
# 2/ str(NA) shows that NA is logical, whereas str(NA_real_) is a number
# logical values are repeated and numerical values are not, so
# x[NA] returns NA five times, whereas x[NA_real_] returns one NA

# what does upper.tri() return?
# a logical matrix, wtih true values only above the diagonal.
# how does subsetting a matrix work?
# the matrix is subsetted with T and F values; each for a location.
# if the logical vector is shorter the matrix (nrow(x) * ncol(x))  <- recycling
# where the indexing logicals are not a matrix, the columns of the matrix are
# joined end to end (column major storage order) and indexing then applied.

#

scale01 <- function(x){
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}

vapply(cars, function(x) scale01(x), numeric(nrow(cars)))

testDF <- data.frame(replicate(4, runif(10)), letters[1:10])

scaleNumeric <- function(x){
    if(class(x) == 'numeric') {
        scale01(x)
    } else x
}

testDF[] <- lapply(testDF, scaleNumeric)

formulas <- list(
                 mpg ~ disp,
                 mpg ~ I(1/disp),
                 mpg ~ disp + wt,
                 mpg ~ I(1/disp) + wt
                 )

mOut <- lapply(formulas, function(M) lm(M, data = mtcars))
mOut[[1]]

outL <- list()
for(m in seq_along(formulas)) {
    outL[[m]] <- lm(formulas[[m]], data = mtcars)
}

bootstraps <- lapply(1:10, function(i) {
                     rows <- sample(1:nrow(mtcars), rep = TRUE)
                     mtcars[rows, ]
})

fitMod <- function(df) lm(mpg ~ disp , data = df)
bootMods <- lapply(bootstraps, function(DF) lm(mpg ~ disp, data = DF))
bootMod2 <- lapply(bootstraps, fitMod)

rsq <- function(mod) summary(mod)$r.squared
r1 <- unlist(lapply(outL, rsq))
r2 <- unlist(lapply(bootMods, rsq))
r3 <- unlist(lapply(bootMod2, rsq))

# rolling stuff ...

rollmean <- function(x, n){
    out <- rep(NA, length(x))
    offset <- trunc(n/2)
    for (i in (offset + 1):(length(x) - n + offset + 1)) {
        out[i] <- mean(x[(i - offset):(i + offset)])
    }
    out
}

x <- seq(1, 3, length = 1e2) + runif(1e2)
plot(x)
lines(rollmean(x, 5), col = 'blue', lwd=2)
lines(rollmean(x, 10), col = 'red', lwd=2)

xx <- 1:10
rollmean(xx, 3)

# exercises

# find the sd of every column in a numeric df

df <- data.frame(replicate(4, rnorm(10) + runif(10)))

vapply(df, sd, numeric(1))

# and now of every numeric element in df
df2 <- data.frame(replicate(4, rnorm(10) + runif(10)), let = sample(LETTERS, 10))
vapply(df2[1:4], sd, numeric(1))
vapply(df2[vapply(df2, function(X) "numeric" %in% class(X), logical(1))], sd, numeric(1))

trials <- replicate(100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE)

sapply(trials, function(tr) tr$p.value)
sapply(trials, `[[`, 'p.value')

