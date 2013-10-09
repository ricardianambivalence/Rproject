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

