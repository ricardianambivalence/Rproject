# Date Functions
toLastDay <- function(dateObj, monAdv=0)
{
  tt <- as.POSIXlt(dateObj)
  tt$mday <- 1L
  tt$mon <- tt$mon + (monAdv + 1L)
  tt <- as.Date(tt) - 1L
  return(tt)
}

matchDate <- function(searchDate, dateList, roundDown=FALSE){
  if(roundDown) {
    dist2date <- as.Date(dateList) - as.Date(searchDate)
    closest <- which(max(dist2date[dist2date<=0]) == dist2date)
  } else {
    dist2date <- as.Date(dateList) - as.Date(searchDate)
    closest <- which(min(dist2date[dist2date>=0]) == dist2date)
  }
  return(dateList[closest])
}

closestDate <- function(searchDate, dateList, roundDown=FALSE){
  # add something that checks to see if reshape2 is loaded
  cDates <- melt(lapply(searchDate, FUN = function(x) matchDate(x, dateList)))[,1]
  return(cDates)
}

# handy misc

`%notin%` <- function(x,y) !(x %in% y)

dirMaker <- function(mainDir="S:/Rates Research/R_share/", subDir="seasplots")
{
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}

dotRenamer <- function(dd)
{
  for (i in 1:ncol(dd)){
    if (strsplit(names(dd)[i], "\\.")[[1]][1] == ""){
      names(dd)[i] <- strsplit(names(dd)[i], "\\.")[[1]][2]
    }
    else names(dd)[i] <- strsplit(names(dd)[i], "\\.")[[1]][1]
  }
  return(dd)
}

mmths <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
MMTHS <- toupper(mmths)

# source("S:/Rates Research/R_share/helpers/saMtx.r")

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
              if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
              stop("vectors must be same length")
              arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# get data from clipboard

frXL <- function(headArg=T) read.table('clipboard', header=headArg, sep="\t")

# paste data to the clipboard

toXL <- function(x) writeClipboard(as.character(x), sep = "\t")

# clever SO post on string replacement methods
String <- function(x="") {
  x <- as.character(paste(x, collapse=""))
  class(x) <- c("String","character")
  return(x)
}

"[.String" <- function(x,i,j,...,drop=TRUE) {
  unlist(strsplit(x,""))[i]
}
"[<-.String" <- function(x,i,j,...,value) {
  tmp <- x[]
  tmp[i] <- String(value)
  x <- String(tmp)
  x
}
print.String <- function(x, ...) cat(x, "\n")
#
# y <- rnorm(500, mean=1)
# ymat <- matrix(y, 100, 5)
# y.means <- colMeans(ymat)
# y.sd <- apply(ymat, 2, sd)
# barx <- barplot(y.means, names.arg=1:5, ylim=c(0, 1.5), col=3, axis.lty=1, xlab="Reps", ylab="Value")
# error.bar(barx, y.means, 1.96*y.sd/10)

## VAR helpers require(vars)
# find the lag length - crit in {'AIC(n)', 'HQ(n)', 'SC(n)', 'FPE(n)'}
findMaxVARLag <- function(varData, firstMax=12, crit = "HQ(n)")
{
  maxLag <- firstMax
  repeat {
    lagSelect <- VARselect(varData, lag.max=maxLag)
    if (lagSelect$selection[crit] < maxLag) {
      maxLag <- maxLag - 1L
    } else break
  }
  return(maxLag)
}
