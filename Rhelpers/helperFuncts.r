# MCJ's R-helper functions

# {{{ general helpers

xtsF <- function(x)
{
# add a smart date switch: http://stackoverflow.com/questions/6194285/dealing-with-messy-dates/7975560#7975560
    ddx <- xts(x[,-1], order.by = as.POSIXct(x[,1]))
    index(ddx) = make.index.unique(index(ddx))
    return(ddx)
}

meltx <- function(dx) { melt(data.frame(date = index(dx), dx), id.vars = 1) }

x2df <- function(XTS) {data.frame('date' = index(XTS), data.frame(coredata(XTS)))}

"%notin%" <- function(x, y) x[!x %in% y]

# }}}

# {{{ plot helpers

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

# }}}

# {{{ pasteboard functions
fcb <- function(hh = TRUE)
{
    switch(Sys.info()[['sysname']],
           Darwin = { dd <- read.table(pipe('pbpaste'), sep = "\t", header=hh, as.is=TRUE)},
           Windows = {dd  <- read.table('clipboard', sep = "\t", header=hh, as.is=TRUE)}
           )
    return(dd)
}
## }}}

# {{{  Date functions
dateSwitch <- function(index, lastDay = TRUE, adv = 0)
{
    index_LT <- as.POSIXlt(index)
    if(lastDay) {
        index_LT$mon <- index_LT$mon + (adv + 1)
        index_LT$mday <- 1
        index_LT <- as.Date(index_LT) - 1
        return(index_LT)
    } else {
        index_LT$mon <- index_LT$mon + adv
        index_LT$mday <- 1
        return(index_LT)
    }
}

# end date stuff }}}

# {{{ SA a matrix
mj_SAmat_m <- function(dfx, TO = 1){
  SAmat <- dfx
  for (i in 1:ncol(dfx)){
    begin <- min(which(!is.na(dfx[,i])))
    beginYr <- as.POSIXlt(index(dfx[begin]))$year + 1900
    beginMth <- as.POSIXlt(index(dfx[begin]))$mon + 1
    end <- max(which(!is.na(dfx[,i])))
    dat <- dfx[begin:end,i]
    sa <- baysea(coredata(dat), period=12, year=beginYr, month=beginMth,
                 span=12, shift=1, out=0, trend.order = TO, plot=FALSE)
    firstNA <- rep(NA, (begin-1))
    lastNA <- rep(NA, (nrow(dfx)-end))
    SAmat[,i] <- c(firstNA, sa$adjust, lastNA)
  }
  return(SAmat)
}
# }}}
