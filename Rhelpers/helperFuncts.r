# MCJ's R-helper functions

# {{{ general helpers

pckReq <- function(pckName)
{
    if(!paste0('package:', pckName) %in% search()) require(pckName, character.only=TRUE)
}

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

# read ABS sheet function
readABS <- function(XLS, SHEET = 'Data1', LineSkip = 9)
{
    pckReq('gdata')
    dat <- read.xls(XLS, sheet = SHEET, as.is=TRUE, skip = LineSkip)
    dat[,1] <- XLdate(dat[,1])
    names(dat)[1] <- 'date'
    return(xtsF(dat))
}

readClvFed <- function(URL, SHEET = 'Sheet1', LineSkip = 0, dateType = 'b-y')
{
    pckReq('gdata')
    dat <- read.xls(URL, sheet = SHEET, as.is=TRUE, skip = LineSkip)
    dat[,1] <- XLdate(dat[,1], type = dateType)
    names(dat)[1] <- 'date'
    return(xtsF(dat))
}

dfxColScl <- function(dfrm, pos=1, idx = 100)
{
    scaledDF <- dfrm
    for (i in 1:ncol(dfrm)) {
        scaledDF[, i] <- dfrm[,i] / as.numeric(dfrm[pos, i]) * idx
    }
    return(scaledDF)
}


# }}}

# {{{ plot helpers

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

pngMk <- function(pngName, Wwidth = 480, Hheight = 480)
{
    png(file.path(plotPATH, pngName), width = Wwidth, height = Hheight)
}

makeTwins <- function(Xx, title = 'twin') {
    options(warn = -1)
    png(file = file.path(plotPATH, paste0(title, '_', names(Xx), ".png")))
    print(paste0(title, '_', names(Xx), ".png"))
    #
    Xx_yy <- 100*(Xx / lag(Xx, 4) - 1)
    Xx_qq <- 100*(Xx / lag(Xx) - 1)
    lineTitle <- paste0(names(Xx)[1], ": %YoY & QoQ%")
    gp_RP_PXline <- ggplot(subset(meltx(Xx_yy), variable == names(Xx)[1]),
                                 aes( x = date, y = value)) +
                            theme_grey() +
                            labs(y = NULL, x = NULL) +
                            labs(title = lineTitle) +
                            theme(legend.position = 'none') +
                            theme(legend.title = element_blank()) +
                            geom_line(color = 'blue')
    #
    gp_RP_diffbar <- ggplot(subset(meltx(Xx_qq), variable == names(Xx)),
                                 aes( x = date, y = value)) +
                            theme_grey() +
                            labs(y = NULL, x = NULL) +
                            theme(legend.position = 'none') +
                            theme(legend.title = element_blank()) +
                            geom_bar(stat = 'identity', color = 'red', fill = 'red')
    #
    grid.arrange(gp_RP_PXline, gp_RP_diffbar, heights = c(2/3, 1/3),
                 sub = textGrob('www.ricardianambivalence.com'))
    dev.off()
}
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

fcbx <- function(hh = TRUE) { xtsF( fcb() ) }
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

XLdate <- function(Xd, type = 'b-Y')
{
    switch(type,
        'b-Y' = as.Date(paste0(substr(Xd, 5, 9), "-", substr(Xd, 1, 3), "-01"),
                        format = "%Y-%b-%d"),
        'b-y' = as.Date(paste0(year1900(substr(Xd, 5, 6)), "-", substr(Xd, 1, 3), "-01"),
                        format = "%Y-%b-%d"),
        'Y-b' = as.Date(paste0(substr(Xd, 1, 3), "-", substr(Xd, 5, 9), "-01"),
                        format = "%Y-%b-%d"),
        'd1-m-y' = as.Date(paste0('01-', substr(Xd, 3, 4), '-', year1900(substr(Xd, 6,7))),
                           format = "%d-%m-%Y"),
        'Y-b-d' = as.Date(Xd)
        )
}

year1900 <- function(dd_y, yrFlip = 50)
{
    dd_y <- as.numeric(dd_y)
    dd_y[dd_y > yrFlip] <- dd_y[dd_y > yrFlip] + 1900
    dd_y[dd_y < yrFlip] <- dd_y[dd_y < yrFlip] + 2000
    return(dd_y)
}

# end date stuff }}}

# {{{ SA a matrix
mj_SAmat_m <- function(dfx, TO = 1, outGet = 'adjust'){
  pckReq('timsac')
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
    SAmat[,i] <- c(firstNA, get(outGet, sa), lastNA)
  }
  return(SAmat)
}
# }}}
