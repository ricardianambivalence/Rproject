#{{{set-up Packs and Funs
Sys.setenv(TZ = 'UTC')
#
#packages and functions
require(gdata)
require(xts)
require(timsac)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(quantmod)
source("~/Rproject/Rhelpers/helperFuncts.r")
source("~/Rproject/Rhelpers/RAcolorpal.r")
# }}}close setup

# {{{ PATHstuff
projPATH <- file.path("~/Rproject/tech/rates/xmkt")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}close paths

getData <- TRUE
if(getData) load(file = file.path(dataPATH, "secList.RData"))


funDiffName <- function(XTS, loc, ff){
    # takes a list of XTS objects, a combination / location feed, and a function to apply
    # preserves names following the operation
    ffxy <- ff(XTS[[loc[1]]], XTS[[loc[2]]])
    names(ffxy) <- paste0(names(XTS[[loc[1]]]), "x", names(XTS[[loc[2]]]))
    ffxy
}

SecPx_diff <- combn(1:(length(SecPx_l) - 1), 2,
                    function(X) funDiffName(SecPx_l, X, `-`),
                    simplify = FALSE)


a<-c(18,NA,12,33,32,14,15,55)
b<-c(18,30,12,33,32,14,15,NA)
c<-c(16,18,17,45,22,10,24,11)
d<-c(16,18,17,42,NA,10,24,11)
data<- data.frame(rbind(a,b,c,d))
names(data)<-rep(1:8)

for(i in 1:nrow(data)) {
    missing <- which(is.na(data[i,]))
    if(i%%2) {
        data[i,missing] <- data[(i+1), missing]
    } else {
        data[i, missing] <- data[(i-1), missing]
    }
}
