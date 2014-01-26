Sys.setenv(TZ = 'UTC')

# packages and functions
require(quantmod)
source('~/Rproject/Rhelpers/helperFuncts.r')

# set paths
PATH <- "~/Rproject/usd/rates"
plotPath <- file.path(PATH, 'plot')
codePath <- file.path(PATH, 'code')
dataPath <- file.path(PATH, 'data')

getData <- TRUE

# Step 1: Get the data
if(getData){
    dataNames <- c('DTB4WK', 'DTB3')
    getSymbols(dataNames,src='FRED', return.class = 'xts')
    bills <- cbind(DTB4WK, DTB3)
} else {
    load(file = "~/Rproject/tech/fx/aud/data/AUDUSD.RData")
}

# add latest data
today <- xts(data.frame(DTB4WK = 0.33, DTB3 = 0.05), order.by = as.Date("2013-10-08"))
bills <- rbind(bills, today)
names(bills) <- c('tbill_4w', 'tbill_3m')
bills$spread <- with(bills, tbill_3m - tbill_4w)

# plot
RAstamp <- paste("ricardianambivalence.com  ", format(Sys.time() + 60*60*24, "%d-%b-%Y"))

png(file.path(plotPath, "usTbills.png"))
par(mar=c(3.5, 4.5, 2, 1), oma=c(1,0,0,0))
plot.zoo(bills['2011/'],
         main = "US Treasury Bills",
         xlab = ""
         )
mtext(RAstamp, cex=0.75, line=0, side=1, adj=1, outer=T)
mtext("Source: FRED", cex=0.75, side=1, adj=0, outer=T)
dev.off()
