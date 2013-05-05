rm(list=ls())
Sys.setenv(TZ='GMT')

require(RODBC)
require(missMDA)
require(xts)
require(ggplot2)
require(vars)

# fix the mortgage data
conMDB <- odbcConnectAccess("S:/Rates Research/autodata/AUD/RBA/Reserve Bank.mdb")
tableName <- "indicator_lending_rates"
query


# get the data from excel
conexcel <- odbcConnectExcel(xls.file="S:/Rates Research/data/MJ_AUD/PCAs/RBA M stack PCA.xls")
s1 <- 'intor'
dd <- sqlFetch(conexcel, s1)
odbcClose(conexcel)

dd <- dd[!is.na(dd$period),]

# fill in the blanks and do the PCA
tt <- as.Date(dd[,1])
ii <- imputePCA(dd[,-1], ncp=10)
pca_dat <- ii$completeObs
zPCA_f <- PCA(pca_dat, graph=FALSE)$ind$coord
f1 <- scale(zPCA_f[,1])

ff_x <- xts(scale(zPCA_f[, (1:2)]), order.by=tt)
colnames(ff_x) <- c('f1', 'f2')
len_ffx <- nrow(ff_x)
shortDF <- as.data.frame(ff_x[(len_ffx-60):len_ffx,])
shortDF$period <- as.Date(rownames(shortDF))

# write the completed dataset
write.csv(zPCA_f, file="P:/R/AUD_PCA/stack/stackout.csv")

# plot some (pretty) charts
pdf("S:/Rates Research/R_share/MCJ/R/AUD_PCA/stack/pics/stackpca_f1_long.pdf")
longStack <- qplot(tt, f1, geom = c('point', 'line'), span=0.05, xlab="m/yr", ylab="std devs from mean")
print(longStack)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/R/AUD_PCA/stack/pics/stackpca_f1_short.pdf")
shortStack <- qplot(shortDF$period, shortDF$f1, geom=c('point', 'line'), span=0.05, xlab="m/yr", ylab="std devs from mean")
print(shortStack)
dev.off()

# a simple VAR -- the factors and the cash rate

# load the data from the .mdb
conMDB = odbcConnectAccess("S:/Rates Research/autodata/AUD/RBA/Reserve Bank.mdb")

tableName = "mmkt_yields"
query = paste("SELECT date, target, interbank FROM ",tableName,";",sep="")
QueryResult = sqlQuery(conMDB, query) #Queries Data from DB

odbcClose(conMDB)  #Close connection to DB

RBA_x <- xts(QueryResult[,-1], order.by=as.Date(QueryResult[,1]))
RBA_x <- round(RBA_x*4)/4

RBA_x$target['2008-10-31'] <- 6.0
RBA_x$interbank['2008-10-31'] <- 6.0
                        

toLastDay <- function(dateObj, monAdv=0)
{
  tt <- as.POSIXlt(dateObj)
  tt$mday <- 1
  tt$mon <- tt$mon + (monAdv + 1)
  tt <- as.Date(tt) - 1
  return(tt)
}

index(ff_x) <- toLastDay(index(ff_x))
index(RBA_x) <- toLastDay(index(RBA_x))

ff.RBA_x <- cbind(ff_x, RBA_x$interbank)
ff.RBA_x_trim <- ff.RBA_x[which(!is.na(ff.RBA_x$f1)),]
names(ff.RBA_x_trim) <- c('real_demand', 'financial', 'RBApolicy')

varmod <- VAR(ff.RBA_x_trim['::20120930'], lag.max=12, ic='SC')
summary(varmod)
cc <- causality(varmod)
print(cc)
prd.mod <- predict(varmod, n.ahead=18, ci= 0.95)
print(prd.mod)

pdf("S:/Rates Research/R_share/MCJ/R/AUD_PCA/stack/pics/demand.pdf")
fanchart(prd.mod, names='real_demand')
dev.off()

pdf("S:/Rates Research/R_share/MCJ/R/AUD_PCA/stack/pics/financial.pdf")
fanchart(prd.mod, names='financial')
dev.off()

pdf("S:/Rates Research/R_share/MCJ/R/AUD_PCA/stack/pics/interbank.pdf")
fanchart(prd.mod, names='RBApolicy')
dev.off()

pdf("S:/Rates Research/R_share/MCJ/R/AUD_PCA/stack/pics/VAR.pdf")
fanchart(prd.mod)
dev.off()

# Stuff related to SO Qn...
# http://stackoverflow.com/questions/12488179/adding-dates-to-fanchart-plots-in-r-package-vars/12488317#12488317
# trying to stick dates on vars fancharts
# fanchart(var.2c.prd, xaxt="n")  
# times <- as.vector(time(Canada))  
# axis(1, at=seq_along(times), labels=as.vector(time(Canada)), las=3, line=-6, cex.axis=0.6)  
# 
# 
# data(Canada)
# var.2c <- VAR(Canada, p = 2, type = "const")
# var.2c.prd <- predict(var.2c, n.ahead = 8, ci = 0.95)
# fanchart(var.2c.prd)
# 
# fanchart(var.2c.prd, xaxt="n")  
# times <- as.vector(time(Canada) )  
# axis(1, at=1:84, labels=as.vector(time(Canada) ), las=3, line=-24, cex.axis=0.6)  
# axis(1, at=seq_along(times), labels=as.vector(time(Canada) ), las=3, line=-15, cex.axis=0.6)  
# axis(1, at=seq_along(times), labels=as.vector(time(Canada) ), las=3, line=-6, cex.axis=0.6)
# axis(1, at=1:84, labels=as.vector(rep(LETTERS[1:12], 7)), las=3, line=-24, cex.axis=0.6)  
