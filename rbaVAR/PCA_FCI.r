# the new PCA --> VAR --> Cash model for Australia

# A :: Matthew Coogan Johnson
# D :: Sunday 14 October 2012

# setup stuff
rm(list=ls())
Sys.setenv(TZ='GMT')

require(xts)
require(timsac)
require(RODBC)
require(missMDA)

# fix mortgage data
#
# 1/ get the data
# conXL <- odbcConnectExcel(xls.file="S:/Rates Research/data/AUD_PCA_FCI/blueprint")
# s0 <- 'adjCashOut'
# adjC <- sqlFetch(conXL, s0)
# adjC_trim <- adjC[which(!is.na(adjC$date)),]
# adjC_trim_x <- xts(adjC_trim[,-1], order.by=as.Date(adjC_trim[,1]))
# odbcClose(conXL)
#
# paste it into MDB
# conMDB = odbcConnectAccess("S:/Rates Research/autodata/AUD/RBA/Reserve Bank.mdb")
# sqlDrop(conMDB, sqtable="mj_AdjRates") # note this will hang if the table cannot be found
# sqlSave(conMDB, as.data.frame(adjC_trim_x), tablename="mj_AdjRates", rownames=T, fast=T)
# odbcClose(conMDB)  #Close connection to DB

# get the data for the PCA
# conXL <- odbcConnectExcel(xls.file="S:/Rates Research/data/AUD_PCA_FCI/blueprint")
# s1 <- 'intor'
# dd <- sqlFetch(conXL, s1)
# s2 <- 'mapR'
# map <- sqlFetch(conXL, s2, as.is=TRUE)
# odbcClose(conXL)

# converts char to +/-1: working around an RODBC 'feature'
# map[3,] <- ifelse(map[3,]=='d', -1, 1)

dd <- read.table('~/R/rbaVAR/bpDd.txt', header=TRUE, as.is=TRUE)
tt <- as.Date(dd[,1]) # note this is in descending order
dd <- dd[!is.na(dd[,1]),][,-1]
dd[['m_cons']] <- abs(dd[['m_cons']]) # fix a bbg bug
dd[['m_k']] <- abs(dd[['m_k']]) # fix a bbg bug
dd_x <- xts(dd, order.by=tt)
dd_x <- dd_x['19840101::']
tts <- index(dd_x['19840101::'])

map <- read.table('~/R/rbaVAR/mapR.txt', header=TRUE, as.is=TRUE)
# rownames(map) <- map[,1]
map <- map[,-1]

dd_ln <- log(dd_x[,which(map[2,] == 'ln')])
dd_diff <- diff(dd_x[,which(map[2,] == 'D')])
dd_lvl <- dd_x[,which(map[2,] == 'lvl')]
dd_lnDiff <- diff(dd_x[,which(map[2,] == 'lnD')], log=TRUE)

s_dat <- cbind(dd_ln, dd_diff, dd_lvl, dd_lnDiff)

# fix some structural breaks
e1 <- '1989-01-01'
e2 <- '1990-01-01'
n1  <- which(index(s_dat) == e1)
n2  <- which(index(s_dat) == e2)
s_dat[n1, c('c_bus', 'c_pers')] <- (coredata(s_dat[n1-1, c('c_bus', 'c_pers')]) + coredata(s_dat[n1+1, c('c_bus', 'c_pers')]))/2
s_dat[n2, c('c_hhd')] <- (coredata(s_dat[n2-1, c('c_hhd')]) + coredata(s_dat[n2+1, c('c_hhd')]))/2

write.csv(s_dat, file="S:/Rates Research/R_share/MCJ/nR/AUD/data/sdat_output.csv")

im <- imputePCA(coredata(s_dat), ncp=4)

s_datR <- xts(im$completeObs, order.by=tts)

dd_3stck_leadNA <- cbind(s_datR, lag(s_datR), lag(s_datR, 2))

dd_3stck_leadNA[2,which(is.na(dd_3stck_leadNA[2,]))] <- dd_3stck_leadNA[3,which(is.na(dd_3stck_leadNA[2,]))]
dd_3stck_leadNA[1,which(is.na(dd_3stck_leadNA[1,]))] <- dd_3stck_leadNA[2,which(is.na(dd_3stck_leadNA[1,]))]

dd_3stck <- dd_3stck_leadNA

PCAmod <- PCA(dd_3stck, graph=F)
W <- PCAmod$var$coord # the eVector matrix
PCA_f <- PCAmod$ind$coord # the first five factors
PCA_f_x <- xts(PCA_f, order.by=tts)

# the scaled factors are returned!
sf_x <- scale(PCA_f_x)[,1]

# in this section we construct the scores index
SAM3mma = (s_datR + lag(s_datR) + lag(s_datR, 2))/3
DSAM3mma = diff(SAM3mma)
AUDpulse <- ifelse(as.matrix(DSAM3mma) <= 0, -1, 1)
AUD_score <- AUDpulse %*% as.numeric(map[3,])

AUD_score <- scale(xts(AUD_score, order.by=tts))
AUD_score3m <- (AUD_score + lag(AUD_score) + lag(AUD_score, 2))/3
AUD_score6m <- (AUD_score + lag(AUD_score) + lag(AUD_score, 2) + lag(AUD_score, 3) +
                lag(AUD_score, 4) + lag(AUD_score, 5))/6

# the scaled factors are returned!
scores <- scale(cbind(AUD_score, AUD_score3m, AUD_score6m))

# now construct the FCI

# first the FCI data sub-set

fciNames <- names(dd)[which(map[4,] == 'f')]
nameIdx <- match(fciNames, colnames(s_datR))
fciDat <- s_datR[,nameIdx]

fciDat3 <- ( fciDat + lag(fciDat, 1) + lag(fciDat, 2))/3
fciDat3[1,] <- fciDat3[2,] <- fciDat3[3,]

purgedFCI <- matrix(rep(0, ncol(fciDat)*nrow(fciDat)), nrow(fciDat), ncol(fciDat))

for (i in 1:ncol(fciDat)){
   resid_i <- lm(fciDat3[,i] ~ sf_x[,1] + coredata(dd_x$CPI))$residuals
   purgedFCI[,i] <- resid_i
}

purgedFCI_x <- xts(as.data.frame(purgedFCI), order.by=tts)
names(purgedFCI_x) <- names(fciDat)

# now do the PCA
FCImod <- PCA(purgedFCI, graph=F)
FCI_W <- FCImod$var$coord
rownames(FCI_W) <- names(fciDat)
FCI_f <- FCImod$ind$coord # the eVector Matrix
FCI_f_x <- xts(FCI_f, order.by=tts) # the first five factors
FCI_sfx <- scale(FCI_f_x)[,1]

# and PCA_f_x is your Watson-ized FCI

# and now we do the VAR
varFrame <- cbind(sf_x, FCI_sfx, dd_x$cash, dd_x$cCPI)
names(varFrame) <- c('demand', 'FCI', 'cash', 'coreCPI')

varSub <- varFrame['19890101::']
mod <- VAR(varSub, lag.max=3, ic='SC')
pp <- predict(mod)
fanchart(pp)
