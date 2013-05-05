# the new PCA --> VAR --> Cash model for Australia

# A :: Matthew Coogan Johnson
# S :: Sunday 14 October 2012

## TODO -- try this as a 3mma rather than using the stacking approach - what changes?

# setup stuff
rm(list=ls()); gc()
Sys.setenv(TZ='GMT')

# packages
require(xts)
require(timsac)
require(RODBC)
require(missMDA)
require(vars)

# mac/ home stuff
source("~/R/PCA/helperFunctions.r")
require(gdata)
dd <- read.xls("~/data/Blueprint.xls", sheet="intor")
map <- read.xls("~/data/Blueprint.xls", sheet="mapR", as.is=TRUE)

# get functions
# source("S:/Rates Research/R_share/helpers/helperFunctions.r")

# get the data for the PCA
# conXL <- odbcConnectExcel(xls.file="S:/Rates Research/data/AUD_PCA_FCI/blueprint.xls")
# s1 <- 'intor'
# dd <- sqlFetch(conXL, s1)
# s2 <- 'mapR'
# map <- sqlFetch(conXL, s2, as.is=TRUE)
# odbcClose(conXL)

# converts char to +/-1: working around an RODBC 'feature'
map <- map[,-1]
map[3,] <- ifelse(map[3,]=='d', -1, 1)

# clean out the ref series
pcaNames <- names(map[, which(map[4,] == 'pca')])
fciNames <- names(map[, which(map[4,] == 'fci')])
bothNames <- names(map[, which(map[4,] == 'both')])
refNames <- names(map[, which(map[4,] == 'ref')])
grfNames <- names(map[, which(map[5,] == 'gf')])
rbaNames <- names(map[, which(map[6,] == 'y')])

tt <- as.Date(dd[,1]) # note this is in descending order
dd <- dd[!is.na(dd[,1]),][,-1]
allD_x <- xts(dd, order.by=tt)

# crush a few bugs
allD_x$m_cons <- abs(allD_x$m_cons) # fix a bbg bug
allD_x$m_k <- abs(allD_x$m_k) # fix a bbg bug

# transform the data according to the map
logNames <- names(map)[which(map[2,] == 'ln')]
dd_ln <- log(allD_x[,logNames])

diffNames <- names(map)[which(map[2,] == 'D')]
dd_diff <- diff(allD_x[,diffNames])

lvlNames <- names(map)[which(map[2,] == 'lvl')]
dd_lvl <- allD_x[,lvlNames]

lnDiffNames <- names(map)[which(map[2,] == 'lnD')]
dd_lnDiff <- diff(allD_x[,lnDiffNames], log=TRUE)

# combine stationary data
s_dat <- cbind(dd_ln, dd_diff, dd_lvl, dd_lnDiff)

# fix some structural breaks
e1 <- '1989-01-01'
e2 <- '1990-01-01'
n1  <- which(index(s_dat) == e1)
n2  <- which(index(s_dat) == e2)
s_dat[n1, c('c_bus', 'c_pers')] <- (coredata(s_dat[n1-1, c('c_bus', 'c_pers')]) + coredata(s_dat[n1+1, c('c_bus', 'c_pers')]))/2
s_dat[n2, c('c_hhd')] <- (coredata(s_dat[n2-1, c('c_hhd')]) + coredata(s_dat[n2+1, c('c_hhd')]))/2

# subset for time -- if you want to shift time shift it here
s_datSub <- s_dat['1991::']
tts <- index(s_datSub)

# split the transformed data into parts
ref_x <- xts(s_datSub[, refNames], order.by=tts)
pca_x <- xts(s_datSub[,c(pcaNames, bothNames)], order.by=tts)
fci_x <- xts(s_datSub[, c(fciNames, bothNames)], order.by=tts)
grf_x <- xts(s_datSub[, grfNames], order.by=tts)
rba_x <- xts(s_datSub[, rbaNames], order.by=tts)

# write.csv(s_dat, file="S:/Rates Research/R_share/MCJ/nR/AUD/data/sdat_output.csv")

############
## RBA PCA##
############

# impute
ii.rba <- imputePCA(coredata(rba_x), ncp=3)
rba_x.pcaDat <- xts(ii.rba$completeObs, order.by=tts)
# stack and take from 1992
rba_x.3stck <- cbind(rba_x.pcaDat, lag(rba_x.pcaDat, 1), lag(rba_x.pcaDat, 2))['1992::']
# get factors
rba.PCAmod <- PCA(rba_x.3stck, graph=FALSE)
rba.W <- rba.PCAmod$var$coord # the eVector matrix
rba.PCA_f <- rba.PCAmod$ind$coord # the first five factors
rba.PCA_f_x <- xts(rba.PCA_f, order.by=index(rba_x.3stck))
# the scores are returned!
rba.sf_x <- scale(rba.PCA_f_x)[,1:2]
names(rba.sf_x) <- c('demand', 'financial')

#############
## RBA VAR ##
#############

rba.var <- cbind(rba.sf_x, ref_x$cash)['1992::']
rba.var <- rba.var[,c('demand', 'cash', 'financial')]
#
rba.maxLag <- findMaxVARLag(rba.var)
rba.varmod <- VAR(rba.var, lag.max=rba.maxLag)
# rba.irf <- irf(rba.varmod, n.ahead=36)
rba.pp <- predict(rba.varmod, n.ahead=18, ci=0.66)


rba.restMatx <- matrix(rep(1, 48), nrow = 3, ncol = 16)
rownames(rba.restMatx) <- c('demand', 'cash', 'financial')
colnames(rba.restMatx) <- c('demand.l1', 'cash.l1', 'financial.l1', 'demand.l2', 'cash.l2', 'financial.l2',
                        'demand.l3', 'cash.l3', 'financial.l3', 'demand.l4', 'cash.l4', 'financial.l4',
                        'demand.l5', 'cash.l5', 'financial.l5',
                        'const')

#^^^^^^^^^^^^^#
# END RBA VAR #
#_____________#


#################
## MJs BIG PCA ##
#################

im <- imputePCA(coredata(pca_x), ncp=4)
pca_xR <- xts(im$completeObs, order.by=tts)
# stack and select 92+
dd_3stck <- cbind(pca_xR, lag(pca_xR, 1), lag(pca_xR, 2))['1992::']

PCAmod <- PCA(dd_3stck, graph=F)
W <- PCAmod$var$coord # the eVector matrix
PCA_f <- PCAmod$ind$coord # the first five factors
PCA_f_x <- xts(PCA_f, order.by=index(dd_3stck))

# the scaled factors are returned!
sf_x <- scale(PCA_f_x)[,1]

# in this section we construct the scores index
SAM3mma = (pca_xR + lag(pca_xR) + lag(pca_xR, 2))/3
DSAM3mma = diff(SAM3mma)
AUDpulse <- ifelse(as.matrix(DSAM3mma) <= 0, -1, 1)
AUD_score <- AUDpulse %*% as.numeric(map[3, match(names(pca_xR), names(map))])

AUD_score <- scale(xts(AUD_score, order.by=tts))
AUD_score3m <- (AUD_score + lag(AUD_score) + lag(AUD_score, 2))/3
AUD_score6m <- (AUD_score + lag(AUD_score) + lag(AUD_score, 2) + lag(AUD_score, 3) +
                lag(AUD_score, 4) + lag(AUD_score, 5))/6

# the scores are returned!
scores <- scale(cbind(AUD_score, AUD_score3m, AUD_score6m))
names(scores) <- c('p1', 'p3', 'p6')

##### ===> FCI <=== ####
# now construct the FCI: 1st fill in the blanks
imF <- imputePCA(coredata(fci_x), ncp=4)
fciCmpl <- xts(imF$completeObs, order.by=tts)
# stack and select 92+
fc_3stck <- cbind(fciCmpl, lag(fciCmpl), lag(fciCmpl, 2))['1992::']
# first purge the data of f1 and inflation
purgedFCI <- matrix(rep(0, ncol(fc_3stck)*nrow(fc_3stck)), nrow(fc_3stck), ncol(fc_3stck))

RBAcash_3mma <- (allD_x$cash + lag(allD_x$cash,1) + lag(allD_x$cash,2))/3
cpi_3mma <- (allD_x$CPI + lag(allD_x$CPI,1) + lag(allD_x$CPI,2))/3

for (i in 1:ncol(fc_3stck)){
   resid_i <- lm(fc_3stck[,i] ~ sf_x[,1] + RBAcash_3mma['1992::'] + cpi_3mma['1992::'])$residuals
   purgedFCI[,i] <- resid_i
}

purgedFCI_x <- xts(as.data.frame(purgedFCI), order.by=index(fc_3stck))
names(purgedFCI_x) <- names(fc_3stck)

# now do the PCA
FCImod <- PCA(purgedFCI, graph=F)
FCI_W <- FCImod$var$coord # the eVector Matrix
rownames(FCI_W) <- names(fc_3stck)
FCI_f <- FCImod$ind$coord # the first five factors
FCI_f_x <- xts(FCI_f, order.by=index(fc_3stck))
FCI_sfx <- scale(FCI_f_x)[,1]

##### ===> FCI_sfx is your Watson-ized FCI <=== #####

##### ===> GRF <=== ####

# 1st make the normalisation ref series
effr_3mma <- (allD_x$effr + lag(allD_x$effr,1) + lag(allD_x$effr,2))/3
ip <- allD_x$us_ip
ip3mma <- (ip + lag(ip) + lag(ip,2))/3
D.ip3mma <- diff(ip3mma, log=TRUE)

imG <- imputePCA(coredata(grf_x), ncp=4)
grfCmpl <- xts(imG$completeObs, order.by=tts)
# stack and select92+
gf_3stck <- cbind(grfCmpl, lag(grfCmpl), lag(grfCmpl, 2))['1992::']

# first purge the data of fed policy, USIP, and US inflation
purgedGRF <- matrix(rep(0, ncol(gf_3stck)*nrow(gf_3stck)), nrow(gf_3stck), ncol(gf_3stck))

for (i in 1:ncol(gf_3stck)){
   resid_i <- lm(gf_3stck[,i] ~ effr_3mma['1992::'] + D.ip3mma['1992::'] + allD_x$usCPI['1992::'])$residuals
   purgedGRF[,i] <- resid_i
}

purgedGRF_x <- xts(as.data.frame(purgedGRF), order.by=index(gf_3stck))
names(purgedGRF_x) <- names(gf_3stck)

GRFmod <- PCA(purgedGRF, graph=FALSE)
GRF_W <- GRFmod$var$coord
rownames(GRF_W) <- names(gf_3stck)
GRF_f <- GRFmod$ind$coord # the first five factors
GRF_f_x <- xts(GRF_f, order.by=index(gf_3stck))
GRF_sfx <- scale(GRF_f_x)[,1]

##### ===> GRF_sfx is your Watson-ized GRF <=== #####

# note the GRF actually hurts out of sample forecasting performance for both
# the RBA and for CPI over a 12m horizon -- need to try an SVAR approach
# in particular, GRF should be exogenous to AUD conditions / policy.

# and now we do the VAR
# this seems like it's going to have to be done in parts
# once the order etc is identified, you are going to have to estimate the equations seperately

# use OLS / lm to do so, and in the data / CPI equations restrict the params via:

# y ~ ... + I(ref_x$cash + ref_x + cashSpread) + ...
# in the cash rate equation, use cash ~ ... + offset(-1*lag(ref_x$cashSpread,1)) ...


# perhaps use the maxLag to find the max lag order with all the variables you'd like in the data, and
# then use restricted OLS to find the functional forms you desire -- the downside being that you
# lose the neat benefits of the vars package ... try and hack it in the restrict step!

# benchmark this against the RBA monthly model ...

# put the other data together
varFrame <- cbind(D.ip3mma*300, ref_x$usCPI, ref_x$effr, ref_x$cCPI, sf_x, FCI_sfx, ref_x$cash)['1992::']
names(varFrame) <- c('us_ip', 'us_cCPI', 'effr', 'cCPI', 'aDmnd', 'aFCI', 'cash')
# some QQ stuff ...
# vfq.ave <- apply.quarterly(varFrame[, -c(3, 6)], apply, 2, mean)
# vfq.last <- apply.quarterly(varFrame[, c(3, 6)], apply, 2, last)
# vfq <- cbind(vfq.ave, vfq.last)
# restrict to 1985+ and re-order
# vfq.ord <- vfq[, c('us_ip', 'us_cCPI', 'effr', 'aDmnd', 'cCPI_adj', 'cash', 'aFCI')]['1988::']

# TODO :: make a MoM model.
# vfm.ord <- varFrame[, c('us_ip', 'us_cCPI', 'effr', 'aDmnd', 'cCPI_adj', 'cash', 'aFCI')]['1988::']

smallVAR <- cbind(allD_x$rbaCpix, ref_x$cCPI, sf_x, FCI_sfx, ref_x$cash)['1992::']
names(smallVAR) <- c("rbaCpix", "cCPI", "aD", "FCI", "cash")
modS <- VAR(scale(smallVAR), p = 4)
plot(irf(modS, n.ahead=36))

maxLag <- 7 # findMaxVARLag(smallVAR, firstMax=12, crit = "HQ(n)")


# set up the unrestricted VAR
mod <- VAR(varFrame, p=maxLag)
pp <- predict(mod, n.ahead=12)
# plot(irf(mod, n.ahead=20))

# set up the restriction matrix
restMatx <- matrix(rep(1, 350), nrow = 7, ncol = 50)
rownames(restMatx) <- c('us_ip', 'us_cCPI', 'effr', 'aDmnd', 'cCPI_adj', 'cash', 'aFCI')
colnames(restMatx) <- c('us_ip.l1', 'us_cCPI.l1', 'effr.l1', 'aDmnd.l1', 'cCPI_adj.l1', 'cash.l1', 'aFCI.l1',
                        'us_ip.l2', 'us_cCPI.l2', 'effr.l2', 'aDmnd.l2', 'cCPI_adj.l2', 'cash.l2', 'aFCI.l2',
                        'us_ip.l3', 'us_cCPI.l3', 'effr.l3', 'aDmnd.l3', 'cCPI_adj.l3', 'cash.l3', 'aFCI.l3',
                        'us_ip.l4', 'us_cCPI.l4', 'effr.l4', 'aDmnd.l4', 'cCPI_adj.l4', 'cash.l4', 'aFCI.l4',
                        'us_ip.l5', 'us_cCPI.l5', 'effr.l5', 'aDmnd.l5', 'cCPI_adj.l5', 'cash.l5', 'aFCI.l5',
                        'us_ip.l6', 'us_cCPI.l6', 'effr.l6', 'aDmnd.l6', 'cCPI_adj.l6', 'cash.l6', 'aFCI.l6',
                        'us_ip.l7', 'us_cCPI.l7', 'effr.l7', 'aDmnd.l7', 'cCPI_adj.l7', 'cash.l7', 'aFCI.l7',
                        'const')

# Set up eestrictions
restMatx[c(1:3), c(4:7, 11:14, 18:21, 25:28, 32:35, 39:42, 46:49)] <- 0 # make US bloc exogenous
mod.res <- restrict(mod, method = 'manual', resmat = restMatx)
mod.res.ser <- restrict(mod, method = 'ser', thresh=3)

pp <- predict(mod.res, n.ahead=12)
pp <- predict(mod.res.ser, n.ahead=18)
plot(irf(mod.res, n.ahead=12))

##### ===> test the prediction model <=== #####

# to change forecast horizon, edit nAhead

testVar <- function(dframe, nAhead = 6, IC = 'SC', periodicity = 'months', skip = NULL, Vlag = 12, RSTmtx = NULL)
{
    # setup
    if(is.null(skip)) { skip <- nrow(dframe) %/% 2 }
    dateList <- index(dframe)
    ttlRows <- length(dateList) + nAhead
    extraDate <- seq(index(dframe[1]), by=periodicity, length.out=ttlRows)
    # make extended variables
    testPack <- list()
    for (i in 1:ncol(dframe))
    {
      testPack[[names(dframe)[i]]] <- as.xts(
                                             cbind(c(coredata(dframe[,i]), rep(NA, nAhead)),
                                              matrix(rep(NA, ttlRows * (length(dateList) - skip)),
                                                     nrow = ttlRows, ncol = (length(dateList) - skip)
                                                     )
                                                  ), order.by = extraDate
                                             )
    }
    # the test loop
    for (d in (skip+1):length(dateList))
    {
      testName <- paste0('from:', dateList[d]) # need to stick this on later
      if (is.null(RSTmtx))
      {
        optLag <- findMaxVARLag(dframe[1:d,], firstMax = Vlag, crit = paste0(IC, "(n)"))
        mod <- VAR(dframe[1:d,], p = optLag)
      } else {
        mod <- VAR(dframe[1:d,], p = Vlag)
        mod <- restrict(mod, method = 'manual', resmat = RSTmtx)
      }
      poosPred <- predict(mod, n.ahead=nAhead)
      #
      for (v in 1:ncol(dframe)) # loop across by column type
      {
        outvec <- rep(NA, ttlRows) # allocate the full length vector
        outvec[(d + 1):(d + nAhead)] <- poosPred$fcst[[v]][,1] # replace with 4casts
        testPack[[v]][, (d - skip + 1)] <- outvec # one variable at a time
      }
    }
    return(testPack)
}

errTstVar <- function(testRslt) {
  sqdError <- testRslt # copy object to pre-allocate
  for (v in 1:length(testRslt))
  {
    for (i in 2:ncol(testRslt[[v]]))
    {
      sqdError[[v]][,i] <- (sqdError[[v]][,1] - sqdError[[v]][,i])**2
    }
  sqdError[['ssfe']][[names(testRslt)[v]]] <- sum(sqdError[[v]], na.rm=TRUE)
  }
  return(sqdError)
}


mod.test <- testVar(smallVAR, skip = 83, IC = "SC")
sumTestError <- errTstVar(mod.test)
plot.zoo(mod.test$cash, screen=1, col=c(1, rep(8, ncol(mod.test$cash)-1)))


mod.testRev <- mod.test[, rev(names(mod.test))]
write.csv(mod.testRev, file="S:/Rates Research/R_share/MCJ/nR/AUD/data/varBacktest_rbaPCA.csv")

#### ===> Charts <=== ####


pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/cash_vAdj.pdf")
plot(ref_x$cash['1999::'], type='o', pch=20, las=1,
     main='RBA Cash v. Spread Adjusted', ylim=c(2,8))
lines(ref_x$adjCash['1999::'], col=3, lwd=2)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/demandF.pdf")
plot(sf_x, type='o', pch=20, las=1, main="Demand Indices", ylim = c(-4.5, 2))
lines(rba.sf_x[,1], type='l', col=2, lwd=2)
lines(sf_x, type='o', pch=20, las=1)
legend('topright', c('UBS1', 'UBS2'), col = c(1,2), lwd = c(2,2), pch = c(19, NA), horiz=T)
mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/demandF_06p.pdf")
plot(sf_x['2006::'], type='o', pch=20, las=1, main="Demand Indices", ylim = c(-4.5, 2))
lines(rba.sf_x['2006::',1], type='l', col=2, lwd=2)
lines(sf_x['2006::'], type='o', pch=20, las=1)
legend('topright', c('UBS1', 'UBS2'), col = c(1,2), lwd = c(2,2), pch = c(19, NA), horiz=T)
mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/FCI.pdf")
plot(FCI_sfx, type='o', pch=20, las=1, main="Financial Conditions Indices", ylim = c(-3, 3))
lines(rba.sf_x[,2], type='l', col=3, lwd=2)
lines(FCI_sfx, type='o', pch=20, las=1)
legend('topright', c('UBS1', 'UBS2'), col = c(1,3), lwd = c(2,2), pch = c(19, NA), horiz=T)
mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/FCI_06p.pdf")
plot(FCI_sfx['2006::'], type='o', pch=20, las=1, main="UBS Financial Conditions Index", ylim = c(-3, 1.5))
lines(rba.sf_x['2006::',2], type='l', col=3, lwd=2)
lines(FCI_sfx['2006::'], type='o', pch=20, las=1)
legend('topright', c('UBS1', 'UBS2'), col = c(1,3), lwd = c(2,2), pch = c(19, NA), horiz=T)
mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/capacity.pdf")
plot(allD_x$cap_util, type='o', pch=20, ylim=c(66,86), main='Capacity Utilisation', las=1)
lines(allD_x$apm_capU_nsa, col=2, type='o', pch=16)
legend('top', c('NAB', 'ManuPMI'), horiz=T, pch = c(20, 16), col = c(1, 2), bty='n')
abline(h=last(allD_x$cap_util), lty=2)
abline(h=last(allD_x$apm_capU_nsa), col=2, lty=2)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/nabJobs.pdf")
plot(scale(allD_x$nab_empl['1997::']), type='o', pch=20, main="NAB Employment Index")
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/anz.pdf")
plot(scale(allD_x$anz_nja['1997::']), type='l', ylim=c(-3,3), main='ANZ Job Ads', las=1)
lines(scale(allD_x$anz_ija['1997::']), type='l', col=4)
abline(h=last(scale(allD_x$anz_nja['1997::'])), lty=2)
abline(h=last(scale(allD_x$anz_ija)), col=4, lty=2)
legend('top', c('News', 'Internet'), horiz=T, lty = c(1, 1), col = c(1, 4), bty='n')
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/nab.pdf")
plot(allD_x$nab_empl['1997::'], type='o', pch=20, main="NAB Employment Index", las=1)
abline(h = last(allD_x$nab_empl), lty=2)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/FCI_cash.pdf")
par(mar=c(5,4,4,5)+0.1)
plot(FCI_sfx['1999::'], type='o', pch=20, las=1, main="FCI v Cash Rate", ylab="+ve = easier than average", xlab="")
par(new=TRUE)
plot(coredata(-ref_x$cash['1999::']) ~ index(ref_x['1999::']), type='l', col=2, xaxt='n', yaxt='n', xlab='', ylab='', las=1)
axis(4, at = c(-3:-7), labels = c(3:7), las=1)
mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
dev.off()

pdf("S:/Rates Research/R_share/MCJ/nR/AUD/pics/DataScores.pdf")
plot.zoo(scores['2003::'][,-1], col=2:3, main="AUD Data Pulse: 2003--2012", las=1, screens=1, xlab="", ylab="", lwd=2)
legend('topleft', c('3mma', '6mma'), horiz=T, lty = c(1, 1), col = c(2, 3), bty='n')
mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
dev.off()
