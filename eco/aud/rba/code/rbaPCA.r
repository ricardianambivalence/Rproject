# the new PCA --> VAR --> Cash model for Australia

# A :: Matthew Coogan Johnson
# S :: Sunday 14 October 2012

## TODO -- try this as a 3mma rather than using the stacking approach - what changes?
## add a VAR with the UR: aD, aFCI, UR, cCPI, cash (and perhaps commodity prices?)

# {{{ setup, packages and functions
cleanUp()
require(xts)
require(timsac)
require(RODBC)
require(missMDA)
require(vars)
require(TTR)
require(Rbbg)
require(ggplot2)
require(lattice)
require(latticeExtra)

# get helper functions
source("S:/Rates Research/autotools/Rhelpers/ecoPCAhelpers.r")
# and get the IB functions
source("S:/Rates Research/derivs/stir/aud/code/ibFunctions.r")

# compile functions
# na.ARextend <- cmpfun(na.ARextend)

# }}}
## {{{ path stuff
projectPATH <- "S:/Rates Research/eco/aud/rba/Policy_VAR"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- file.path(projectPATH, 'data')
codePATH <- file.path(projectPATH, 'code')

# }}}
#  {{{ get the data etc
conXL <- odbcConnectExcel(xls.file="S:/Rates Research/eco/aud/rba/policy_var/data/blueprint.xls")
s1 <- 'intor'
dd <- sqlFetch(conXL, s1)
s2 <- 'mapR'
map <- sqlFetch(conXL, s2, as.is=TRUE)
s3 <- 'Q2R'
qRef <- sqlFetch(conXL, s3, as.is=TRUE)
qRef <- qRef[complete.cases(qRef),]
odbcClose(conXL)

# date flags
dateFlag <- paste0("1985", "::")
modStartDate <- as.Date("1985-03-01")
modStartDateFlag <- paste0(modStartDate, '::')
prelimFlag <- paste0(as.numeric(substr(dateFlag, 1,4)) - 1, "::")

# converts char to +/-1: working around an RODBC 'feature'
map <- map[,-1]
map[3,] <- ifelse(map[3,]=='d', -1, 1)

# prep the QoQ ref series (it's only rDGI)
rGDIx <- xtsF(qRef)

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
s_dat[n1, c('c_bus', 'c_pers')] <- (coredata(s_dat[n1-1, c('c_bus', 'c_pers')]) +
                                    coredata(s_dat[n1+1, c('c_bus', 'c_pers')]))/2
s_dat[n2, c('c_hhd')] <- (coredata(s_dat[n2-1, c('c_hhd')]) +
                          coredata(s_dat[n2+1, c('c_hhd')]))/2

# fill in gaps, and extend using AR1 -- then subset for date
s_datSub <- mapXtsL(s_dat[dateFlag], na.ARextend)
tts <- index(s_datSub)

# split the transformed data into parts
ref_x <- xts(s_datSub[, refNames], order.by=tts)
pca_x <- xts(s_datSub[,c(pcaNames, bothNames)], order.by=tts)
fci_x <- xts(s_datSub[, c(fciNames, bothNames)], order.by=tts)
grf_x <- xts(s_datSub[, grfNames], order.by=tts)
rba_x <- xts(s_datSub[, rbaNames], order.by=tts)

## }}}
## {{{ RBA VAR -->
############
## RBA PCA##
############

# complete-obs step

# extend using AR2

# impute
ii.rba <- imputePCA(coredata(rba_x), ncp=1)
rba_x.pcaDat <- xts(ii.rba$completeObs, order.by=tts)
# stack and take from 1992
rba_x.3stck <- cbind(rba_x.pcaDat, lag(rba_x.pcaDat, 1), lag(rba_x.pcaDat, 2))[modStartDateFlag]
# get factors
rba.PCAmod <- PCA(rba_x.3stck, graph=FALSE)
rba.W <- rba.PCAmod$var$coord # the eVector matrix
rba.PCA_f <- rba.PCAmod$ind$coord # the first five factors
rba.PCA_f_x <- xts(rba.PCA_f, order.by=index(rba_x.3stck))
# the scores are returned!
rba.sf_x <- scale(rba.PCA_f_x)[,1:2]
names(rba.sf_x) <- c('demand', 'financial')

rbaVAR <- cbind(rba.sf_x, ref_x$cash, ref_x$cCPI)[paste0(modStartDate, '::')]
names(rbaVAR) <- c('aD', 'aFCI', 'cash', 'cCPI')
rbaVAR.mod <- VAR(rbaVAR, p=4)

# }}}
### {{{ ===> MJ BIG PCA

#################
## MJs BIG PCA ##
#################

im <- imputePCA(coredata(pca_x), ncp=1)
pca_xR <- xts(im$completeObs, order.by=tts)
# stack and select dates
dd_3stck <- cbind(pca_xR, lag(pca_xR, 1), lag(pca_xR, 2))[dateFlag]
#
PCAmod <- PCA(dd_3stck, graph=F)
W <- PCAmod$var$coord # the eVector matrix
PCA_f <- PCAmod$ind$coord # the first five factors
PCA_f_x <- xts(PCA_f, order.by=index(dd_3stck))
#
# the scaled factors are returned!
sf_x <- scale(PCA_f_x)[,1]
#
# in this section we construct the scores index
SAM3mma = (pca_xR + lag(pca_xR) + lag(pca_xR, 2))/3
DSAM3mma = diff(SAM3mma)
AUDpulse <- ifelse(as.matrix(DSAM3mma) <= 0, -1, 1)
AUD_score <- AUDpulse %*% as.numeric(map[3, match(names(pca_xR), names(map))])
#
AUD_score <- scale(xts(AUD_score, order.by=tts))
AUD_score3m <- (AUD_score + lag(AUD_score) + lag(AUD_score, 2))/3
AUD_score6m <- (AUD_score + lag(AUD_score) + lag(AUD_score, 2) + lag(AUD_score, 3) +
                lag(AUD_score, 4) + lag(AUD_score, 5))/6
#
# the scores are returned!
scores <- scale(cbind(AUD_score, AUD_score3m, AUD_score6m))
names(scores) <- c('p1', 'p3', 'p6')

## the no-lag version
dd_1stck <- pca_xR
PCA1_mod <- PCA(dd_1stck, graph=FALSE)
PCA1f <- PCA1_mod$ind$coord
PCA1_f_x <- xts(PCA1f, order.by = index(dd_1stck))
dd1_x <- scale(PCA1_f_x)[, 1]

# }}}
#####  {{{ ===> FCI <=== ####

# 1st fill in the blanks
imF <- imputePCA(coredata(fci_x), ncp=1)
fciCmpl <- xts(imF$completeObs, order.by=tts)
# stack and select dates
fc_3stck <- cbind(fciCmpl, lag(fciCmpl), lag(fciCmpl, 2))[-c(1:2), ]
# {{{ purge FCI?
purgeFCI <- TRUE
if(purgeFCI) {
    # first purge the data of f1 and inflation
    purgedFCI <- matrix(rep(0, ncol(fc_3stck)*nrow(fc_3stck)), nrow(fc_3stck), ncol(fc_3stck))
    #
    RBAcash_3mma <- rollapplyr(na.ARextend(allD_x$adjCash), 3, mean)[dateFlag]
    cpi_3mma <- rollapplyr(na.ARextend(allD_x$cCPI), 3, mean)[dateFlag]
    #
    for (i in 1:ncol(fc_3stck)) {
        resid_i <- lm(fc_3stck[,i] ~ sf_x[-c(1:2), 1] + RBAcash_3mma[-c(1:2)] + cpi_3mma[-c(1:2)])$residuals
        purgedFCI[,i] <- resid_i
    }
    #
} else {
    purgedFCI <- fc_3stck
}
purgedFCI_x <- xts(as.data.frame(purgedFCI), order.by=index(fc_3stck))
names(purgedFCI_x) <- names(fc_3stck)

# }}} close purge FCI?
#
# now do the PCA
FCImod <- PCA(purgedFCI, graph=F)
FCI_W <- FCImod$var$coord # the eVector Matrix
rownames(FCI_W) <- names(fc_3stck)
FCI_f <- FCImod$ind$coord # the first five factors
FCI_f_x <- xts(FCI_f, order.by=index(fc_3stck))
FCI_sfx <- scale(FCI_f_x)[,1]
names(FCI_sfx) <- 'fci'

# output order weights
oo <- order(rownames(FCI_W[1:60, ]))
EvectorAlphaSort <- FCI_W[1:60, 1, drop=FALSE][oo,]

## the no-lag version
fci_1stack <- fciCmpl
purge1 <- matrix(rep(0, ncol(fciCmpl) * nrow(fciCmpl)), nrow(fciCmpl), ncol(fciCmpl))
#
for (i in 1:ncol(fciCmpl)) {
    resid_i <- lm(fciCmpl[,i] ~ dd1_x +
                  na.ARextend(allD_x$adjCash[dateFlag]) +
                  na.ARextend(allD_x$cCPI[dateFlag]))$residuals
    purge1[, i] <- resid_i
}
purge1x <- xts(as.data.frame(purge1), order.by = index(fci_1stack))
names(purge1) <- names(fciCmpl)
#
fci1mod <- PCA(purge1, graph=FALSE)
fci1_f <- fci1mod$ind$coord # the first five factors
fci1_fx <- xts(fci1_f, order.by = index(fci_1stack))
fci1_sfx <- scale(fci1_fx)[, 1] # this is your MoM fci => NO-stack

##### ===> FCI_sfx is your Watson-ized FCI <=== ##### }}}
##### {{{ ===> GRF <=== ####

# note the GRF actually hurts out of sample forecasting performance for both
# the RBA and for CPI over a 12m horizon -- need to try an SVAR approach
# in particular, GRF should be exogenous to AUD conditions / policy.

# 1st make the normalisation ref series
effr_3mma <- rollapply(ref_x$effr, 3, sum, align = 'right')[dateFlag]
ip3mma <- rollapply(na.ARextend(allD_x$us_ip), 3, sum, align = 'right')[dateFlag]
usCPI3mma <- rollapply(ref_x$usCPI, 3, mean, align = 'right')[dateFlag]
D.ip3mma <- diff(ip3mma, log=TRUE)[dateFlag]
#
imG <- imputePCA(coredata(grf_x), ncp=1)
grfCmpl <- xts(imG$completeObs, order.by=tts)
# stack and select92+
gf_3stck <- cbind(grfCmpl, lag(grfCmpl), lag(grfCmpl, 2))[-c(1:2), ]
#
# first purge the data of fed policy, USIP, and US inflation
purgedGRF <- matrix(rep(0, ncol(gf_3stck)*nrow(gf_3stck)), nrow(gf_3stck), ncol(gf_3stck))
#
for (i in 1:ncol(gf_3stck)){
    resid_i <- lm(gf_3stck[,i] ~ effr_3mma[index(gf_3stck), ] +
                  D.ip3mma[index(gf_3stck), ] + usCPI3mma[index(gf_3stck), ])$residuals
    purgedGRF[,i] <- resid_i
}
#
purgedGRF_x <- xts(as.data.frame(purgedGRF), order.by=index(gf_3stck))
names(purgedGRF_x) <- names(gf_3stck)
#
GRFmod <- PCA(purgedGRF, graph=FALSE)
GRF_W <- GRFmod$var$coord
rownames(GRF_W) <- names(gf_3stck)
GRF_f <- GRFmod$ind$coord # the first five factors
GRF_f_x <- xts(GRF_f, order.by=index(gf_3stck))
GRF_sfx <- scale(GRF_f_x)[,1]

##### ===> GRF_sfx is your Watson-ized GRF <=== #####
# }}}
### {{{ ===> VAR Models
###^^^^^^^^^^^^^^###
###  VAR Models  ###
####################

# TODO: perhaps ... do by parts?
# add a larger VAR with the unemployment rate
# use OLS / lm to do so, and in the data / CPI equations restrict the params via:
# y ~ ... + I(ref_x$cash + ref_x + cashSpread) + ...
# in the cash rate equation, use cash ~ ... + offset(-1*lag(ref_x$cashSpread,1)) ...

#
XTRA <- cbind(diff(na.ARextend(allD_x$rCpixA), lag = 12, log=T),
              diff(na.ARextend(allD_x$rCpixS), lag = 12, log=T),
              diff(na.ARextend(allD_x$rCpixU), lag = 12, log=T),
              log(na.ARextend(allD_x$rCpixA)),
              log(na.ARextend(allD_x$rCpixS)),
              log(na.ARextend(allD_x$rCpixU)),
              diff(na.ARextend(allD_x$audusd), log=T),
              na.ARextend(allD_x$nairu),
              na.ARextend(ref_x$cCPI),
              na.ARextend(allD_x$jpm_reer),
              na.ARextend(allD_x$rCpixU),
              na.ARextend(allD_x$ur)
              )
XTRA3 <- rollapplyr(XTRA, 3, colMeans)
names(XTRA3) <- c('AUDlnD', 'SDRlnD', 'USDlnD', 'AUDl', 'SDRl', 'USDl', 'fx',
                  'nairu', 'cCPI', 'reer', 'rCpixU', 'ur'
                  )
#
icT <- "SC"

#################
# small MJ VAR  #
#################

# mjPCA ==> rmsfe :: 3m 28.4 (30.4); 6m 86.2 (99); 12m 273 (346) -- plot looks good too
MJsVAR <- cbind(sf_x, FCI_sfx, ref_x$cash)[-c(1:2),]
names(MJsVAR) <- c('aD', 'aFCI', 'cash')
#
MJsVAR.mod <- VAR(scale(MJsVAR), p = 5, ic = icT)
MJsVAR.mod2 <- VAR((MJsVAR), p = 5, ic = icT)

#################
# small MJ +CPI #
#################

# mjPCA + cCPI ==> rmsfe :: 3m 31.7 (30.7); 6m 95.3 (99.1); 12m 293.8 (341) -- plots are good too
MJscVAR <- cbind(sf_x, FCI_sfx, XTRA3$cCPI, ref_x$cash)[modStartDateFlag]
names(MJscVAR) <- c('aD', 'aFCI', 'cCPI', 'cash')
#
MJscVAR.mod <- VAR(scale(MJscVAR), p = 5, ic = icT)
MJscVAR.mod2 <- VAR((MJscVAR), p = 5, ic = icT)
MJscVAR.pp <- predict(VAR(MJscVAR, p = 5, ic = icT), n.ahead = 6)

####################
# RESTRICTED VARS ##
####################

# [mjPCA + cCPI] L4 (L3) ==> rmsfe :: 3m 32.9 (37.9); 6m 98.1 (119); 12m 309 (375)
MJsRcVAR <- cbind(FCI_sfx, sf_x, ref_x$cCPI, ref_x$cash)[modStartDateFlag]
names(MJsRcVAR) <- c('aFCI', 'aD', 'cCPI', 'cash')
#
MJsRcRST <- matrix(rep(1, 68), nrow = 4, ncol = 17)
rownames(MJsRcRST) <- c('aFCI', 'aD', 'cCPI', 'cash')
#
colnames(MJsRcRST) <- c('aFCI.l1', 'aD.l1', 'cCPI.l1', 'cash.l1',
                        'aFCI.l2', 'aD.l2', 'cCPI.l2', 'cash.l2',
                        'aFCI.l3', 'aD.l3', 'cCPI.l3', 'cash.l3',
                        'aFCI.l4', 'aD.l4', 'cCPI.l4', 'cash.l4',
                        'const'
                        )
# restrict CPI -- no cash
MJsRcRST[3, c( 4,
              8,
              12,
              16
              )] <- 0
#

# models
MJsRcVAR.mod <- VAR(scale(MJsRcVAR), p = 4)
MJsRcVAR.mod2 <- VAR(MJsRcVAR, p = 4)
#
MJsRcVAR.res <- restrict(MJsRcVAR.mod, method = 'manual', resmat = MJsRcRST)
MJsRcVAR.res2 <- restrict(MJsRcVAR.mod2, method = 'manual', resmat = MJsRcRST)

# [mjPCA] + {SDRl} L4 ==> rmsfe ::
MJsRcxVAR <- cbind(XTRA3$SDRl, FCI_sfx, sf_x, ref_x$cCPI, ref_x$cash)[modStartDateFlag]
names(MJsRcxVAR) <- c('xpx', 'aFCI', 'aD', 'cCPI', 'cash')
#
MJsRcxRST <- matrix(rep(1, 105), nrow = 5, ncol = 21)
rownames(MJsRcxRST) <- c('xpx', 'aD', 'aFCI', 'cCPI', 'cash')
#
colnames(MJsRcxRST) <- c('xpx.l1', 'aFCI.l1', 'aD.l1', 'cCPI', 'cash.l1',
                         'xpx.l2', 'aFCI.l2', 'aD.l2', 'cCPI.l1', 'cash.l2',
                         'xpx.l3', 'aFCI.l3', 'aD.l3', 'cCPI.l2', 'cash.l3',
                         'xpx.l4', 'aFCI.l4', 'aD.l4', 'cCPI.l3', 'cash.l4',
                         'const'
                         )
# make commodity prices exogenous
MJsRcxRST[1, c(2:5,
               7:10,
               12:15,
               17:20,
               21
               )] <- 0
# make cCPI exog to cash and xpx
MJsRcxRST[4, c(1:2, 5,
               6:7, 10,
               11:12, 15,
               16:17, 20
               )] <- 0

# models
MJsRcxVAR.mod <- VAR(scale(MJsRcxVAR), p = 4)
MJsRcxVAR.mod2 <- VAR(MJsRcxVAR, p = 4)
#
MJsRcxVAR.res <- restrict(MJsRcxVAR.mod, method = 'manual', resmat = MJsRcxRST)
MJsRcxVAR.res2 <- restrict(MJsRcxVAR.mod2, method = 'manual', resmat = MJsRcxRST)
MJsRcxVAR.pp24 <- predict(MJsRcxVAR.res2, n.ahead = 24)

# [mjPCA7] + xpx ~ REER :: fci ~ dd ~ UR ~ cpi ~ RBA

# larger VAR with proper treatment of the currency ...
var7 <- cbind(
              log(XTRA3$rCpixU), log(XTRA3$reer),
              FCI_sfx, sf_x, XTRA3$nairu, ref_x$cCPI, ref_x$cash)[modStartDateFlag]
#
names(var7) <- c('xpx', 'reer', 'aFCI', 'aD', 'ur', 'cCPI', 'cash')
#
var7RST <- matrix(rep(1, 154), nrow = 7, ncol = 22)
rownames(var7RST) <- c('xpx', 'reer', 'aFCI', 'aD', 'ur', 'cCPI', 'cash')
#
colnames(var7RST) <- c(
                       'xpx.l1', 'reer.l1', 'aFCI.l1', 'aD.l1', 'ur.l1', 'cCPI.l1', 'cash.l1',
                       'xpx.l2', 'reer.l2', 'aFCI.l2', 'aD.l2', 'ur.l2', 'cCPI.l2', 'cash.l2',
                       'xpx.l3', 'reer.l3', 'aFCI.l3', 'aD.l3', 'ur.l3', 'cCPI.l3', 'cash.l3',
                       'const'
                       )
# make commodity prices exogenous
var7RST[1,   c( 2:7,
                9:14,
               16:21
               )] <- 0
# allow reer to be driven by xpx
var7RST[2,   c( 3:7,
               10:14,
               17:21
               )] <- 0
# make cCPI = f(ur, L(cCPI))
var7RST[6,   c( 1:4, 7,
                8:11, 14,
               15:18, 21
               )] <- 0

# irf
var7.mod <- VAR(scale(var7), p = 3)
var7.mod2 <- VAR(var7, p = 3)
#
var7.res <- restrict(var7.mod, method = 'manual', resmat = var7RST)
var7.res2 <- restrict(var7.mod2, method = 'manual', resmat = var7RST)

var7.pp24 <- predict(var7.res2, n.ahead = 24)

# }}} end VAR model bit
# {{{ the forecast combinator functions etc.
## packaging up the bits of the VAR models
check2deep <- function(Env, lvl1, lvl2) {
    EnvN <- ls(Env)
    checkFun <- function(eNames) {
        x <- eNames[1]
        if(lvl1 %in% names(get(x, Env))) {
            if(lvl2 %in% (names(get(lvl1, (get(x, Env)))))) TRUE else FALSE
        } else FALSE
    }
    recList <- function(lat) {
        if(!length(lat)) {
            list()
        } else {
            if(checkFun(lat)) {
                c(lat[1], recList(lat[-1]))
            } else {
                recList(lat[-1])
            }
        }
    }
    recList(EnvN)
}

fetch2deep <- function(Env, lvl1, lvl2) {
    lapply(check2deep(Env, lvl1, lvl2),
           function(X) get(lvl2, get(lvl1, get(X, Env))))
}

combineVARPred <- function(inD, outName, nAhead, confInt, lvl1, lvl2) {
    eOut <- new.env()
    makePred <- function(inD, outName, nAhead, confInt) {
        assign(
               paste0(outName, paste0(".pp", nAhead, "m.", confInt)),
               value = predict(inD, n.ahead = nAhead, ci = confInt/100),
               envir = eOut
               )
    }
    # note the mapply step sends to results to eOut thanks to makePred
    mapply(makePred, inD, outName, MoreArgs = list(nAhead, confInt))
    Reduce("+", fetch2deep(eOut, lvl1, lvl2)) / length(check2deep(eOut, lvl1, lvl2))
}

# collect all the var models and generate forecasts
dfs <- list("rbaVAR" = rbaVAR, "MJsVAR" = MJsVAR, "MJscVAR" = MJscVAR,
            "MJsRcVAR" = MJsRcVAR, "MJsRcxVAR" = MJsRcxVAR, "var7" = var7)
skips <- list(165, 165, 165, 165, 165, 165)
lags <- list(3, 3, 3, 12, 12, 12)
icT <- list("SC", "SC", "SC", "SC", "SC", "SC")
rsts <- list(NULL, NULL, NULL, MJsRcRST, MJsRcxRST, var7RST)
vars <- list(frames = dfs, skip = skips, maxLag = lags, info = icT, RM = rsts)
varMods_std <- list(rbaVAR.mod, MJsVAR.mod2, MJscVAR.mod2, MJsRcVAR.res2, MJsRcxVAR.res2, var7.res2)
varOutNames <- list("rbaVAR", "MJsVAR", "MJscVAR", "MJsRcVAR", "MJsRcxVAR", "var7")

# put the conf int stuff into it's own environment
confEnv <- new.env()
for(conf in c(30, 60, 90)) {
    for(mons in c(6, 12, 18, 24)) {
        assign(paste0("combo.pp", mons, "m.cash_", conf),
               value = combineVARPred(varMods_std,
                                      varOutNames,
                                      nAhead = mons,
                                      confInt = conf,
                                      'fcst',
                                      'cash'),
               envir = confEnv)
    }
}

for(conf in c(30, 60, 90)) {
    for(mons in c(6, 12, 18, 24)) {
        assign(paste0("combo.pp", mons, "m.cCPI_", conf),
               value = combineVARPred(varMods_std,
                                      varOutNames,
                                      nAhead = mons,
                                      confInt = conf,
                                      'fcst',
                                      'cCPI'),
               envir = confEnv)
    }
}

# a function to generate, combine and summarise predictions from VAR models
makePOOScombo <- function(inMods, outName, nAhead, ff) {
    eOut <- new.env()
    # function to generate predictions from VAR models: assigns results to eOut
    POOSpred <- function(Mdl, outName, nAhead, X) {
        assign(
               paste0(outName[[X]], paste0(".",nAhead, "m.poos")),
               value = testVar(dframe = get('frames', Mdl)[[X]],
                               skip = get('skip', Mdl)[[X]],
                               nAhead = nAhead,
                               Vlag = get('maxLag', Mdl)[[X]],
                               IC = get('info', Mdl)[[X]],
                               RSTmtx = get('RM', Mdl)[[X]]),
               envir = eOut)
    }
    # apply the prediction function
    lapply(1:length(inMods$frames), function(X) POOSpred(inMods, outName, nAhead, X))
    # function to checks models in eOut (via ls(eOut)) to see if they contain the target variable
    checkEnvVars <- function(eNames, target) {
        if(!length(eNames)) {
            list()
        } else {
            if(target %in% names(get(eNames[1], eOut))) {
                c(eNames[1], checkEnvVars(eNames[-1], target))
            } else {
                checkEnvVars(eNames[-1], target)
            }
        }
    }
    # match up the forcasts for each variable: returns a list of model names
    multiModelSummary <- function(Variable, Env, ff) {
        apply(
              sapply(checkEnvVars(ls(Env), Variable),
                     function(X) get(Variable, get(X, Env)),
                     simplify = 'array'),
              1:2,
              ff)
    }
    # find largest VAR -- all smaller VARs must be a subset of the largest one
    EnvN <- ls(eOut)
    largeModel <- EnvN[which.max(lapply(1:length(ls(eOut)),
                                        function(X) length(get(ls(eOut)[X], eOut))))]
    sapply(ff, function(fun) {
           sapply(names(get(largeModel, eOut)),
                  multiModelSummary,
                  eOut,
                  fun,
                  simplify = FALSE
                  )},
           simplify = FALSE)
}

# simulate once for the longest (say 24m) and then to subset the result
modelAgSummary_24m <- makePOOScombo(inMods = vars,
                                    outName = varOutNames,
                                    nAhead = 24,
                                    ff = c("mean", "median", "min", "max"))

# create dates to attach to the forecasts
modDates <- seq(modStartDate, by = 'mon', length.out = nrow(modelAgSummary_24m$mean$cash))

# a function to trim the modelAg to size, and stick dates back on
trimSummary <- function(agSum, monDepth, dates) {
    trimDown <- function(inD) {
        for(j in 2:ncol(inD)) {
            trimRows <- which(!is.na(inD[,j]))[-c(1:monDepth)]
            inD[trimRows, j] <- NA
        }
        maxNonNA <- max(which(!is.na(inD[,ncol(inD)])))[1]
        xts(inD, order.by = dates)[1:maxNonNA,]
    }
    outList <- list()
    for(name in names(modelAgSummary_24m)) {
        outList[[name]] <- lapply(get(name, agSum), trimDown)
    }
    outList
}

# makes the summaries of the 6m, 12m and 18m Ahead forecasts from the 24m output
ag6 <- trimSummary(modelAgSummary_24m, 6, modDates)
ag12 <- trimSummary(modelAgSummary_24m, 12, modDates)
ag18 <- trimSummary(modelAgSummary_24m, 18, modDates)

comboPOOS6 <- list(cash = ag6$mean$cash['1999::'],
                   aD = ag6$mean$aD['1999::'],
                   cCPI = ag6$mean$cCPI['1999::'])
comboPOOS12 <- list(cash = ag12$mean$cash['1999::'],
                    aD = ag12$mean$aD['1999::'],
                    cCPI = ag12$mean$cCPI['1999::'])

# }}} end forecast combos
# {{{ make forecast error bands

# this function combines the averaged conf ints and model preds -- to make fancharts
buildFanObj <- function(variName, confs, prd) {
    agObj <- trimSummary(modelAgSummary_24m, prd, modDates)
    maxCol <- ncol(get(variName, get('mean', agObj)))
    fanObj <- data.frame(
                         Act = get(variName, get('mean', agObj))[,1],
                         ave = get(variName, get('mean', agObj))[,maxCol],
                         med = get(variName, get('median', agObj))[,maxCol],
                         min = get(variName, get('min', agObj))[,maxCol],
                         max = get(variName, get('max', agObj))[,maxCol]
                         )
    confIntRows <- (nrow(fanObj) - (prd - 1)):nrow(fanObj)
    variPeriodMatches <- grepl(paste0(prd, "m"), ls(confEnv)) * grepl(variName, ls(confEnv))
    confMatches <- ls(confEnv)[as.logical(variPeriodMatches)]
    for(ci in confs) {
        fanObj[paste0("MML",ci)] <- fanObj$ave
        fanObj[paste0("MMU",ci)] <- fanObj$ave
        fanObj[confIntRows, paste0("MML",ci)] <- get(grep(ci,
                                                          confMatches,
                                                          value=TRUE), confEnv)[, 'lower']
        fanObj[confIntRows, paste0("MMU",ci)] <- get(grep(ci,
                                                          confMatches,
                                                          value=TRUE), confEnv)[, 'upper']
    }
    lastNonNA <- max(which(!is.na(fanObj$Act)))
    fanObj[lastNonNA, -1] <- fanObj[lastNonNA, 1]
    xts(fanObj, order.by = as.Date(rownames(fanObj)))
}
#
# debugonce(buildFanObj)
cash4c_6 <- buildFanObj('cash', c(30, 60, 90), 6)
cCPI4c_6 <- buildFanObj('cCPI', c(30, 60, 90), 6)
cash4c_12 <- buildFanObj('cash', c(30, 60, 90), 12)
cCPI4c_12 <- buildFanObj('cCPI', c(30, 60, 90), 12)
cash4c_18 <- buildFanObj('cash', c(30, 60, 90), 18)
cCPI4c_18 <- buildFanObj('cCPI', c(30, 60, 90), 18)
cCPI4c_24 <- buildFanObj('cCPI', c(30, 60, 90), 24)
# }}}
## {{{ integrate with market pricing ==>

histStart <- as.Date("1993-01-01")
today <- as.Date(as.POSIXlt(Sys.time(), tz='Australia/Sydney')) # end date is today
## bbg tickers
securities <- c("RBACTRD Index", "IB1 Comdty", "IB2 Comdty", "IB3 Comdty", "IB4 Comdty", "IB5 Comdty", "IB6 Comdty",
                "IB7 Comdty", "IB8 Comdty", "IB9 Comdty", "IB10 Comdty", "IB11 Comdty", "IB12 Comdty",
                "IB13 Comdty", "IB14 Comdty", "IB15 Comdty", "IB16 Comdty", "IB17 Comdty", "IB18 Comdty")
mktRef <- c("RBACTRD Index", "ADSWAP1Q Curncy", "ADSWAP2Q Curncy", "ADSWAP3Q Curncy",
            "ADSWAP5 Curncy", "ADSWAP10 Curncy", "GACGB2 Index", "GACGB3 Index")
midFields <- c("BID", "ASK")
#
# connect to bbg and get data into usable format
conn <- blpConnect(blpapi.jar.file = helpEnv$Bjar, verbose=FALSE)
currentMkt <- bdp(conn, securities[-1], midFields)
currentRBA <- bdp(conn, "RBACTRD Index", "PX_LAST")
histMkt <- bdh(conn, mktRef, "PX_LAST", start_date = histStart, include.non.trading.days = FALSE)
blpDisconnect(conn) # now close the connection
#
# unstack the historical data and add a spread
IRSx <- na.locf(bbgUnstacker(histMkt))
IRSx_m <- apply.monthly(IRSx, mean)
index(IRSx_m) <- toLastDay(index(IRSx_m), toFirst=TRUE)
IRSx_m$cash2y <- with(IRSx_m, 100*(ADSWAP2Q - RBACTRD))
IRSx_m$cash3y <- with(IRSx_m, 100*(ADSWAP3Q - RBACTRD))
IRSx_m$cash2g <- with(IRSx_m, 100*(GACGB2 - RBACTRD))
IRSx_m$cash3g <- with(IRSx_m, 100*(GACGB3 - RBACTRD))
IRSx_m$i2x5 <- with(IRSx_m, 100*(ADSWAP5 - ADSWAP2Q))
IRSx_m$i5x10 <- with(IRSx_m, 100*(ADSWAP10 - ADSWAP5))

# take the mid
currentMkt$MID <- (currentMkt[,1] + currentMkt[,2])/2

# stick mids together with RBA rate
midList <- as.list(c(currentRBA$PX_LAST, t(currentMkt$MID)))
mid_x <- xts(as.data.frame(midList), order.by = today)
names(mid_x) <- c('RBACTRD', 'IB1', 'IB2', 'IB3', 'IB4', 'IB5', 'IB6', 'IB7', 'IB8', 'IB9', 'IB10', 'IB11',
                  'IB12', 'IB13', 'IB14', 'IB15', 'IB16', 'IB17', 'IB18')
mid_x[, -1] <- 100 - mid_x[, -1]

meetingDates <- firstTuesday(index(mid_x), numcol = 17)
adjIbRate <- ibAdjusteR(mid_x, meetingDates) # this throws an error on early month pre meeting date
adjIb_x <- xts(t(coredata(adjIbRate)),
               order.by = as.Date(t(meetingDates$meetDay)[,1]))

index(adjIb_x) <- toLastDay(index(adjIb_x), toFirst = TRUE)


cash4c_6$mkt <- cash4c_6$ave
cash4c_6$mkt[index(tail(cash4c_6, 6))] <- adjIb_x[index(tail(cash4c_6, 6))]

cash4c_12$mkt <- cash4c_12$ave
cash4c_12$mkt[index(tail(cash4c_12, 12))] <- adjIb_x[index(tail(cash4c_12,12))]
## }}} close market pricing stuff
#### {{{ ===> Charts <=== ####

pp_DataScores <- function()
{
    plot.zoo(scores['2003::'][,-1], col=2:3, main="AUD Data Pulse: 2003--2012", las=1, screens=1, xlab="", ylab="", lwd=2)
    legend('topleft', c('3mma', '6mma'), horiz=T, lty = c(1, 1), col = c(2, 3), bty='n')
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
}

pp_demandCombo_04p <- function()
{
    plot(sf_x['2004::'], type = 'o', pch = 19, main = "UBS Demand Indices", las=1,
         major.format = "%b-%y")
    lines(dd1_x['2004::'], type = 'l', lwd = 2, col = 3)
    legend('topright', c('3m stack', 'monthly'), col = c(1, 3), lwd = c(1, 2), pch = c(19, NA), horiz=T, bg='grey94')
    mtext(text="Source: UBS, Bloomberg", side=1, line=4, adj=1)
}

pp_fciCombo_04p <- function()
{
    plot(-1*FCI_sfx['2004::'], type = 'o', pch = 19, main = "UBS Financial Conditions Indices", las=1,
         major.format = "%b-%y", ylim = c(-1.5, 3))
    lines(-1*fci1_sfx['2004::'], type = 'l', lwd = 2, col = 3)
    legend('topright', c('3m stack', 'monthly'), col = c(1, 3), lwd = c(1, 2), pch = c(19, NA), horiz=T, bg='grey94')
    mtext(text="Source: UBS, Bloomberg", side=1, line=4, adj=1)
}

pp_fciCombo_04pCash <- function()
{
    par(mar = c(5, 4, 4, 4) +0.1)
    plot(-1*FCI_sfx['2004::'], type = 'o', pch = 19, main = "UBS Financial Conditions Indices", las=1,
         major.format = "%b-%y", ylim = c(-1.5, 3))
    lines(-1*fci1_sfx['2004::'], type = 'l', lwd = 2, col = 3)
    par(new = TRUE)
    plot(coredata(ref_x$cash['2004::']) ~ index(ref_x['2004::']), type='l', col=2, xaxt='n',
         yaxt='n', xlab='', ylab='', las=1, lwd=2, ylim = c(2, 8))
    axis(4, at = c(3:7), labels = c(3:7), las=1)
    mtext('RBA Cash', side = 4, line = 2)
    legend('topright', c('3m stack', 'monthly', 'RBA cash'), col = c(1, 3, 2), lwd = c(1, 2, 2), pch = c(19, NA, NA),
           horiz=F, bg='grey94')
    mtext(text="Source: UBS, Bloomberg", side=1, line=4, adj=1)
}

pp_VAR_cCPIfan <- function()
{
    fanchart(MJsRcxVAR.pp24, names="cCPI", las=1, main="VAR model (incl xPX): core CPI %YoY")
}

pp_CashFanFilled <- function(fanObj)
{
    fanRange12 <- range(fanObj$MMU90, fanObj$MML90, na.rm=TRUE)
    fanDates12 <- seq(index(fanObj)[(nrow(fanObj) - 15)], index(fanObj)[(nrow(fanObj))], by='mon')
    plot(coredata(fanObj$Act[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)],
         type = 's', ylim = fanRange12, lwd = 6, las = 1,
         xlab = '', ylab = '',
         main = "RBA 12m fwd fanchart (12m 4c): median / 30% / 60% / 90% conf int")
    lines(coredata(fanObj$MML90[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 'grey74')
    lines(coredata(fanObj$MMU90[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 'grey74')
    polygon(c(fanDates12, fanDates12[16:1]),
            c(coredata(fanObj$MMU90[(nrow(fanObj) - 15):nrow(fanObj)]),
              coredata(fanObj$MML90[(nrow(fanObj) - 15):nrow(fanObj)])[16:1]
              ), col = 'grey74', border = NA
            )
    lines(coredata(fanObj$MML60[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 'grey47')
    lines(coredata(fanObj$MMU60[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 'grey47')
    polygon(c(fanDates12, fanDates12[16:1]),
            c(coredata(fanObj$MMU60[(nrow(fanObj) - 15):nrow(fanObj)]),
              coredata(fanObj$MML60[(nrow(fanObj) - 15):nrow(fanObj)])[16:1]
              ), col = 'grey47', border = NA
            )
    lines(coredata(fanObj$MML30[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 'orange')
    lines(coredata(fanObj$MMU30[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 'orange')
    polygon(c(fanDates12, fanDates12[16:1]),
            c(coredata(fanObj$MMU30[(nrow(fanObj) - 15):nrow(fanObj)]),
              coredata(fanObj$MML30[(nrow(fanObj) - 15):nrow(fanObj)])[16:1]
              ), col = 'orange', border = NA
            )
    lines(coredata(fanObj$ave[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)], col = 7, lwd=4)
    if("mkt" %in% names(fanObj)) {
        lines(coredata(fanObj$mkt[(nrow(fanObj) - 15):nrow(fanObj)]) ~ index(fanObj)[(nrow(fanObj) - 15):nrow(fanObj)],
              col = 6, lwd=2, pch=5, type = 'o', cex = 1.25)
    }
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
}

pp_CashFanFilled_6m <- function() pp_CashFanFilled(cash4c_6)
pp_CashFanFilled_12m <- function() pp_CashFanFilled(cash4c_12)
pp_cCPIFanFilled_6m <- function() pp_CashFanFilled(cCPI4c_6)
pp_cCPIFanFilled_12m <- function() pp_CashFanFilled(cCPI4c_12)

pp_faveVAR_mkt_6m <- function()
{
    plot.zoo(cash4c_6[(nrow(cash4c_6) - 9):nrow(cash4c_6), c(1, 7, 17)], screen=1,
             col = c(1, 3, 6),
             type = c('s', rep('l', 2), 'o'), las=1, lwd = c(3, 2, 2),
             main = "UBS RBA cash rate model (Ave) v. mkt implied",
             xlab = "",
             ylab = "",
             #          ylim = c(1.00, 4.50)
             )
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
}

pp_faveVAR_mkt_12m <- function()
{
    plot.zoo(cash4c_12[(nrow(cash4c_12) - 15):nrow(cash4c_12), c(1, 7, 17)], screen=1,
             col = c(1, 3, 6),
             type = c('s', rep('l', 2), 'o'), las=1, lwd = c(3, 2, 2),
             main = "UBS RBA cash rate model (Ave) v. mkt implied",
             xlab = "",
             ylab = "",
             #          ylim = c(1.00, 4.50)
             )
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
}

pp_AveCashModel6 <- function()
{
    plot.zoo(cash4c_6[(nrow(cash4c_6) - 9):nrow(cash4c_6), c(1, 9, 10:11, 18)],
             screen=1, col = c(1, 3, 4, 2, 6),
             type = c('s', 'l', 'l', 'l', 'o'), las=1, lwd = c(3,2,1,1,1),
             pch = c(rep(NA, 4), 5),
             main = "UBS RBA Policy VARs: cash rate models (6m ahead)",
             xlab = "",
             ylab = "",
             ylim = c(2.00, 3.75)
             )
    abline(h = cash4c_6[(nrow(cash4c_6) - 6), 1] - seq(-0.75, 1.5, 0.25), col=8, lty=3)
    legend('top',
           c('Actual', 'Median Model', 'Min', 'Max', 'mkt'),
           col = c(1, 3, 4, 2, 6),
           pch = c(rep(NA, 4), 5),
           lwd = c(3, 2, 1, 1, 2),
           bg='grey94', horiz = T, cex = 0.8)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
}

pp_AveCashModel12m <- function()
{
    plot.zoo(cash4c_12[(nrow(cash4c_12) - 15):nrow(cash4c_12), c(1, 9, 10:11, 18)],
             screen=1, col = c(1, 3, 4, 2, 6),
             type = c('s', 'l', 'l', 'l', 'o'), las=1, lwd = c(3,2,1,1,1),
             pch = c(rep(NA, 4), 5),
             main = "UBS RBA Policy VARs: cash rate models (12m ahead)",
             xlab = "",
             ylab = "",
             ylim = c(2.00, 3.75)
             )
    abline(h = cash4c_12[(nrow(cash4c_12) - 12), 1] - seq(-0.75, 1.5, 0.25), col=8, lty=3)
    legend('top',
           c('Actual', 'Median Model', 'Min', 'Max', 'mkt'),
           col = c(1, 3, 4, 2, 6),
           pch = c(rep(NA, 4), 5),
           lwd = c(3, 2, 1, 1, 2),
           bg='grey94', horiz = T, cex = 0.8)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
}


if (fromSource())
{
    pdf(file.path(plotPATH, "AveCashModel6.pdf"))
    pp_AveCashModel6()
    dev.off()

    pdf(file.path(plotPATH, "AveCashModel12m.pdf"))
    pp_AveCashModel12m()
    dev.off()

    ## add the market projection to the following two charts
    pdf(file.path(plotPATH, "CashFanSkeleton_6m.pdf"))
    plot.zoo(cash4c_6[(nrow(cash4c_6) - 9):nrow(cash4c_6), c(1, 7, 11:16, 17)], screen=1,
             col = c(1, 3, rep(c('grey75', 'grey45', 'grey18'), 2), 6),
             type = c('s', rep('l', 7), 'o'), las=1, lwd = c(4,3, rep(2, 7)),
             main = "UBS RBA Policy fanchart: 30% / 60% / 90%",
             xlab = "",
             ylab = "",
             ylim = c(1.00, 4.50),
             cex = 1.75
             )
    abline(h = cash4c_6[(nrow(cash4c_6) - 6), 1] - seq(-2, 2, 0.25), col=8, lty=3)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "CashFanSkeleton_12m.pdf"))
    plot.zoo(cash4c_12[(nrow(cash4c_12) - 15):nrow(cash4c_12), c(1, 7, 11:16, 17)], screen=1,
             col = c(1, 3, rep(c('grey75', 'grey45', 'grey18'), 2), 6),
             type = c('s', rep('l', 7), 'o'), las=1, lwd = c(4,3, rep(2, 7)),
             main = "UBS RBA Policy fanchart: 30% / 60% / 90%",
             xlab = "",
             ylab = "",
             ylim = c(0, 4),
             cex = 1.75
             )
    abline(h = cash4c_12[(nrow(cash4c_12) - 12), 1] - seq(-2, 2, 0.25), col=8, lty=3)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    ## forecast ave VAR + mkt 6m
    pdf(file.path(plotPATH, "faveVAR_mkt_6m.pdf"))
    pp_faveVAR_mkt_6m()
    dev.off()

    ## add the market projection to the following two charts
    pdf(file.path(plotPATH, "faveVAR_mkt_12m.pdf"))
    pp_faveVAR_mkt_12m()
    dev.off()

    # 6m ahead fan chart
    pdf(file.path(plotPATH, "CashFanFilled_6m.pdf"))
    pp_CashFanFilled_6m()
    dev.off()

    # 12m ahead fan chart
    pdf(file.path(plotPATH, "CashFanFilled_12m.pdf"))
    pp_CashFanFilled_12m()
    dev.off()

    pdf(file.path(plotPATH, "VAR_cCPIfan.pdf"))
    pp_VAR_cCPIfan()
    dev.off()

    pdf(file.path(plotPATH, "VAR_cashfan.pdf"))
    fanchart(MJsRcxVAR.pp24, names="cash", las=1, main="VAR model (incl xPX): RBA policy rate")
    dev.off()

    pdf(file.path(plotPATH, "VAR_demand.pdf"))
    fanchart(MJsRcxVAR.pp24, names="aD", las=1, main="VAR model (incl xPX): RBA policy rate")
    dev.off()

    pdf(file.path(plotPATH, "cash_vAdj.pdf"))
    plot(ref_x$cash['1999::'], type='o', pch=20, las=1,
         main='RBA Cash v. Spread Adjusted', ylim=c(2,8))
    lines(ref_x$adjCash['1999::'], col=3, lwd=2)
    dev.off()

    pdf(file.path(plotPATH, "demandF.pdf"))
    plot(sf_x, type='o', pch=20, las=1, main="Demand Indices", ylim = c(-4.5, 2))
    legend('topright', c('UBS_Demand'), col = c(1), lwd = c(2), pch = c(19), horiz=T)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "demandF_rGDI.pdf"))
    plot(sf_x, type = 'l', las=1, main = "UBS Demand Index & ABS Real GDI %QoQ", ylim = c(-3.5, 3.5))
    lines(sf_x, type = 'o', pch=19, lwd=2, col=1)
    lines(scale(diff(rGDIx, log=T)), type='l', pch=20, lwd=2, col=6)
    legend('topleft', c('UBS_Demand', 'rGDI'), col = c(1, 6), lwd = c(2, 2), pch = c(19, NA), horiz=F, bg = 'lightgray')
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "demandF_rGDI_06p.pdf"))
    plot(sf_x['2006::'], type = 'l', las=1, main = "UBS Demand Index & ABS Real GDI %QoQ", ylim = c(-3.5, 3.5))
    lines(sf_x['2006::'], type = 'o', pch=19, lwd=2, col=1)
    lines(scale(diff(rGDIx, log=T))['2006::'], type='l', pch=20, lwd=2, col=6)
    legend('topleft', c('UBS_Demand', 'rGDI'), col = c(1, 6), lwd = c(2, 2), pch = c(19, NA), horiz=F, bg = 'lightgray')
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "demandF_06p.pdf"))
    plot(sf_x['2006::'], type='o', pch=20, las=1, main="Demand Indices", ylim = c(-4, 2))
    legend('topright', c('UBS_Demand'), col = c(1), lwd = c(2), pch = c(19), horiz=T, bg = 'grey94')
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "FCI.pdf"))
    plot(-1*FCI_sfx, type='o', pch=20, las=1, main="Financial Conditions Indices", ylim = c(-3, 3))
    legend('topleft', c('UBS_FCI'), col = c(1), lwd = c(2), pch = c(19), horiz=T, bg = 'grey94')
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "FCI_06p.pdf"))
    plot(-1*FCI_sfx['2006::'], type='o', pch=20, las=1, main="UBS Financial Conditions Index", ylim = c(-1.5, 3))
    legend('topright', c('UBS_FCI'), col = c(1), lwd = c(2), pch = c(19), horiz=T, bg = 'grey94')
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "capacity.pdf"))
    plot(allD_x$cap_util, type='o', pch=20, ylim=c(66,86), main='Capacity Utilisation', las=1)
    lines(allD_x$apm_capU_nsa, col=2, type='o', pch=16)
    legend('top', c('NAB', 'ManuPMI'), horiz=T, pch = c(20, 16), col = c(1, 2), bty='n')
    abline(h=last(allD_x$cap_util), lty=2)
    abline(h=last(allD_x$apm_capU_nsa), col=2, lty=2)
    dev.off()

    pdf(file.path(plotPATH, "nabJobs.pdf"))
    plot(scale(allD_x$nab_empl['1997::']), type='o', pch=20, main="NAB Employment Index")
    dev.off()

    pdf(file.path(plotPATH, "anz.pdf"))
    plot(scale(allD_x$anz_nja['1997::']), type='l', ylim=c(-3,3), main='ANZ Job Ads', las=1)
    lines(scale(allD_x$anz_ija['1997::']), type='l', col=4)
    abline(h=last(scale(allD_x$anz_nja['1997::'])), lty=2)
    abline(h=last(scale(allD_x$anz_ija)), col=4, lty=2)
    legend('top', c('News', 'Internet'), horiz=T, lty = c(1, 1), col = c(1, 4), bty='n')
    dev.off()

    pdf(file.path(plotPATH, "nab.pdf"))
    plot(allD_x$nab_empl['1997::'], type='o', pch=20, main="NAB Employment Index", las=1)
    abline(h = last(allD_x$nab_empl), lty=2)
    dev.off()

    pdf(file.path(plotPATH, "FCI_cash.pdf"))
    par(mar=c(5,4,4,4)+0.1)
    plot(-1*FCI_sfx['1999::'], type='o', pch=20, las=1, main="FCI v Cash Rate", ylab="+ve = tighter than average", xlab="")
    par(new=TRUE)
    plot(coredata(ref_x$cash['1999::']) ~ index(ref_x['1999::']), type='l', col=2,
         xaxt='n', yaxt='n', xlab='', ylab='', las=1, lwd=2)
    axis(4, at = c(3:7), labels = c(3:7), las=1)
    mtext("RBA Cash", side=4, line=3)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    legend('topleft', c('UBS_FCI', 'RBAcash'), horiz=F, pch = c(20, NA), lwd = c(1,2), col = c(1, 2), bg = 'grey94')
    dev.off()

    pdf(file.path(plotPATH, "FCI_cash_04p.pdf"))
    par(mar=c(5,4,4,4)+0.1)
    plot(-1*FCI_sfx['2004::'], type='o', pch=20, las=1, main="FCI v Cash Rate", ylab="+ve = tighter than average", xlab="")
    par(new=TRUE)
    plot(coredata(ref_x$cash['2004::']) ~ index(ref_x['2004::']), type='l', col=2, xaxt='n',
         yaxt='n', xlab='', ylab='', las=1, lwd=2)
    axis(4, at = c(3:7), labels = c(3:7), las=1)
    mtext("RBA Cash", side=4, line=3)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    legend('topright', c('UBS_FCI', 'RBAcash'), horiz=F, pch = c(20, NA), lwd = c(1,2), col = c(1, 2), bg = 'grey94')
    dev.off()

    pdf(file.path(plotPATH, "FCI_cash_06p.pdf"))
    par(mar=c(5,4,4,4)+0.1)
    plot(-1*FCI_sfx['2006::'], type='o', pch=20, las=1, main="FCI v Cash Rate", ylab="+ve = tighter than average", xlab="")
    par(new=TRUE)
    plot(coredata(ref_x$cash['2006::']) ~ index(ref_x['2006::']), type='l', col=2, xaxt='n',
         yaxt='n', xlab='', ylab='', las=1, lwd=2)
    axis(4, at = c(3:7), labels = c(3:7), las=1)
    mtext("RBA Cash", side=4, line=3)
    mtext(text="Source: UBS, ABS, RBA, Bloomberg", side=1, line=4, adj=1)
    legend('topright', c('UBS_FCI', 'RBAcash'), horiz=F, pch = c(20, NA), lwd = c(1,2), col = c(1, 2), bg = 'grey94')
    dev.off()

    pdf(file.path(plotPATH, "DataScores.pdf"))
    pp_DataScores()
    dev.off()

    pdf(file.path(plotPATH, "mjGRF.pdf"))
    plot(GRF_sfx['1990::'], type='o', pch=20, main="Global Risk Factor (+ve = more risk)", las=1, major.format = "%b-%y")
    legend('bottomleft', c('UBS_GlobalRisk'), col = c(1), lwd = c(2), pch = c(19), horiz=T, bg='grey94')
    mtext(text="Source: UBS, Bloomberg", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "demandCombo_04p.pdf"))
    pp_demandCombo_04p()
    dev.off()

    pdf(file.path(plotPATH, "fciCombo_04p.pdf"))
    pp_fciCombo_04p()
    dev.off()

    pdf(file.path(plotPATH, "fciCombo_04pCash.pdf"))
    pp_fciCombo_04pCash()
    dev.off()

    pdf(file.path(plotPATH, "rbaVAR_cashPOOS6.pdf"))
    plot.zoo(rbaVAR.test6$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(rbaVAR.test6$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(rbaVAR.test6$cash) - 1)),
             type = c('s', rep('l', ncol(rbaVAR.test6$cash) - 1)),
             main = "Pseudo Out of Sample forecasts: RBA VAR",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "rbaVAR_cashPOOS12.pdf"))
    plot.zoo(rbaVAR.test12$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(rbaVAR.test12$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(rbaVAR.test12$cash) - 1)),
             type = c('s', rep('l', ncol(rbaVAR.test12$cash) - 1)),
             main = "Pseudo Out of Sample forecasts: RBA VAR",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "domModelRc.pdf"))
    plot.zoo(MJsRcVAR.test6$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(MJsRcVAR.test6$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(MJsRcVAR.test6$cash) - 1)),
             type = c('s', rep('l', ncol(MJsRcVAR.test6$cash) - 1)),
             main = "Pseudo Out of Sample (6m forecasts): Domestic Bloc",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "domModelRc.pdf"))
    plot.zoo(MJsRcVAR.test12$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(MJsRcVAR.test12$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(MJsRcVAR.test12$cash) - 1)),
             type = c('s', rep('l', ncol(MJsRcVAR.test12$cash) - 1)),
             main = "Pseudo Out of Sample (12m forecasts): Domestic Bloc",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "POOS6_DomAndExportPx.pdf"))
    plot.zoo(MJsRcxVAR.test6$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(MJsRcxVAR.test6$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(MJsRcxVAR.test6$cash) - 1)),
             type = c('s', rep('l', ncol(MJsRcxVAR.test6$cash) - 1)),
             main = "Pseudo Out of Sample forecasts: Dom + Export prices",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "POOS12_DomAndExportPx.pdf"))
    plot.zoo(MJsRcxVAR.test12$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(MJsRcxVAR.test12$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(MJsRcxVAR.test12$cash) - 1)),
             type = c('s', rep('l', ncol(MJsRcxVAR.test12$cash) - 1)),
             main = "Pseudo Out of Sample forecasts: Dom + Export prices",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    ## this is the 12m ahead POOS chart -- really this looks better as a 6m chart! -- make 6m __.test and combine
    pdf(file.path(plotPATH, "aveVAR_cashPOOS_12m.pdf"))
    plot.zoo(comboPOOS12$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(comboPOOS12$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(comboPOOS12$cash) - 1)),
             type = c('s', rep('l', ncol(comboPOOS12$cash) - 1)),
             main = "Pseudo Out of Sample (12m forecasts): 6 model Average",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "aveVAR_cashPOOS_6m.pdf"))
    plot.zoo(comboPOOS6$cash['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(comboPOOS6$cash)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(comboPOOS6$cash) - 1)),
             type = c('s', rep('l', ncol(comboPOOS6$cash) - 1)),
             main = "Pseudo Out of Sample (6m forecasts): 6 model Average",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "aveVAR_cCPIPOOS_12m.pdf"))
    plot.zoo(comboPOOS12$cCPI['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(comboPOOS12$cCPI)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(comboPOOS12$cCPI) - 1)),
             type = c('s', rep('l', ncol(comboPOOS12$cCPI) - 1)),
             main = "Pseudo Out of Sample (12m forecasts): 4 model Average",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

    pdf(file.path(plotPATH, "aveVAR_cCPIPOOS_6m.pdf"))
    plot.zoo(comboPOOS6$cCPI['1999::'],
             screen=1,
             col=c(1, rep(8, ncol(comboPOOS6$cCPI)-1)),
             las=1,
             lwd = c(3, rep(1, ncol(comboPOOS6$cCPI) - 1)),
             type = c('s', rep('l', ncol(comboPOOS6$cCPI) - 1)),
             main = "Pseudo Out of Sample (6m forecasts): 4 model Average",
             xlab = "",
             ylab = ""
             )
    mtext(text="Source: UBS, RBA ", side=1, line=4, adj=1)
    dev.off()

}
## }}}
# {{{ match FCI to short end curve shape

# a custom function for making a period factor
addPeriod <- function(DF, DATE){
# add a period ID to a dataframe -- assuming rownames are date-strings
    DF$period <- NA
    DF[as.Date(rownames(DF)) < as.Date(DATE), c('period')] <- paste0('pre', as.POSIXlt(DATE)$year + 1900)
    DF[as.Date(rownames(DF)) >= as.Date(DATE), c('period')] <- paste0('post', as.POSIXlt(DATE)$year + 1900)
    DF$period <- factor(DF$period, levels = c(paste0('pre', as.POSIXlt(DATE)$year + 1900),
                                              paste0('post', as.POSIXlt(DATE)$year + 1900)))
    return(DF)
}

# a dataframe with FCI, adj cash, and market rates
two5df <- as.data.frame(cbind(-1*FCI_sfx, ref_x$adjCash, IRSx_m)['1994::'])
two5df <- addPeriod(two5df, "2004-01-01")

# a dataframe with FCI leading by 6m to match the 0dx2y spread lead-lag
FCI_mkt <- as.data.frame(merge(lead(-1*FCI_sfx, 6), IRSx_m)['1994::'])
FCI_mkt <- addPeriod(FCI_mkt, "2004-01-01")

# ggplot xy-plot: 2x5 & FCI
gp_2x5Vfci_XY <- ggplot(two5df[, c('fci', 'i2x5', 'period')],
       aes(x = fci, y = i2x5, color = period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "2x5 Curve & fci") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = 'fci' , y = '2x5 IRS (bps)') +
       geom_point(data = tail(comCaseRet(two5df[, c('fci', 'i2x5')]),1),
                  aes(x = fci, y = i2x5), color = 'purple',
                  pch=18, size = 4)

# ggplot xy-plot: 2x5 & adjCash
gp_2x5VadjCash_XY <- ggplot(two5df[, c('adjCash', 'i2x5', 'period')],
       aes(x = adjCash, y = i2x5, color = period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "2x5 Curve & adjCash") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = 'adjCash' , y = '2x5 IRS (bps)') +
       geom_point(data = tail(comCaseRet(two5df[, c('adjCash', 'i2x5')]),1),
                  aes(x = adjCash, y = i2x5), color = 'purple',
                  pch=18, size = 4)

# ggplot xy-plot: 5x10 & FCI
gp_5x10Vfci_XY <- ggplot(two5df[, c('fci', 'i5x10', 'period')],
       aes(x = fci, y = i5x10, color = period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "5x10 Curve & fci") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = 'fci' , y = '5x10 IRS (bps)') +
       geom_point(data = tail(comCaseRet(two5df[, c('fci', 'i5x10')]),1),
                  aes(x = fci, y = i5x10), color = 'purple',
                  pch=18, size = 4)

# ggplot xy-plot: 5x10 & adjCash
gp_5x10VadjCash_XY <- ggplot(two5df[, c('adjCash', 'i5x10', 'period')],
       aes(x = adjCash, y = i5x10, color = period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "5x10 Curve & adjCash") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = 'adjCash' , y = '5x10 IRS (bps)') +
       geom_point(data = tail(comCaseRet(two5df[, c('adjCash', 'i5x10')]),1),
                  aes(x = adjCash, y = i5x10), color = 'purple',
                  pch=18, size = 4)

# ggplot xy-plot: split this into the two periods
gp_fci2yrcash_XY <- ggplot(FCI_mkt[, c('fci', 'cash2y', 'period')],
       aes(x = fci, y = cash2y, color=period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "2yr v Cash v FCI (adv 6m)") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = 'FCI', y = '2yr yield v RBA cash (bps)') +
       geom_vline(aes(intercept = tail(FCI_mkt$fci, 1)),
                      linetype = 5, col = 'black', lwd = 0.7) +
       geom_point(data = tail(comCaseRet(FCI_mkt[, c('fci', 'cash2y')]),1),
                  aes(x = fci, y = cash2y), color = 'purple',
                  pch=18, size = 4)

# ggplot xy-plot: 2x5 & 2yr-cash
gp_2x5V2ycash_XY <- ggplot(FCI_mkt[, c('cash2y', 'i2x5', 'period')],
       aes(x = cash2y, y = i2x5, color=period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "2x5 Curve & 2yr v. cash") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = '2yr v cash (bps)', y = '') +
       geom_point(data = tail(comCaseRet(FCI_mkt[, c('cash2y', 'i2x5')]),1),
                  aes(x = cash2y, y = i2x5), color = 'purple',
                  pch=18, size = 4)

# ggplot xy-plot: 2x5 & 2yr yield
gp_2x5V2y_XY <- ggplot(FCI_mkt[, c('ADSWAP2Q', 'i2x5', 'period')],
       aes(x = ADSWAP2Q, y = i2x5, color=period)) +
       geom_point(shape=1) +
       geom_smooth(method=lm) +
       theme(legend.position = 'right') +
       labs(title = "2x5 Curve & 2yr") +
       scale_color_brewer(palette = "Set1") +
       theme_grey(16) +
       labs(x = '2yr' , y = '2x5 IRS (bps)') +
       geom_point(data = tail(comCaseRet(FCI_mkt[, c('ADSWAP2Q', 'i2x5')]),1),
                  aes(x = ADSWAP2Q, y = i2x5), color = 'purple',
                  pch=18, size = 4)



# line plots that show FCI and adjCash v. 2x5
lp_2x5VadjCash94p <- TwoAxesLat(merge(-ref_x$adjCash, IRSx_m)['1994::', c('adjCash', 'i2x5')],
                             leg.string = c('adj. RBA cash (inv)', 'AUD 2x5 IRS'))
lp_2x5VadjCash9404 <- TwoAxesLat(merge(-ref_x$adjCash, IRSx_m)['1997::2007', c('adjCash', 'i2x5')],
                             leg.string = c('adj. RBA cash (inv)', 'AUD 2x5 IRS'))
lp_2x5VadjCash04p <- TwoAxesLat(merge(-ref_x$adjCash, IRSx_m)['2008::', c('adjCash', 'i2x5')],
                             leg.string = c('adj. RBA cash (inv)', 'AUD 2x5 IRS'))
#
lp_2x5Vfci94p <- TwoAxesLat(merge(FCI_sfx, IRSx_m)['1994::', c('fci', 'i2x5')],
                             leg.string = c('FCI', 'AUD 2x5 IRS'))
lp_2x5Vfci9704 <- TwoAxesLat(merge(FCI_sfx, IRSx_m)['1997::2004', c('fci', 'i2x5')],
                             leg.string = c('FCI', 'AUD 2x5 IRS'))
lp_2x5Vfci04p <- TwoAxesLat(merge(FCI_sfx, IRSx_m)['2004::', c('fci', 'i2x5')],
                             leg.string = c('FCI', 'AUD 2x5 IRS'))

# line plots that show FCI and adjCash v. 5x10
lp_5x10VadjCash94p <- TwoAxesLat(merge(-ref_x$adjCash, IRSx_m)['1998::', c('adjCash', 'i5x10')],
                             leg.string = c('adj. RBA cash (inv)', 'AUD 5x10 IRS'))
lp_5x10VadjCash9404 <- TwoAxesLat(merge(-ref_x$adjCash, IRSx_m)['1997::2007', c('adjCash', 'i5x10')],
                             leg.string = c('adj. RBA cash (inv)', 'AUD 5x10 IRS'))
lp_5x10VadjCash04p <- TwoAxesLat(merge(-ref_x$adjCash, IRSx_m)['2008::', c('adjCash', 'i5x10')],
                             leg.string = c('adj. RBA cash (inv)', 'AUD 5x10 IRS'))
#
lp_5x10Vfci94p <- TwoAxesLat(merge(FCI_sfx, IRSx_m)['1994::', c('fci', 'i5x10')],
                             leg.string = c('FCI', 'AUD 5x10 IRS'))
lp_5x10Vfci9704 <- TwoAxesLat(merge(FCI_sfx, IRSx_m)['1997::2004', c('fci', 'i5x10')],
                             leg.string = c('FCI', 'AUD 5x10 IRS'))
lp_5x10Vfci04p <- TwoAxesLat(merge(FCI_sfx, IRSx_m)['2004::', c('fci', 'i5x10')],
                             leg.string = c('FCI', 'AUD 5x10 IRS'))

# line plots to show the lead / lag between rates and the FCI

lp_cash2yVfci_line94p <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['1994::', c('fci', 'cash2y')],
                                    leg.string = c('fci (adv 6m)', 'cash2yIRS'))
lp_cash2yVfci_line9403 <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['1994::2003', c('fci', 'cash2y')],
                                    leg.string = c('fci (adv 6m)', 'cash2yIRS'))
lp_cash2yVfci_line04p <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['2004::', c('fci', 'cash2y')],
                                    leg.string = c('fci (adv 6m)', 'cash2yIRS'))
#
lp_cash3yVfci_line94p <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['1994::', c('fci', 'cash3y')],
                                    leg.string = c('fci (adv 6m)', 'cash3yIRS'))
lp_cash3yVfci_line9403 <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['1994::2003', c('fci', 'cash3y')],
                                     leg.string = c('fci (adv 6m)', 'cash3yIRS'))
lp_cash3yVfci_line04p <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['2004::', c('fci', 'cash3y')],
                                    leg.string = c('fci (adv 6m)', 'cash3yIRS'))
# cash v 2yr govt
lp_cash2gVfci_line94p <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['1994::', c('fci', 'cash2g')],
                                    leg.string = c('fci (adv 6m)', 'cash2Govt'))
lp_cash2gVfci_line9403 <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['1994::2003', c('fci', 'cash2g')],
                                     leg.string = c('fci (adv 6m)', 'cash2Govt'))
lp_cash2gVfci_line04p <- TwoAxesLat(merge(lead(FCI_sfx, 6), IRSx_m)['2004::', c('fci', 'cash2g')],
                                    leg.string = c('fci (adv 6m)', 'cash2Govt'))
# cash v 3yr govt
lp_cash3gVfci_line94p <- TwoAxesLat(merge(lead(FCI_sfx, 3), IRSx_m)['1994::', c('fci', 'cash3g')],
                                    leg.string = c('fci (adv 3m)', 'cash3Govt'))
lp_cash3gVfci_line9403 <- TwoAxesLat(merge(lead(FCI_sfx, 3), IRSx_m)['1994::2003', c('fci', 'cash3g')],
                                     leg.string = c('fci (adv 3m)', 'cash3Govt'))
lp_cash3gVfci_line04p <- TwoAxesLat(merge(lead(FCI_sfx, 3), IRSx_m)['2004::', c('fci', 'cash3g')],
                                    leg.string = c('fci (adv 3m)', 'cash3Govt'))
if(fromSource()) {
    pdf(file.path(plotPATH, "fciV2x5.pdf"))
    print(gp_2x5Vfci_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "2x5VadjCash.pdf"))
    print(gp_2x5VadjCash_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "fciV5x10.pdf"))
    print(gp_5x10Vfci_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "5x10VadjCash.pdf"))
    print(gp_5x10VadjCash_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "fciVcash2yXY.pdf"))
    print(gp_fci2yrcash_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "2x5Vcash2yXY.pdf"))
    print(gp_2x5V2ycash_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "2x5V2yXY.pdf"))
    print(gp_2x5V2y_XY)
    dev.off()
    #
    pdf(file.path(plotPATH, "lp_2x5VadjCash94p.pdf"))
    print(lp_2x5VadjCash94p)
    dev.off()
    #
    pdf(file.path(plotPATH, "lp_2x5Vfci94p.pdf"))
    print(lp_2x5Vfci94p)
    dev.off()
    #
    pdf(file.path(plotPATH, "lp_5x10VadjCash94p.pdf"))
    print(lp_5x10VadjCash94p)
    dev.off()
    #
    pdf(file.path(plotPATH, "lp_5x10Vfci94p.pdf"))
    print(lp_5x10Vfci94p)
    dev.off()
}

# }}} close s/e curve ~ FCI
