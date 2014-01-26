# the new PCA --> VAR --> Cash model for NZ

# A :: Matthew Coogan Johnson
# S :: Tuesday 30 April 2013

# TODO :: better plots -- equiv to RBA model

# {{{ setup stuff
cleanUp()
Sys.setenv(TZ='GMT')
options(warnings = 0)

# options for analysis
HAVER <- FALSE
# end setup }}}
# {{{ functions and packages 
require(xts)
require(timsac)
require(RODBC)
require(missMDA)
require(vars)
require(Rbbg)
require(stringr)
# require(Haver)

# get helper functions
source("S:/Rates Research/autotools/Rhelpers/ecoPCAhelpers.r")

## }}}
## {{{ path stuff
# dlxSetDbPath("//nhkg6030pap/haver/dlx/data")
projectPATH <- "S:/Rates Research/eco/nzd/rbnz/policyVAR"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- file.path(projectPATH, 'data')
codePATH <- file.path(projectPATH, 'code')
# }}}
# {{{ get the meta data for fetching data
conXL <- odbcConnectExcel(xls.file="S:/Rates Research/eco/nzd/rbnz/policyVAR/data/zPCA_FCI.xls")
s1 <- 'hMap'
dH <- sqlFetch(conXL, s1, as.is=TRUE)
s2 <- 'bmMap'
dBm <- sqlFetch(conXL, s2, as.is=TRUE)
s3 <- 'bdMap'
dBd <- sqlFetch(conXL, s3, as.is=TRUE)
s4 <- 'bqMap'
dBq <- sqlFetch(conXL, s4, as.is=TRUE)
s5 <- 'db'
dbMeta <- sqlFetch(conXL, s5, as.is=TRUE)
if(!HAVER) {
    s6 <- 'HtoR_f'
    datH <- xtsF(sqlFetch(conXL, s6, as.is=TRUE))
}
odbcClose(conXL)
# combine the maps -- make sure they all have the same column names
dDBmap <- dbMeta[!dbMeta$name %in% c('period', 'date'), -c(1:2)] # cut off the nasties
dd_map <- rbind(dH, dBm, dBd, dBq, dDBmap) # and combine!
# }}}
# {{{ bbg data
start.date <- as.Date("1980-01-01")
# end.date <- Sys.Date() + 1
fields <- c("PX_LAST")
#
# connect to bbg and get data into a consistent format
conn <- blpConnect(verbose=FALSE, blpapi.jar.file = helpEnv$Bjar)
qtrlyBBG <- bdh(conn, dBq$ticker, fields, start.date, include.non.trading.days=FALSE) 
monthlyBBG <- bdh(conn, dBm$ticker, fields, start.date, include.non.trading.days=FALSE) 
dailyBBG <- bdh(conn, dBd$ticker, fields, start.date, include.non.trading.days = FALSE)
invisible(blpDisconnect(conn)) # now close the connection

# unstack bbg data
datBq <- bbgUnstacker(qtrlyBBG) #DATAIN
datBm <- bbgUnstacker(monthlyBBG) #DATAIN
datBd <- bbgUnstacker(dailyBBG) #DATAIN

# convert daily data to mon and qtr averages
datBd_qAve <- apply.quarterly(datBd, colMeans, na.rm=TRUE)
datBd_mAve <- apply.monthly(datBd, colMeans, na.rm=TRUE)
datBd_mAve$NZOCRS <- apply.monthly(na.locf(datBd$NZOCRS), last) # use last OCR value
datBd_qAve$NZOCRS <- apply.quarterly(na.locf(datBd$NZOCRS), last) # use last OCR value
index(datBd_qAve) <- toQtrMons(index(datBd_qAve))
index(datBd_mAve) <- toLastDay(index(datBd_mAve))
#
# convert mon to qtr and qtr to mon
datBm_qAve <- apply.quarterly(datBm, colMeans, na.rm=TRUE)
index(datBm_qAve) <- toQtrMons(index(datBm_qAve))
datBq_2m <- qtr2Mon(datBq)
index(datBq_2m) <- toLastDay(index(datBq_2m))
#
# stick the desired names on
names(datBd_qAve) <- dBd$name
names(datBd_mAve) <- dBd$name
names(datBm) <- dBm$name
names(datBm_qAve) <- dBm$name
names(datBq) <- dBq$name
names(datBq_2m) <- dBq$name

# turn NaN into NA
is.na(datBd_qAve) <- do.call(cbind, lapply(datBd_qAve, is.nan))
is.na(datBd_mAve) <- do.call(cbind, lapply(datBd_mAve, is.nan))
is.na(datBm_qAve) <- do.call(cbind, lapply(datBm_qAve, is.nan))
#
# now SA the data and make stationary
datBm <- SAandStationary(datBm, dBm, PRD=12)
datBm_qAve <- SAandStationary(datBm_qAve, dBm, PRD=4)
datBd_mAve <- SAandStationary(datBd_mAve, dBd, PRD=12)
datBd_qAve <- SAandStationary(datBd_qAve, dBd, PRD=4)
datBq_2m <- SAandStationary(datBq_2m, dBq, PRD=12)
datBq <- SAandStationary(datBq, dBq, PRD=4)

# }}}
# {{{ Haver data
if(HAVER) 
{
    datH <- dlxGetData(dH$ticker, "anz")
    names(datH) <- dH$name
}
# fix zbo data (which does not include a Jan observation) and other gaps
zbo_cols <- which(substr(names(datH), 1, 3) == 'zbo')
gapCols <- c(146, zbo_cols)
datH[, gapCols] <- na.locf(as.xts(datH[, c(gapCols)]))
#
# eliminate a zero
datH[which(datH$cnst_resiApt_Num == 0), 'cnst_resiApt_Num'] <- 1
# make Qtrly, SA and Stationary + shift dates
datH_sa <- SAandStationary(datH, dH, PRD=12)
datHq_sa <- apply.quarterly(datH_sa, colMeans, na.rm=TRUE)
index(datHq_sa) <- toQtrMons(index(datHq_sa))
index(datH_sa) <- toLastDay(index(datH_sa))
# }}} close HAVER
# {{{ Database data
# clean out NA excel spandrels
dbMeta <- dbMeta[!is.na(dbMeta$path),]
#
#make a list of the database file connections
dblist <- unique(dbMeta[,1])
tbllist <- unique(dbMeta[,1:2])
#
# get data and make into list(xts) and split into env
datDb <- fetchUDB(dblist, tbllist, dbMeta) #DATAIN
datDb_x <- lapply(datDb, xtsF)
list2env(datDb_x, globalenv())

# stick names on from meta
names(RBNZ_weekly) <- dbMeta$name[match(names(RBNZ_weekly), dbMeta$ticker)]
names(m14_exp) <- dbMeta$name[match(names(m14_exp), dbMeta$ticker)]
names(m1_inflation) <- dbMeta$name[match(names(m1_inflation), dbMeta$ticker)]
m1_inflation['::19911130', 2:3] <- NA
m1_inflation['::19931130',3] <- NA
#
mtgWeekT <- RBNZ_weekly
mtgWeekT[,1] <- baysea(RBNZ_weekly[, 1], period = 52, span = 13, 
                       shift=1, trend.order=1, year = 2003, plot = FALSE)$trend
mtgWeekT[,2] <- baysea(RBNZ_weekly[, 2], period = 52, span = 13, 
                       shift=1, trend.order=1, year = 2003, plot = FALSE)$trend
mtgTrd_aveQtr <- apply.quarterly(mtgWeekT, colMeans, na.rm=TRUE)
index(mtgTrd_aveQtr) <- toQtrMons(index(mtgTrd_aveQtr))
#
dbFrame_q <- merge(mtgTrd_aveQtr, m14_exp, m1_inflation)
dbFrame_m <- qtr2Mon(dbFrame_q)
index(dbFrame_m) <- toLastDay(index(dbFrame_m))
# }}}
# data object {{{
basicDataList <- list(
                 datBq, 
                 datBm, 
                 datBd,
                 datH, 
                 datDb)
save(basicDataList, file = file.path(dataPATH, "masterData.RData"))

# }}}
# {{{ create the study frames
# starting period
startDate <- as.Date("1991-08-31")
endDate <- toQtrMons(Sys.Date() + 1)# change this to tweak the last obs
# endDate <- toLastDay(Sys.Date()) # change this to tweak the last obs
windowStamp <- paste0(startDate, "::", endDate)
#
# Qtrly data-maker
# list that maps data sources to studies via map$group
datList_q <- list(DFs = c('datBm_qAve', 'datBd_qAve', 'datBq', 'dbFrame_q', 'datHq_sa'), 
                  DFmaps = c('dBm', 'dBd', 'dBq', 'dDBmap', 'dH'))
#
# function makes the _dd
REFdd_q <- xtsMerger(datList_q, IN = c('ref'))[windowStamp]
REFdd_q$cpiFMp <- rbind(REFdd_q$zCPI_y['19880331::19910930'],
                        REFdd_q$cpiFM_y['19911231::19930930'], 
                        REFdd_q$cpiSFM_y['19931231::']
                        )[windowStamp]

cpiFMp_q <- na.ARextend(REFdd_q$cpiFMp)

PCAdd_q <- xtsMerger(datList_q, IN = c('PCA', 'both'))[windowStamp]
FCIdd_q <- xtsMerger(datList_q, IN = c('FCI', 'both'))[windowStamp]
FCIdd_q$move90d_1y <- FCIdd_q$RBNZs_1y90d - FCIdd_q$RBNZs_90d # added RBNZ move expectations

# monthly data-maker
datList_m <- list(DFs = c('datBm', 'datBd_mAve', 'datBq_2m', 'dbFrame_m', 'datH_sa'), 
                  DFmaps = c('dBm', 'dBd', 'dBq', 'dDBmap', 'dH'))
#
# function makes the _dd
REFdd_m <- xtsMerger(datList_m, IN = c('ref'))[paste(startDate, "::")]
cpiFMp_m <- qtr2Mon(cpiFMp_q)[paste(startDate, "::")] # use the qtrly DF -> mom
PCAdd_m <- xtsMerger(datList_m, IN = c('PCA', 'both'))[windowStamp]
FCIdd_m <- xtsMerger(datList_m, IN = c('FCI', 'both'))[windowStamp]
FCIdd_m$move90d_1y <- FCIdd_m$RBNZs_1y90d - FCIdd_m$RBNZs_90d # added RBNZ move expectations

# extend UR to cover missing values :: later use an AR method
PCAdd_q$ur <- na.ARextend(PCAdd_q$ur)
PCAdd_m$ur <- na.ARextend(PCAdd_m$ur)

# extend NZ OCR using the 1m FIX data
REFdd_m$rbnz_plus <- REFdd_m$rbnz_ocr
REFdd_m$rbnz_plus['::19990228'] <- REFdd_m$z1m['::19990228'] - 0.12

# }}}
## {{{ ===> MJ BIG PCA

#################
## MJs BIG PCA ##
#################
calcWindow <- paste0(startDate, "::", toLastDay(Sys.Date(), monAdv = -1))
# PCA1 using monthly data + smoothed Qtrly data
PCAdd_m <- mapXtsL(PCAdd_m, na.ARextend)
im <- imputePCA(coredata(PCAdd_m[calcWindow]), ncp=3)
PCAddm_filled <- xts(im$completeObs, order.by=index(PCAdd_m[calcWindow]))
# stack and trim (note you lose 2 df in the stacking)
PCAdd_3stck <- cbind(PCAddm_filled, lag(PCAddm_filled, 1), lag(PCAddm_filled, 2))[-c(1:2)]
# and then find the first factor (from 3m stack) and then scale
sf_x <- scale(xts(PCA(PCAdd_3stck, graph=FALSE)$ind$coord, 
                  order.by = index(PCAdd_3stck)))[,1]
#
## the qtrly data version -- agg up higher freq data
iq <- imputePCA(coredata(PCAdd_q), ncp=3)
PCAddq_filled <- xts(iq$completeObs, order.by = index(PCAdd_q))
ddq_x <- scale( xts(PCA(PCAddq_filled, graph=FALSE)$ind$coord, 
                    order.by = index(PCAddq_filled)))[,1]
#
## monthly data -- unstacked
ddm1_x <- scale( xts(PCA(PCAddm_filled, graph=FALSE)$ind$coord, 
                     order.by = index(PCAddm_filled)))[,1]

# {{{ SCORES
# in this section we construct the scores index
obsvdCols <- which(!is.na(tail(PCAdd_m, 1)))
#
# make the scores using a function
dataScore <- scale(makeScore(PCAddm_filled, MAP = dd_map))
dataScore_subset <- scale(makeScore(PCAddm_filled, SUB = obsvdCols, MAP = dd_map))
# }}}

# }}} close PCA
#####  {{{ ===> FCI <=== #### 

# 1st fill in the blanks
FCIdd_m <- mapXtsL(FCIdd_m, na.ARextend)
imF <- imputePCA(coredata(FCIdd_m[calcWindow]), ncp=3)
fciCmpl <- xts(imF$completeObs, order.by=index(FCIdd_m[calcWindow]))
#
# stack and trim (note you lose 2 df by stacking)
fc_3stck <- cbind(fciCmpl, lag(fciCmpl), lag(fciCmpl, 2))[-c(1:2), ]
#
# first purge the data of f1 and inflation
purgedFCI <- matrix(rep(0, ncol(fc_3stck)*nrow(fc_3stck)), nrow(fc_3stck), ncol(fc_3stck))
#

CBpolicy_3mma <- rollmean(na.locf(REFdd_m$rbnz_plus), 3, 
                          align = 'right', 
                          fill = list(slideFill(REFdd_m$rbnz_plus, 2, `mean`)))
CBpolicy_6mma <- rollmean(na.locf(REFdd_m$rbnz_plus), 6, 
                          align = 'right', 
                          fill = list(slideFill(REFdd_m$rbnz_plus, 5, `mean`)))
cpi_3mma <- rollmean(na.locf(cpiFMp_m), 3, 
                     align = 'right', 
                     fill = list(slideFill(cpiFMp_m, 2, `mean`))
                     )[index(CBpolicy_3mma)]
cpi_6mma <- rollmean(na.locf(cpiFMp_m), 6, 
                     align = 'right', 
                     fill = list(slideFill(cpiFMp_m, 5, `mean`))
                     )[index(CBpolicy_3mma)]
dd_3mma <- rollmean(sf_x[,1], 3, 
                    align = 'right',
                    fill = list(slideFill(sf_x[,1], 2, `mean`)))
dd_6mma <- rollmean(sf_x[,1], 6, 
                    align = 'right',
                    fill = list(slideFill(sf_x[,1], 5, `mean`)))

for (i in 1:ncol(fc_3stck)){
    purgedFCI[,i] <- lm(fc_3stck[,i] ~ dd_3mma + 
                        CBpolicy_6mma[index(dd_6mma), 'rbnz_plus'] + 
                        cpi_6mma[index(dd_6mma)])$residuals
}

purgedFCI_x <- xts(as.data.frame(purgedFCI), order.by=index(fc_3stck))
names(purgedFCI_x) <- names(fc_3stck)
#
# now make the FCI
FCImod <- PCA(purgedFCI, graph=F) # orth FCI
FCI_W <- FCImod$var$coord # the eVector Matrix
rownames(FCI_W) <- names(fc_3stck)
FCImod_no <- PCA(fc_3stck, graph=FALSE)
#
fci_sox <- scale(xts(FCImod$ind$coord, order.by = index(fc_3stck))) # the first five factors
fci_sox <- -1 * sign(FCI_W['c_housing',1]) * fci_sox
fci_snx <- scale(xts(FCImod_no$ind$coord, order.by=index(fc_3stck)))
fci_snx <- -1 * sign(FCImod_no$var$coord['c_housing',1]) * fci_snx
#
plot(fci_sox[,1]); lines(fci_snx[,1], col=2)

# make qtrly using apply.quarterly
fci_sox_Q <-  apply.quarterly(fci_sox[,1], mean)
fci_snx_Q <-  apply.quarterly(fci_snx[,1], mean)
index(fci_sox_Q) <- toQtrMons(index(fci_sox_Q))
index(fci_snx_Q) <- toQtrMons(index(fci_snx_Q))

## the no-lag version
fci_1stack <- fciCmpl
purge1 <- matrix(rep(0, ncol(fciCmpl) * nrow(fciCmpl)), nrow(fciCmpl), ncol(fciCmpl))

for (i in 1:ncol(fciCmpl)) {
    purge1[, i] <- lm(fciCmpl[,i] ~ ddm1_x + 
                      REFdd_m[index(ddm1_x), 'z3m'] + 
                      cpiFMp_m[index(ddm1_x)])$residuals 
}

purge1x <- xts(as.data.frame(purge1), order.by = index(fci_1stack))
names(purge1) <- names(fciCmpl)
# 
fci1mod <- scale(xts(PCA(purge1, graph=FALSE)$ind$coord, order.by = index(fci_1stack)))
fci1_nonOrth <- scale(xts((PCA(fciCmpl, graph=FALSE)$ind$coord), order.by=index(fci_1stack)))

##### ===> FCI_sfx is your Watson-ized FCI <=== ##### }}}
# {{{ VAR models

# {{{ prep
# the full VAR -- house prices, FCI, dd, UR, CPI and 3m rate

# extend reer (forward and back) using TWI
REFdd_m$reer_bisBroad <- fillXZ(REFdd_m$reer_bisBroad, 
                                REFdd_m$zTWI, 
                                dateRange = "1991::1993", 
                                DOWN=FALSE, 
                                LOG = FALSE)
REFdd_m$reer_bisBroad <- fillXZ(REFdd_m$reer_bisBroad, 
                                REFdd_m$zTWI, 
                                dateRange = "2013::", 
                                LOG=FALSE)
# set the estimation period
startEst <- "19990131"
endEst <- "20130930"
estRange <- paste0(startEst, "::", endEst)
icT = 'FPE' 

# }}} end prep

####################
# RESTRICTED VARS ##
####################

# TODO: add unrestricted VARs, add some small VARs with demand, CPI, cash etc combinations of variables
# TODO: add a simpler VAR: house prices, TOT, REER, 3m, CPI, UR ...

# {{{ monthly
## start ==> full model is xpx ~ reer & fci~ff~ur~cpi~z3m ##
VAR7frameR <- cbind(na.locf(PCAdd_m$anzCPIdx_sdr), REFdd_m$reer_bisBroad, 
                    fci_snx[,1], sf_x, PCAddm_filled$ur, cpiFMp_m[index(sf_x)], 
                    REFdd_m$z3m
                    )[calcWindow]['19940101::']
index(VAR7frameR) <- toLastDay(index(VAR7frameR), toFirst=TRUE)
names(VAR7frameR) <- c('xpx', 'reer', 'fci', 'dd', 'ur', 'cpi', 'z3m')
v7Mod <- VAR(VAR7frameR, p = 2, ic = icT)
optLag <- findMaxVARLag(VAR7frameR, firstMax=12, crit = paste0(icT, "(n)")) # 5
var7_RSTM <- matrix(rep(1, 105), nrow = 7, ncol = 15)
rownames(var7_RSTM) <- c('xpx', 'reer', 'fci', 'dd', 'ur', 'cpi', 'z3m')
colnames(var7_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'dd.l1', 'ur.l1', 'cpi.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'dd.l2', 'ur.l2', 'cpi.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var7_RSTM[1, c(2:7,
               9:14
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var7_RSTM[2, c(3:7,
               10:14
               )] <- 0
# restrict cpi: cpi ~ ur + reer + L(cpi)
var7_RSTM[6, c(1, 3, 4, 7,
               8, 10, 11, 14
               )] <- 0

# restricted NZD model
tt7_mod <- restrict(v7Mod, method = 'manual', resmat = var7_RSTM)
tt7 <- testVar(VAR7frameR, skip = 84, nAhead = 6, IC = icT, RSTmtx = var7_RSTM)
tt7_12m <- testVar(VAR7frameR, skip = 84, nAhead = 12, IC = icT, RSTmtx = var7_RSTM)
tt7_12m_30 <- predict(tt7_mod, n.ahead = 12, ci = 0.3)
tt7_12m_60 <- predict(tt7_mod, n.ahead = 12, ci = 0.6)
tt7_12m_90 <- predict(tt7_mod, n.ahead = 12, ci = 0.9)

## start six part RBNZ model :: xpx ~ reer & fci~dd~cpi~z3m ##
VAR6frameR <- cbind(na.locf(PCAdd_m$anzCPIdx_sdr), REFdd_m$reer_bisBroad, 
                    fci_snx[,1], sf_x, cpiFMp_m[index(sf_x)], 
                    REFdd_m$z3m
                    )[calcWindow]['19940101::']
index(VAR6frameR) <- toLastDay(index(VAR6frameR), toFirst=TRUE)
names(VAR6frameR) <- c('xpx', 'reer', 'fci', 'dd', 'cpi', 'z3m')
v6Mod <- VAR(VAR6frameR, p = 2, ic = icT)
optLag <- findMaxVARLag(VAR6frameR, firstMax=12, crit = paste0(icT, "(n)")) # 5
var6_RSTM <- matrix(rep(1, 78), nrow = 6, ncol = 13)
rownames(var6_RSTM) <- c('xpx', 'reer', 'fci', 'dd', 'cpi', 'z3m')
colnames(var6_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'dd.l1', 'cpi.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'dd.l2', 'cpi.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var6_RSTM[1, c(2:6,
               8:12
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var6_RSTM[2, c(3:6,
               9:12
               )] <- 0

# restricted NZD model
tt6_mod <- restrict(v6Mod, method = 'manual', resmat = var6_RSTM)
tt6 <- testVar(VAR6frameR, skip = 84, nAhead = 6, IC = icT, RSTmtx = var6_RSTM)
tt6_12m <- testVar(VAR6frameR, skip = 84, nAhead = 12, IC = icT, RSTmtx = var6_RSTM)
tt6_12m_30 <- predict(tt6_mod, n.ahead = 12, ci = 0.3)
tt6_12m_60 <- predict(tt6_mod, n.ahead = 12, ci = 0.6)
tt6_12m_90 <- predict(tt6_mod, n.ahead = 12, ci = 0.9)

## start z3m 5 part model :: xpx ~ reer & fci~cpi~z3m ##
VAR5frameR <- cbind(na.locf(PCAdd_m$anzCPIdx_sdr), REFdd_m$reer_bisBroad, 
                    fci_snx[,1], cpiFMp_m[index(REFdd_m)], 
                    REFdd_m$z3m
                    )[calcWindow]['19940101::']
index(VAR5frameR) <- toLastDay(index(VAR5frameR), toFirst=TRUE)
names(VAR5frameR) <- c('xpx', 'reer', 'fci', 'cpi', 'z3m')
v5Mod <- VAR(VAR5frameR, p = 2, ic = icT)
optLag <- findMaxVARLag(VAR5frameR, firstMax=12, crit = paste0(icT, "(n)")) # 5
var5_RSTM <- matrix(rep(1, 55), nrow = 5, ncol = 11)
rownames(var5_RSTM) <- c('xpx', 'reer', 'fci', 'cpi', 'z3m')
colnames(var5_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'cpi.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'cpi.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var5_RSTM[1, c(2:5,
               7:10
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var5_RSTM[2, c(3:5,
               8:10
               )] <- 0
#
# restricted NZD model
tt5_mod <- restrict(v5Mod, method = 'manual', resmat = var5_RSTM)
tt5 <- testVar(VAR5frameR, skip = 84, nAhead = 6, IC = icT, RSTmtx = var5_RSTM)
tt5_12m <- testVar(VAR5frameR, skip = 84, nAhead = 12, IC = icT, RSTmtx = var5_RSTM)
tt5_12m_30 <- predict(tt5_mod, n.ahead = 12, ci = 0.3)
tt5_12m_60 <- predict(tt5_mod, n.ahead = 12, ci = 0.6)
tt5_12m_90 <- predict(tt5_mod, n.ahead = 12, ci = 0.9)

## start ## xpx~reer & fci-dd-z3m (note no CPI)
VAR5dframeR <- cbind(na.locf(PCAdd_m$anzCPIdx_sdr), REFdd_m$reer_bisBroad,
                     fci_snx[,1], sf_x, 
                     REFdd_m$z3m
                     )[calcWindow]['19940101::']
index(VAR5dframeR) <- toLastDay(index(VAR5dframeR), toFirst=TRUE)
names(VAR5dframeR) <- c('xpx', 'reer', 'fci', 'dd', 'z3m')
v5dMod <- VAR(VAR5dframeR, p = 2, ic = icT)
optLag <- findMaxVARLag(VAR5dframeR, firstMax=12, crit = paste0(icT, "(n)")) # 5
var5d_RSTM <- matrix(rep(1, 55), nrow = 5, ncol = 11)
rownames(var5d_RSTM) <- c('xpx', 'reer', 'fci', 'dd', 'z3m')
colnames(var5d_RSTM) <- c(
                          'xpx.l1', 'reer.l1', 'fci.l1', 'dd.l1', 'z3m.l1',
                          'xpx.l2', 'reer.l2', 'fci.l2', 'dd.l2', 'z3m.l2',
                          'const'
                          )
#
# restrict xpx to be exogenous
var5d_RSTM[1, c(2:5,
                7:10
                )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var5d_RSTM[2, c(3:5,
                8:10
                )] <- 0
#
# restricted 5d NZD model
tt5d_mod <- restrict(v5dMod, method = 'manual', resmat = var5d_RSTM)
tt5d <- testVar(VAR5dframeR, skip = 84, nAhead = 6, IC = icT, RSTmtx = var5d_RSTM)
tt5d_12m <- testVar(VAR5dframeR, skip = 84, nAhead = 12, IC = icT, RSTmtx = var5d_RSTM)
tt5d_12m_30 <- predict(tt5d_mod, n.ahead = 12, ci = 0.3)
tt5d_12m_60 <- predict(tt5d_mod, n.ahead = 12, ci = 0.6)
tt5d_12m_90 <- predict(tt5d_mod, n.ahead = 12, ci = 0.9)

## start 4 part VAR :: xpx~reer & fci~z3m ##
VAR4frameR <- cbind(na.locf(PCAdd_m$anzCPIdx_sdr), REFdd_m$reer_bisBroad, 
                    fci_snx[,1], 
                    REFdd_m$z3m
                    )[calcWindow]['19940101::']
index(VAR4frameR) <- toLastDay(index(VAR4frameR), toFirst=TRUE)
names(VAR4frameR) <- c('xpx', 'reer', 'fci', 'z3m')
v4Mod <- VAR(VAR4frameR, p = 2, ic = icT)
optLag <- findMaxVARLag(VAR4frameR, firstMax=12, crit = paste0(icT, "(n)")) # 5
var4_RSTM <- matrix(rep(1, 36), nrow = 4, ncol = 9)
rownames(var4_RSTM) <- c('xpx', 'reer', 'fci', 'z3m')
colnames(var4_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var4_RSTM[1, c(2:4,
               6:8
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var4_RSTM[2, c(3:4,
               7:8
               )] <- 0
#
# restricted NZD model
tt4_mod <- restrict(v4Mod, method = 'manual', resmat = var4_RSTM)
tt4 <- testVar(VAR4frameR, skip = 84, nAhead = 6, IC = icT, RSTmtx = var4_RSTM)
tt4_12m <- testVar(VAR4frameR, skip = 84, nAhead = 12, IC = icT, RSTmtx = var4_RSTM)
tt4_12m_30 <- predict(tt4_mod, n.ahead = 12, ci = 0.3)
tt4_12m_60 <- predict(tt4_mod, n.ahead = 12, ci = 0.6)
tt4_12m_90 <- predict(tt4_mod, n.ahead = 12, ci = 0.9)
######## END Restricted VARS ########

# }}} end monthly
# {{{ Qtrly build
## start ==> full model is xpx ~ reer & fci~ff~ur~cpi~z3m ##
VAR7frameR_q <- cbind(na.locf(PCAdd_q$anzCPIdx_sdr), REFdd_q$reer_bisBroad, 
                      fci_snx_Q[,1], ddq_x, PCAddq_filled$ur, cpiFMp_q[index(ddq_x)], 
                      REFdd_q$z3m
                      )[calcWindow]['19940101::']
index(VAR7frameR_q) <- toLastDay(index(VAR7frameR_q), toFirst=TRUE)
names(VAR7frameR_q) <- c('xpx', 'reer', 'fci', 'dd', 'ur', 'cpi', 'z3m')
# optLag <- findMaxVARLag(VAR7frameR_q, firstMax=4, crit = "AIC(n)") # 2
v7Mod_q <- VAR(VAR7frameR_q, p = 2, ic = "AIC")
var7_RSTM <- matrix(rep(1, 105), nrow = 7, ncol = 15)
rownames(var7_RSTM) <- c('xpx', 'reer', 'fci', 'dd', 'ur', 'cpi', 'z3m')
colnames(var7_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'dd.l1', 'ur.l1', 'cpi.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'dd.l2', 'ur.l2', 'cpi.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var7_RSTM[1, c(2:7,
               9:14
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var7_RSTM[2, c(3:7,
               10:14
               )] <- 0
# restrict cpi: cpi ~ ur + reer + L(cpi)
var7_RSTM[6, c(1, 3, 4, 7,
               8, 10, 11, 14
               )] <- 0

# restricted NZD model
tt7_mod_q <- restrict(v7Mod_q, method = 'manual', resmat = var7_RSTM)
tt7_q <- testVar(VAR7frameR_q, skip = 28, nAhead = 4, IC = icT, RSTmtx = var7_RSTM)
tt7_8q <- testVar(VAR7frameR_q, skip = 28, nAhead = 8, IC = icT, RSTmtx = var7_RSTM)
tt7_8q_30 <- predict(tt7_mod_q, n.ahead = 8, ci = 0.3)
tt7_8q_60 <- predict(tt7_mod_q, n.ahead = 8, ci = 0.6)
tt7_8q_90 <- predict(tt7_mod_q, n.ahead = 8, ci = 0.9)

## start six part RBNZ model :: xpx ~ reer & fci~dd~cpi~z3m ##
VAR6frameR_q <- cbind(na.locf(PCAdd_q$anzCPIdx_sdr), REFdd_q$reer_bisBroad, 
                    fci_snx_Q[,1], ddq_x, cpiFMp_q[index(sf_x)], 
                    REFdd_q$z3m
                    )[calcWindow]['19940101::']
index(VAR6frameR_q) <- toLastDay(index(VAR6frameR_q), toFirst=TRUE)
names(VAR6frameR_q) <- c('xpx', 'reer', 'fci', 'dd', 'cpi', 'z3m')
v6Mod_q <- VAR(VAR6frameR_q, p = 2, ic = icT)
var6_RSTM <- matrix(rep(1, 78), nrow = 6, ncol = 13)
rownames(var6_RSTM) <- c('xpx', 'reer', 'fci', 'dd', 'cpi', 'z3m')
colnames(var6_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'dd.l1', 'cpi.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'dd.l2', 'cpi.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var6_RSTM[1, c(2:6,
               8:12
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var6_RSTM[2, c(3:6,
               9:12
               )] <- 0

# restricted NZD model
tt6_mod_q <- restrict(v6Mod_q, method = 'manual', resmat = var6_RSTM)
tt6_q <- testVar(VAR6frameR_q, skip = 28, nAhead = 4, IC = icT, RSTmtx = var6_RSTM)
tt6_8q <- testVar(VAR6frameR_q, skip = 28, nAhead = 8, IC = icT, RSTmtx = var6_RSTM)
tt6_8q_30 <- predict(tt6_mod_q, n.ahead = 8, ci = 0.3)
tt6_8q_60 <- predict(tt6_mod_q, n.ahead = 8, ci = 0.6)
tt6_8q_90 <- predict(tt6_mod_q, n.ahead = 8, ci = 0.9)

## start z3m 5 part model :: xpx ~ reer & fci~cpi~z3m ##
VAR5frameR_q <- cbind(na.locf(PCAdd_q$anzCPIdx_sdr), REFdd_q$reer_bisBroad, 
                    fci_snx_Q[,1], cpiFMp_q[index(REFdd_q)], 
                    REFdd_q$z3m
                    )[calcWindow]['19940101::']
index(VAR5frameR_q) <- toLastDay(index(VAR5frameR_q), toFirst=TRUE)
names(VAR5frameR_q) <- c('xpx', 'reer', 'fci', 'cpi', 'z3m')
v5Mod_q <- VAR(VAR5frameR_q, p = 2, ic = icT)
var5_RSTM <- matrix(rep(1, 55), nrow = 5, ncol = 11)
rownames(var5_RSTM) <- c('xpx', 'reer', 'fci', 'cpi', 'z3m')
colnames(var5_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'cpi.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'cpi.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var5_RSTM[1, c(2:5,
               7:10
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var5_RSTM[2, c(3:5,
               8:10
               )] <- 0
#
# restricted NZD model
tt5_mod_q <- restrict(v5Mod_q, method = 'manual', resmat = var5_RSTM)
tt5_q <- testVar(VAR5frameR_q, skip = 28, nAhead = 4, IC = icT, RSTmtx = var5_RSTM)
tt5_8q <- testVar(VAR5frameR_q, skip = 28, nAhead = 8, IC = icT, RSTmtx = var5_RSTM)
tt5_8q_30 <- predict(tt5_mod_q, n.ahead = 8, ci = 0.3)
tt5_8q_60 <- predict(tt5_mod_q, n.ahead = 8, ci = 0.6)
tt5_8q_90 <- predict(tt5_mod_q, n.ahead = 8, ci = 0.9)

## start ## xpx~reer & fci-dd-z3m (note no CPI)
VAR5dframeR_q <- cbind(na.locf(PCAdd_q$anzCPIdx_sdr), REFdd_q$reer_bisBroad,
                     fci_snx_Q[,1], ddq_x, 
                     REFdd_q$z3m
                     )[calcWindow]['19940101::']
index(VAR5dframeR_q) <- toLastDay(index(VAR5dframeR_q), toFirst=TRUE)
names(VAR5dframeR_q) <- c('xpx', 'reer', 'fci', 'dd', 'z3m')
v5dMod_q <- VAR(VAR5dframeR_q, p = 2, ic = icT)
var5d_RSTM <- matrix(rep(1, 55), nrow = 5, ncol = 11)
rownames(var5d_RSTM) <- c('xpx', 'reer', 'fci', 'dd', 'z3m')
colnames(var5d_RSTM) <- c(
                          'xpx.l1', 'reer.l1', 'fci.l1', 'dd.l1', 'z3m.l1',
                          'xpx.l2', 'reer.l2', 'fci.l2', 'dd.l2', 'z3m.l2',
                          'const'
                          )
#
# restrict xpx to be exogenous
var5d_RSTM[1, c(2:5,
                7:10
                )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var5d_RSTM[2, c(3:5,
                8:10
                )] <- 0
#
# restricted 5d NZD model
tt5d_mod_q <- restrict(v5dMod_q, method = 'manual', resmat = var5d_RSTM)
tt5d_q <- testVar(VAR5dframeR_q, skip = 28, nAhead = 4, IC = icT, RSTmtx = var5d_RSTM)
tt5d_8q <- testVar(VAR5dframeR_q, skip = 28, nAhead = 8, IC = icT, RSTmtx = var5d_RSTM)
tt5d_8q_30 <- predict(tt5d_mod_q, n.ahead = 8, ci = 0.3)
tt5d_8q_60 <- predict(tt5d_mod_q, n.ahead = 8, ci = 0.6)
tt5d_8q_90 <- predict(tt5d_mod_q, n.ahead = 8, ci = 0.9)

## start 4 part VAR :: xpx~reer & fci~z3m ##
VAR4frameR_q <- cbind(na.locf(PCAdd_q$anzCPIdx_sdr), REFdd_q$reer_bisBroad, 
                    fci_snx_Q[,1], 
                    REFdd_q$z3m
                    )[calcWindow]['19940101::']
index(VAR4frameR_q) <- toLastDay(index(VAR4frameR_q), toFirst=TRUE)
names(VAR4frameR_q) <- c('xpx', 'reer', 'fci', 'z3m')
v4Mod_q <- VAR(VAR4frameR_q, p = 2, ic = icT)
var4_RSTM <- matrix(rep(1, 36), nrow = 4, ncol = 9)
rownames(var4_RSTM) <- c('xpx', 'reer', 'fci', 'z3m')
colnames(var4_RSTM) <- c(
                         'xpx.l1', 'reer.l1', 'fci.l1', 'z3m.l1',
                         'xpx.l2', 'reer.l2', 'fci.l2', 'z3m.l2',
                         'const'
                         )
#
# restrict xpx to be exogenous
var4_RSTM[1, c(2:4,
               6:8
               )] <- 0
# restrict reer: reer ~ xpx + L(reer)
var4_RSTM[2, c(3:4,
               7:8
               )] <- 0
#
# restricted NZD model
tt4_mod_q <- restrict(v4Mod_q, method = 'manual', resmat = var4_RSTM)
tt4_q <- testVar(VAR4frameR_q, skip = 28, nAhead = 4, IC = icT, RSTmtx = var4_RSTM)
tt4_8q <- testVar(VAR4frameR_q, skip = 28, nAhead = 8, IC = icT, RSTmtx = var4_RSTM)
tt4_8q_30 <- predict(tt4_mod_q, n.ahead = 8, ci = 0.3)
tt4_8q_60 <- predict(tt4_mod_q, n.ahead = 8, ci = 0.6)
tt4_8q_90 <- predict(tt4_mod_q, n.ahead = 8, ci = 0.9)
######## END Restricted VARS ########

# }}} end qtrly
# }}} Close VAR models
## {{{ combine forecasts

# monthly data combo
comboPOOS6 <- list('z3m' = tt7$z3m['1994::'],
                   'cpi' = tt7$cpi['1994::'])
#
comboPOOS6$z3m <- (tt7$z3m['1994::'] +
                   tt6$z3m['1994::'] + 
                   tt5$z3m['1994::'] + 
                   tt5d$z3m['1994::'] + 
                   tt4$z3m['1994::']) / 5
#
comboPOOS6$cpi <- (tt7$cpi['1994::'] +
                   tt6$cpi['1994::'] + 
                   tt5$cpi['1994::']) / 3
#

comboPOOS12 <- list('z3m' = tt7_12m$z3m['1994::'],
                    'cpi' = tt7_12m$cpi['1994::'])
#
comboPOOS12$z3m <- (tt7_12m$z3m['1994::'] +
                    tt6_12m$z3m['1994::'] + 
                    tt5_12m$z3m['1994::'] + 
                    tt5d_12m$z3m['1994::'] + 
                    tt4_12m$z3m['1994::']) / 5
#
comboPOOS12$cpi <- (tt7_12m$cpi['1994::'] +
                    tt6_12m$cpi['1994::'] + 
                    tt5_12m$cpi['1994::']) / 3

sumTestError.comboPOOS6 <- errTstVar(comboPOOS6)
sumTestError.comboPOOS12 <- errTstVar(comboPOOS12)

# spiderPOOS(comboPOOS6, 'z3m', startYr = 2004)
# spiderPOOS(comboPOOS12, 'z3m', startYr = 2004)

# qtrly data combo
comboPOOS4q <- list('z3m' = tt7_q$z3m['1994::'],
                   'cpi' = tt7_q$cpi['1994::'])
#
comboPOOS4q$z3m <- (tt7_q$z3m['1994::'] +
                   tt6_q$z3m['1994::'] + 
                   tt5_q$z3m['1994::'] + 
                   tt5d_q$z3m['1994::'] + 
                   tt4_q$z3m['1994::']) / 5
#
comboPOOS4q$cpi <- (tt7_q$cpi['1994::'] +
                   tt6_q$cpi['1994::'] + 
                   tt5_q$cpi['1994::']) / 3
#

comboPOOS8q <- list('z3m' = tt7_8q$z3m['1994::'],
                    'cpi' = tt7_8q$cpi['1994::'])
#
comboPOOS8q$z3m <- (tt7_8q$z3m['1994::'] +
                    tt6_8q$z3m['1994::'] + 
                    tt5_8q$z3m['1994::'] + 
                    tt5d_8q$z3m['1994::'] + 
                    tt4_8q$z3m['1994::']) / 5
#
comboPOOS8q$cpi <- (tt7_8q$cpi['1994::'] +
                    tt6_8q$cpi['1994::'] + 
                    tt5_8q$cpi['1994::']) / 3

sumTestError.comboPOOS4q <- errTstVar(comboPOOS4q)
sumTestError.comboPOOS8q <- errTstVar(comboPOOS8q)

# spiderPOOS(comboPOOS4q, 'cpi', startYr = 2001)
# spiderPOOS(comboPOOS8q, 'cpi', startYr = 2001)

mktNZ <- c('NFIX3FRA Index', 
           'ZB1 Comdty', 'ZB2 Comdty', 'ZB3 Comdty', 'ZB4 Comdty', 
           'ZB5 Comdty', 'ZB6 Comdty', 'ZB7 Comdty', 'ZB8 Comdty'
           )
conn <- blpConnect(verbose=FALSE, blpapi.jar.file = helpEnv$Bjar)
mktBBG_rates <- bdp(conn, mktNZ, "PX_LAST")
mktBBG_dates <- c(Sys.Date(), as.Date(bdp(conn, mktNZ, "FO013")[-1,]))
invisible(blpDisconnect(conn)) # now close the connection

lastAct <- max(which(!is.na(comboPOOS12$z3m[,1])))
lastActDate <- index(comboPOOS12$z3m)[lastAct]
z3mRates <- xts(c(as.numeric(comboPOOS12$z3m[lastAct, 1]), mktBBG_rates[1,], 100 - mktBBG_rates[-1,]), 
                order.by = c(index(comboPOOS12$z3m)[lastAct], mktBBG_dates))
names(z3mRates) <- "z3mMkt"

predRates <- rbind(comboPOOS12$z3m[lastActDate, 1], 
                   comboPOOS12$z3m[paste0(lastActDate+1, "::"), c(ncol(comboPOOS12$z3m))])
predRates_q <- rbind(comboPOOS8q$z3m[lastActDate, 1], 
                   comboPOOS8q$z3m[paste0(lastActDate+1, "::"), c(ncol(comboPOOS8q$z3m))])


# join the last actual rate with the forecast, so it meets at the current date

# }}} close combine forecasts
# Plotting code {{{

# plot rates
Model3mMkt <- function(qtr = FALSE) {
    toYear <- paste0("::", as.POSIXlt(Sys.Date())$year + 1901)
    frYear <- paste0(as.POSIXlt(Sys.Date())$year + 1899, "/")
    extendActual <- addDates(comboPOOS12$z3m[,1], N=24)[toYear]
    plot(extendActual[frYear,], ylim = c(2.5, 5), 
         las=2, major.format="%b-%y",
         main = "NZD 3m Bill Model v Mkt")
    lines(z3mRates, las=1, main = "NZD 3m rates v. model")
    lines(z3mRates, type = "o", pch = 15, col = 'green')
    lines(comboPOOS12$z3m['2013::', 1], type = 'o', lwd=2, pch=18, cex=1.25)
    lines(predRates, col='red', type = 'o', pch=19)
    if(qtr) {
        lines(predRates_q, type = 'o', lwd=2, pch=14, col = 'orange')
        legend('top', c('Actual', 'Market', 'Model-m', 'Model-q'), 
               lwd = 2, pch = c(18, 15, 19, 14), col = c(1, 3, 2, 'orange'),
               bg = 'grey94', horiz=TRUE)
    } else {
        legend('top', c('Actual', 'Market', 'Model-m'), 
               lwd = 2, pch = c(18, 15, 19), col = c(1, 3, 2),
               bg = 'grey94', horiz=TRUE)
    }
}

# show the stacked 3m demand factor
demandPlot <- function(startYr = 1999) {
    startToken <- paste0(startYr, "/")
    plot(sf_x[startToken], main = "NZD Demand Factors", 
         major.format = "%b-%y", 
         las=2)
    lines(sf_x, type = 'o', pch = 19, lwd=1)
    lines(ddm1_x, type = 'l', col = 'green', lwd=2)
    legend('top', legend = c('3m Stack', 'MoM'),
           col = c(1, 'green'), pch = c(19, NA), lwd = 2,
           bg = 'gray97',
           horiz = TRUE)
}

# show FCI and non-orth FCI

FCIplot <- function(FCI = fci_sox, startYr = 1999) {
    startToken <- paste0(startYr, "/")
    plot(FCI[startToken,1], main = "NZD FCI (higher = tighter)", 
         major.format = "%b-%y", 
         las=2, 
         ylim = c(-3, 3))
    lines(FCI[,1], type = 'o', lwd = 2, pch=20)
    lines(scale(datBd_mAve$zMCI), type = 'l', lwd=2, col = 'red')
    legend('top', 
           legend = c('FCI 3m Ave', 'RBNZ MCI'),
           col = c(1, 'red'), 
           pch = c(19, NA), lwd = 2,
           bg = 'gray97',
           horiz = TRUE)
}

spiderPOOS(comboPOOS6, 'z3m', startYr = 2004, MAINstring = "NZ 90d Rate: model v Act (6m ahead)")
spiderPOOS(comboPOOS12, 'z3m', startYr = 2004, MAINstring = "NZ 90d Rate: model v Act (12m ahead)")
spiderPOOS(comboPOOS6, 'cpi', startYr = 2004, MAINstring = "NZ CPI (factor mod): model v Act (6m ahead)")
spiderPOOS(comboPOOS12, 'cpi', startYr = 2004, MAINstring = "NZ CPI (factor mod): model v Act (12m ahead)")
# }}} close plotting code
