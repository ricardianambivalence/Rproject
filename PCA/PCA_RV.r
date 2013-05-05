## PCA RV Analysis: S:\Rates Research\R_share\MCJ\nR\PCA_RV.r

# TODO: 
# 1/ change PCAdf_1.10 to something less misleading -- it can be more than 1-10yr!!
# 2/ move functions to a library file and source it

# set up the environment {{{
rm(list=ls())
Sys.setenv(TZ='GMT')

#Load Functions
source("S:/Rates Research/R_share/MCJ/nR/PCA_helpers.r")

# load required packages
require(RODBC)
require(xts)
require(Rbbg)

# end env setup }}}

# {{{ Get data from DB

# ADBcon <-  odbcConnectAccess("S:/Rates Research/autodata/mkt data/IRS.mdb") 
#  
# tableName <- 'AUDplay'
# query <- paste0('SELECT * FROM ', tableName, ';')
# 
# QueryResult = sqlQuery(ADBcon, query) 
# odbcClose(ADBcon)  

# }}} end fetch from DB

# {{{ sort data and save

# AUDdat <- QueryResult[order(QueryResult[,1], decreasing=FALSE),]
# rownames(AUDdat) <- NULL
# AUDdat <- xts(AUDdat[,-1], order.by=as.Date(AUDdat[,1]))
# names(AUDdat) <- c('RBA', 'IR1q', 'IR2q', 'IR3q', 'IR1s', 'IR2s', 'IR3s', 'IR4s', 'IR5s', 'IR7s', 'IR10s', 
#                    'IR12s', 'IR15s', 'IR20s', 'IR25s', 'IR30s', 'b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'b10', 
#                    'b12', 'b15', 'b20', 'b25', 'b30')
# 
# save(AUDdat, file="S:/Rates Research/projects/RV/data/IRS_AUD.RData")

# }}} end sort and save

# {{{ load and update data - save it back to ADB

## re-load data and trim off the prior ten sessions
# rm(list=ls())
load("S:/Rates Research/projects/RV/data/IRS_AUD.RData", .GlobalEnv)

rowTrim <- nrow(AUDdat) - 10
AUDdat <- AUDdat[-(rowTrim:nrow(AUDdat)),]

start.date <- index(AUDdat[nrow(AUDdat),1]) + 1
end.date <- as.POSIXct(Sys.Date())

# get data from bbg terminal
conn <- blpConnect()
securities <- c("RBACTRD Index", "ADSWAP1Q IAUS Curncy", "ADSWAP2Q IAUS Curncy", "ADSWAP3Q IAUS Curncy", "ADSWAP4 IAUS Curncy", "ADSWAP5 IAUS Curncy",
		"ADSWAP7 IAUS Curncy", "ADSWAP10 IAUS Curncy", "ADSWAP12 IAUS Curncy", "ADSWAP15 IAUS Curncy", "ADSWAP20 IAUS Curncy",
		"ADSWAP25 IAUS Curncy", "ADSWAP30 IAUS Curncy", "ADBBCF1 Curncy", "ADBBCF2 Curncy", "ADBBCF3 Curncy", "ADBBCF4 Curncy",
		"ADBBCF5 Curncy", "ADBBCF7 Curncy", "ADBBCF10 Curncy", "ADBBCF12 Curncy", "ADBBCF15 Curncy", "ADBBCF20 Curncy", 
		"ADBBCF25 Curncy", "ADBBCF30 Curncy")
fields <- c("PX_LAST")
bbg_data <- bdh(conn, securities, fields, start.date, end.date)

# got data - now clean it up

bbg_data_unstack <- unstack((bbg_data), PX_LAST~ticker)
bbg_data_unstack$date <- as.POSIXct(unique(bbg_data$date))
bbg_data_unstack <- bbg_data_unstack[which(!is.na(bbg_data_unstack$RBACTRD.Index)),]  # only use day where there's an RBA rate

bbg_d_X <- xts(bbg_data_unstack[,-26], order.by=bbg_data_unstack[,26])  
bbg_d_X <- na.locf(bbg_d_X)

# make the names the same
names(bbg_d_X) <- c('b1', 'b10', 'b12', 'b15', 'b2', 'b20', 'b25', 'b3', 'b30', 'b4', 'b5', 'b7', 
		   'IR10s', 'IR12s', 'IR15s', 'IR1q', 'IR20s', 'IR25s', 'IR2q', 'IR30s', 'IR3q',
		   'IR4s', 'IR5s', 'IR7s', 'RBA')

# find the semi-semi rate
bbg_d_X$IR1s <- ((1+(bbg_d_X$IR1q + bbg_d_X$b1/100)/400)^2 -1)*200
bbg_d_X$IR2s <- ((1+(bbg_d_X$IR2q + bbg_d_X$b2/100)/400)^2 -1)*200
bbg_d_X$IR3s <- ((1+(bbg_d_X$IR3q + bbg_d_X$b3/100)/400)^2 -1)*200

columnIdx <- match(names(AUDdat),names(bbg_d_X))
bbg_d_X <- bbg_d_X[, names(bbg_d_X)[c(columnIdx)]]

# paste the data together
AUDdat <- rbind(AUDdat, bbg_d_X)

# protect against double values
mn <- match(unique(index(AUDdat)), index(AUDdat))
AUDdat <- AUDdat[mn,]

# re-open the Access data-table
ADBcon <-  odbcConnectAccess("S:/Rates Research/autodata/mkt data/IRS.mdb") 

# delete the old table 
sqlDrop(ADBcon, sqtable="AUDplay") # note this will hang if the table cannot be found

# insert the new table
sqlSave(ADBcon, data.frame(AUDdat), tablename="AUDplay", rownames="date", fast=T)
odbcClose(ADBcon)  #Close connection to DB

save(AUDdat, file="S:/Rates Research/projects/RV/data/IRS_AUD.RData")

# clean up - only have AUDdat remaining in the environment
rm(ADBcon, bbg_d_X, bbg_data, bbg_data_unstack, columnIdx, conn, end.date, fields, mn, rowTrim, securities, start.date)

# }}} done updating data

# {{{ prepare data for the PCA

# select columns - 

# nodeNames <- c('IR2s', 'IR3s', 'IR4s', 'IR5s', 'IR7s', 'IR10s', 'IR12s', 'IR15s', 'IR20s', 'IR25s', 'IR30s') 
nodeNames <- c('IR1s', 'IR2s', 'IR3s', 'IR4s', 'IR5s', 'IR7s', 'IR10s')
nodeIdx <- match(nodeNames, names(AUDdat))
PCAdf_1.10x <- AUDdat[, nodeIdx]

# set date range -- set rewind value to adjust scope, or hardcode the date in firstPCADate

# rewind <- 2500
lastPCADate <- last(index(PCAdf_1.10x))
firstPCADate <- as.Date("1998-05-27")
# firstPCADate <- lastPCADate - rewind

# scale - and unscale to check what we are doing!
PCAdf_1.10 <- data.frame(PCAdf_1.10x[index(PCAdf_1.10x)>= firstPCADate])
PCAback <- PCAdf_1.10 # this is just for testing -- can come out later
PCAdf_1.10_mean <- sapply(PCAdf_1.10, FUN = mean)
PCAdf_1.10_sd <- sapply(PCAdf_1.10, 2, FUN = sd) 
sPCAdf_1.10 <- scale(PCAdf_1.10)

# UNscaling [tested ... it works!]
# NOTE: the center operation is done before scaling, so `-mean/sd` is the Reverse.center
# the second step is multiplying by `1/sd` to reverse the scaling
Re.center <- -(PCAdf_1.10_mean/PCAdf_1.10_sd)
Re.scale  <- (1/PCAdf_1.10_sd)
# unscaled <- scale(sPCAdf_1.10, center=Re.center, scale=Re.scale)
# done unscaling

# realised volatility - over twenty sessions;
# right means only the 1st est belongs to the 20th period
volFrame = 20
PCAdf_1.10x_trim <- PCAdf_1.10x[index(PCAdf_1.10x)>= firstPCADate]
RollSD <- 100*(rollapplyr(PCAdf_1.10x_trim, width=volFrame, FUN=sd, by.column=T))

RollSD_mean <- sapply(RollSD, FUN = mean)
RollSD_sd <- sapply(RollSD, 2, FUN = sd) 
sRollSD <- (scale(RollSD))

# UNscaling
Re.Volcenter <- -(RollSD_mean/RollSD_sd)
Re.Volscale  <- (1/RollSD_sd)
# end UNscalilng

# }}} done with PCA prep

# {{{ PCA ==>>>

# if you want to change the PCAdim, change dimUsed: = c(1,2,3) uses first three factors
dimUsed <- c(1,2,3)
volDimUsed <- c(1,2,3)
# end set-dim

# LEVELS -->>
PCA_Covar <- cov(sPCAdf_1.10)
PCA_eValues <- eigen(PCA_Covar)$values
PCA_eVectors <- eigen(PCA_Covar)$vectors

# stick the IRS references to the eVector matrix
dimnames(PCA_eVectors)[[1]] <- colnames(sPCAdf_1.10)

# stick the factor references to the eVector matrix
PCA_eVectors <- mtxNameSticker(PCA_eVectors, prepend='F', MARGIN=2)

# if low rates is +ve, multiply eVector Matrix by -1
PCA_eVectors <- if(PCA_eVectors[1,1] < 0) {PCA_eVectors * -1} else PCA_eVectors

# generate factors
factors <- sPCAdf_1.10 %*% PCA_eVectors[,dimUsed]

# done LEVELS <<--

# realised vol -->>
realzdVol_Covar <- cov(sRollSD)
realzdVol_eValues <- eigen(realzdVol_Covar)$values
realzdVol_eVectors <- eigen(realzdVol_Covar)$vectors

# stick the IRS references to the eVector matrix
dimnames(realzdVol_eVectors)[[1]] <- colnames(sRollSD)

# stick the factor references to the eVector matrix
realzdVol_eVectors <- mtxNameSticker(realzdVol_eVectors, prepend='F', MARGIN=2)

# if low vol == +ve, multiply eVector Matrix by -1
realzdVol_eVectors <- if(realzdVol_eVectors[1,1] < 0) {realzdVol_eVectors * -1} else realzdVol_eVectors

# generate Vol Factors
volFactors <- sRollSD %*% realzdVol_eVectors[,volDimUsed]
# put the dates back on as the rownames: less the (volFrame -1) lost from the sliding window
rownames(volFactors) <- rownames(factors)[-c(1:(volFrame-1))]

# }}} PCA decomp is complete!

# {{{ the RVengine ==>>> 

# find the `whitened` curve {

# level of rates
whitenedScaledCrv <- factors %*% solve(PCA_eVectors)[dimUsed,]
colnames(whitenedScaledCrv) <- colnames(sPCAdf_1.10)
whitenedCrv <- scale(whitenedScaledCrv, center=Re.center, scale=Re.scale)

# find differences with the `whitened` curve
scaledDiff <- sPCAdf_1.10 - whitenedScaledCrv 
bpsDiff <- 100*(PCAdf_1.10 - whitenedCrv)

# realised Vol

whitenedScaledVol <- volFactors %*% solve(realzdVol_eVectors)[volDimUsed,]
colnames(whitenedScaledVol) <- colnames(sRollSD)
whitenedVol <- scale(whitenedScaledVol, center=Re.Volcenter, scale=Re.Volscale)

# find differences with the `whitened` curve
# scaledVolDiff <- sRollSD - whitenedScaledVol 
# bpsVolDiff <- 100*(scale(scaledVolDiff, center=rep(0, ncol(scaledVolDiff)), scale=Re.Volscale))
bpsVolDiff <- (RollSD - whitenedVol)

# done with `whitened` curve }

## Check all possible trade combinations -- needs checking to be sure it works. 

WeightedCombos <- getTrades(dd=PCAdf_1.10, Maxleg=4, PCAdf_1.10_sd, PCA_eVectors)
VanillaCombos <- getTrades(dd=PCAdf_1.10, Maxleg=4)

## have checked these functions -- they work! (70% sure)
# note we need to retrieve the weights for trade reccos ... 

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#               after here is total messing about
#__________________________________________________________________________________________

# need to pass in weights ... consider a for loop: 

i <- 2
# a container for the output
tradeList <- list()

# a name stamp for stamping list positions in tradeList 
tradeLeg <- paste0('legs', i)

# a list with each slot corresponding to a trade grouping: ie 1*2*3
tradeLegsList <- combn(names(PCAdf_1.10), i, function(x) PCAdf_1.10[x], simplify = FALSE)

# a matrix with ncol(nameMtx) == length(tradeLegsList)
nameMtx <- combn(names(PCAdf_1.10), i) 

# applying the names from nameMtx --> tradeLegsList ... 
# now tradeLegsList[[1]] == tradeLegsList$"IR1s*IR2s*IR3s"
names(tradeLegsList) <- apply(nameMtx, MARGIN=2, FUN=function(x) paste(x, collapse='*'))

# a list of character vectors: smashedTrades[[1]] == c('IR1s', 'IR2s', 'IR3s')
smashedTrades <- strsplit(names(tradeLegsList), "\\*")

# stick the IRS references to the eVector matrix
dimnames(PCA_eVectors)[[1]] <- colnames(sPCAdf_1.10)

# stick the factor references to the eVector matrix
PCA_eVectors <- mtxNameSticker(PCA_eVectors, prepend='F', MARGIN=2)

# a returns a list with the PCA_neutral delta weights, dra
# inputs are the trade names list, the sds, and the eVectors
# i have checked these, and they are accurate.
rvV <- relVols(smashedTrades, PCAdf_1.10_sd, PCA_eVectors)

oo <- colADD(tradeLegsList[[1]], rvV[[1]])

i=2

outlist <- list()
for ( i in 1:length(tradeLegsList)){
     outlist[[i]] <- colADD(tradeLegsList[[i]], w[[i]])
}
names(outlist) <- names(tradeLegsList) 


# http://stackoverflow.com/questions/6253159/using-lapply-with-changing-arguments



# for the PCA trades, the weights come from the eigenvector matrix
# these weights are then multiplied by the vol ratios, to obtain the optimal weights
# these optimal weights are then multiplied by the vol ratios to get the weights in rates / delta space
# we can maybe get this done by naming the rows and columns of the eigenvector matrix

mm <- matrix(1:16, 4)
dimnames(mm)[[1]] <- c('ir1', 'ir2', 'ir3', 'ir4')
dimnames(mm)[[2]] <- c('f1', 'f2', 'f3', 'f4')

mm['ir1', 'f2']


testDF <- data.frame(aaa = 1:20, bbb = seq(4, 80, 4), ccc = -1:-20, ddd = seq(2,40,2)^2, 
		     eee = rep(c(5,-5), 10))

testDF_mean <- lapply(testDF, mean)
testDF_sd <- lapply(testDF, sd)
testDF_scaled <- scale(testDF)

# get the eValues and eVectors from the covar matrix
dimUsed <- c(1,2,3)
PCA_Covar <- cov(testDF_scaled)
PCA_eValues <- eigen(PCA_Covar)$values
PCA_eVectors <- eigen(PCA_Covar)$vectors

dimnames(PCA_eVectors)[[1]] <- names(testDF)

PCA_eVectors <- mtxNameSticker(PCA_eVectors, prepend='F', MARGIN=2)

# makes F1 +ve
PCA_eVectors <- if(PCA_eVectors[1,1] < 0) {PCA_eVectors * -1} else PCA_eVectors

# this section needs to return a list similar to the above
tradeNamesMtx <- combn(rownames(PCA_eVectors), 3)
tradeNamesList <- apply(tradeNamesMtx, MARGIN=2, FUN=function(x) paste(x, collapse='*'))

smashedTrades <- strsplit(tradeNamesList, "\\*")

# this may have to be written as an explicit loop, rather than using _apply magic

# for a fly trade:
ffs <- c('F1', 'F2')
t1 <- smashedTrades[[1]]
W <- t(PCA_eVectors)[ffs, t1[c(1,3)], drop=FALSE]
g <- t(PCA_eVectors)[ffs, t1[2], drop=FALSE]
x <- solve(W,g)
w1 <- c(x[1], -1, x[2])


rvV <- relVols(smashedTrades, testDF_sd, PCA_eVectors)

