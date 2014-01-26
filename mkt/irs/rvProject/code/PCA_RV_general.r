## PCA RV Analysis: S:\Rates Research\R_share\MCJ\nR\RVproject\PCA_RV.r

# TODO: 
# 1/ a lot of work has to be done on names -- so that the same basic script can run on mulitple products
# 2/ to support making it more generic regarding names, you must add switches up to -- for adding IAUS etc for AUD swaps; nodes generally ... 
# 3/ move some more of the functions to a library file and source it

# clean up and ID what are we trading - perhaps get from command line?
cleanUp()
#
# surface %in% {audIRS, audBond, nzdIRS, cadIRSm usdIRS}
surface <- 'audIRS' # points to the right bit of ./mktmap.r
addTOO <- FALSE # facilitates adding series to ADB
rewind <- NULL # If set this takes precedence over startDate in mktmap
## rewind should be set up here -- it should not be in the map!

## use this via

# set up the environment {{{
#
# load required packages
require(RODBC)
require(xts)
require(Rbbg)
require(MASS)
require(ggplot2)
#
workingPATH <- "S:/Rates Research/projects/RVproject"
plotPATH <- file.path(workingPATH, "plots")
dataPATH <- file.path(workingPATH, "data")
codePATH <- file.path(workingPATH, "code")

#Load Functions
source(file.path(codePATH, "PCA_helpers.r"))

# Load the product maps
source(file.path(codePATH, "mktmap.r"))

# end env setup }}}
# {{{ Get data from DB
#
ADBcon <- odbcConnectAccess(get(surface, mktMap)[['db']])
#
tableName <- get(surface, mktMap)[['dbTable']]
query <- paste0('SELECT * FROM ', tableName, ';')
#
QueryResult = sqlQuery(ADBcon, query) 
odbcClose(ADBcon)  
# 
# }}} end fetch from DB
# {{{ load and update data - save it back to ADB

## re-load data and trim off the prior ten sessions
dateCol <- which(names(QueryResult) == 'date')
sortedData <- xts(QueryResult[, -dateCol], order.by = as.Date(QueryResult[, dateCol]))
sortedData <- sortedData[1:(nrow(sortedData) - 10),] # trim off final 10 rows

start.date <- index(last(sortedData))
end.date <- as.POSIXct(Sys.Date() + 1)

# get data from bbg terminal
Bconn <- blpConnect(blpapi.jar.file = Bjar, verbose=FALSE)
securities <- get(surface, mktMap)[['bbgSecurities']]
fields <- c("PX_LAST")
bbg_data <- bdh(Bconn, securities, fields, start.date, end.date, 
                include.non.trading.days = FALSE)
blpDisconnect(Bconn)

# got data - unstack and remove names after periods
bbg_d_X <- bbgUnstacker(bbg_data)
names(bbg_d_X) <- gsub("^\\.+|\\.[^.]*$", "", names(bbg_d_X), perl=TRUE)

# find the semi-semi rates -- IF surface == 'audIRS'
if (surface == 'audIRS') {
  bbg_d_X$ADSWAP1 <- ((1+(bbg_d_X$ADSWAP1Q + bbg_d_X$ADBBCF1/100)/400)^2 -1) * 200
  bbg_d_X$ADSWAP2 <- ((1+(bbg_d_X$ADSWAP2Q + bbg_d_X$ADBBCF2/100)/400)^2 -1) * 200
  bbg_d_X$ADSWAP3 <- ((1+(bbg_d_X$ADSWAP3Q + bbg_d_X$ADBBCF3/100)/400)^2 -1) * 200
}

# match column order btwn new and old data
columnIdx <- match(names(sortedData),names(bbg_d_X))
bbg_d_X <- bbg_d_X[, names(bbg_d_X)[c(columnIdx)]]

# paste the data together
sortedData <- rbind(sortedData, bbg_d_X)

# protect against double values
mn <- match(unique(index(sortedData)), index(sortedData))
sortedData <- na.approx(sortedData[mn,])

##  {{{ ad-hoc additions to the ADB
# get data from bbg terminal and add into ADB
if (addTOO) {
  Bconn2 <- blpConnect()
  securities2 <- c("FDTR Index", "FEDL01 Index", "US0001M Index", "US0003M Index", "US0006M Index")
  fields2 <- c("PX_LAST")
  start.date2 <- as.Date("1997-01-01")
  bbg_data.Xtra <- bdh(Bconn2, securities2, fields2, start.date2, end.date)
  blpDisconnect(Bconn2)
  #
  bbg_data_unstack.Xtra <- unstack((bbg_data.Xtra), PX_LAST~ticker)
  bbg_data_unstack.Xtra <- cbind('date' = as.POSIXct(unique(bbg_data.Xtra$date)), bbg_data_unstack.Xtra)
  bbg_data_unstack.Xtra <- bbg_data_unstack.Xtra[which(!is.na(bbg_data_unstack.Xtra$US0003M.Index)),]
  bbg_d_Xx <- xts(bbg_data_unstack.Xtra[, -1], order.by = bbg_data_unstack.Xtra[, 1])
  #
  namesX <- sapply(bbg_d_Xx, FUN = function(X) strsplit(names(X), "\\.")[[1]][1])
  names(bbg_d_Xx) <- namesX
  #
  # this column order bit is neccessarily fiddly
  sortedData <- merge(sortedData, bbg_d_Xx, join = 'left')
  sortedData <- sortedData[, c(17:21, 1:16)]
  sortedData <- na.locf(sortedData)
  #
  # clean up
  rm(Bconn2, securities2, fields2, start.date2, bbg_data.Xtra, bbg_data_unstack.Xtra, bbg_d_Xx, namesX)
}

# }}} 

# re-open the Access data-table
ADBcon <- odbcConnectAccess(get(surface, mktMap)[['db']])

# delete the old table 
sqlDrop(ADBcon, sqtable = get(surface, mktMap)[['dbTable']]) # note this will hang if the table cannot be found

# insert the new table
sqlSave(ADBcon, data.frame(sortedData), tablename = get(surface, mktMap)[['dbTable']], rownames="date", fast=TRUE)
odbcClose(ADBcon)  #Close connection to DB

# saves an .RData file :: this is the exit == continuation-point
save(sortedData, file = file.path(dataPATH, paste0(surface, ".RData")))

# clean up - only have sortedData remaining in the environment
rm(ADBcon, bbg_d_X, bbg_data, columnIdx, Bconn, end.date, fields, mn, securities, start.date)

# }}} done updating data
# {{{ prepare data for the PCA

# select columns - 

nodeNames <- get(surface, mktMap)[['nodes']]
nodeIdx <- match(nodeNames, names(sortedData))
PCAdf_x <- sortedData[, nodeIdx]

# PCA date range -- set rewind value to adjust scope, or hardcode the date in firstPCADate
lastPCADate <- last(index(PCAdf_x))
if(is.null(rewind)) {
  firstPCADate <- get(surface, mktMap)[['startDate']]
} else {
  firstPCADate <- lastPCADate - rewind
}

# scale - and unscale to check what we are doing!
PCAdf <- PCAdf_x[paste0(firstPCADate, "::")]
PCAback <- PCAdf # this is just for testing -- can come out later
PCAdf_mean <- apply(PCAdf, 2, FUN = mean)
PCAdf_sd <- apply(PCAdf, 2, FUN = sd) 
sPCAdf <- scale(PCAdf)
# UNscaling
Re.center <- -(PCAdf_mean/PCAdf_sd) # center is done before scaling so -mean/sd is anti-center
Re.scale <- (1/PCAdf_sd) # which makes 1/sd the anti-scale
unscaled <- scale(sPCAdf, center=Re.center, scale=Re.scale)

# realised volatility - over twenty sessions;
# rollapplyr means only known data is used -- the default is centered
volFrame = 20
PCAdf_x_trim <- PCAdf_x[paste0(firstPCADate, "::")]
RollSD <- 100*(rollapplyr(PCAdf_x_trim, width=volFrame, FUN = function(X) apply(X, 2, sd), by.column=TRUE))
RollSD <- RollSD[complete.cases(RollSD),]
RollSD_mean <- sapply(RollSD, FUN = function(X) mean(X, na.rm=T))
RollSD_sd <- sapply(RollSD, FUN = function(X) apply(X, 2, FUN = function(Y) sd(Y, na.rm=T))) 
sRollSD <- scale(RollSD)
sRollSD <- sRollSD[complete.cases(sRollSD),]
# UNscaling
Re.Volcenter <- -(RollSD_mean/RollSD_sd)
Re.Volscale  <- 1/RollSD_sd
unscaled_rVol <- scale(sRollSD, center = Re.Volcenter, scale = Re.Volscale)
# end UNscalilng

# }}} done with PCA prep
# PCA ==>>> {{{

# if you want to change the PCAdim, change dimUsed: = c(1,2,3) uses first three factors
dimUsed <- c(1,2,3)
volDimUsed <- c(1,2,3)
# end set-dim

# LEVELS -->> {{{
PCA_Covar <- cov(sPCAdf)
PCA_eValues <- eigen(PCA_Covar)$values
PCA_eVectors <- eigen(PCA_Covar)$vectors

# stick the IRS references to the eVector matrix
dimnames(PCA_eVectors)[[1]] <- colnames(sPCAdf)

# stick the factor references to the eVector matrix
PCA_eVectors <- mtxNameSticker(PCA_eVectors, prepend='F', MARGIN=2)

# if low rates is +ve, multiply eVector Matrix by -1
PCA_eVectors <- if(PCA_eVectors[1,1] < 0) {PCA_eVectors * -1} else PCA_eVectors

# generate factors
factors <- xts(sPCAdf %*% PCA_eVectors[,dimUsed], 
               order.by = index(sPCAdf))

# }}} done LEVELS <<--
# realised vol -->> {{{
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
volFactors <- xts(sRollSD %*% realzdVol_eVectors[,volDimUsed],
                  order.by = index(sRollSD))
# put the dates back on as the rownames: less the (volFrame -1) lost from the sliding window
rownames(volFactors) <- rownames(factors)[-c(1:(volFrame-1))]

# }}} end realised vol PCA

# }}} PCA decomp is complete!
# {{{ the RV engine

# find the `whitened` curves {

# level of rates
whitenedScaledCrv <- factors %*% solve(PCA_eVectors)[dimUsed,]
colnames(whitenedScaledCrv) <- colnames(sPCAdf)
whitenedCrv <- scale(whitenedScaledCrv, center=Re.center, scale=Re.scale)
#
# find differences with the `whitened` curve
scaledDiff <- sPCAdf - whitenedScaledCrv 
bpsDiff <- 100*(PCAdf - whitenedCrv)

# done whitened curve <<--

# realised Vol

whitenedScaledVol <- volFactors %*% solve(realzdVol_eVectors)[volDimUsed,]
colnames(whitenedScaledVol) <- colnames(sRollSD)
whitenedVol <- scale(whitenedScaledVol, center=Re.Volcenter, scale=Re.Volscale)

# find differences with the `whitened` curve
bpsVolDiff <- (RollSD - whitenedVol)

# done with `whitened` curve }

## Check all possible trade combinations -- needs checking to be sure it works. 

# to access trades, use WeightedCombos$trades$legs3[[2]] or  plot(WeightedCombos$trades$legs3$"IR1s*IR2s*IR3s")

WeightedCombos <- getTrades(dd = as.data.frame(PCAdf), Maxleg=4, PCAdf_sd, PCA_eVectors)
VanillaCombos <- getTrades(dd = as.data.frame(PCAdf), Maxleg=4)

ranks_W <- ranker(WeightedCombos$trades) # rank trades
goodTrades_W <- lapply(ranks_W, FUN = function(X) X[which(X < 0.1 | X > 0.9)]) # take only extremes
goodTrades_W <- lapply(goodTrades_W, FUN = function(X) X[rev(order(abs(X - 0.5)))]) # re-order, so best is at the top

ranks_V <- ranker(VanillaCombos$trades) # rank trades
goodTrades_V <- lapply(ranks_V, FUN = function(X) X[which(X < 0.1 | X > 0.9)]) # take only extremes
goodTrades_V <- lapply(goodTrades_V, FUN = function(X) X[rev(order(0.5 - X))]) # re-order, so best is at the top

# get trade history and histogram with:
# tradeTwinPlot("ADSWAP5.ADSWAP7.ADSWAP12", WeightedCombos)
# }}} end RV engine
# standard RV plots/output {{{

# plot of the factors
# gp_fplot <- gplot(meltx(factors), 
#                   aes(x = date, y = value, color = variable), size = 1.2) + 
#                 theme_grey() + 
#                 facet_grid(

# plot of the rich-cheap (bps)
# }}}
