# clean up

rm(list=ls()); gc()

## functions

# trade max - returns a rolling historical maximum

tradeMax <- function(x) {
  dump <- rep(0, length(x))
  for (i in 2:length(x)) {
    ifelse(dump[(i-1)] > x[i], dump[i] <- dump[(i-1)], dump[i] <- x[i])
  }
  return(dump)
}

#x = 1:100 + runif(100, 0, 30)
#max_x <- tradeMax(x)˜
#plot(max_x, type='l', col=2, lwd=2)
#lines(x)

# drawdown calc - returns deviation from rolling hist max

drawdown <- function(x) {
  dump <- rep(0, length(x))
  dump[1] <- x[1]
  for (i in 2:length(x)) {
    ifelse(dump[(i-1)] > x[i], dump[i] <- dump[(i-1)], dump[i] <- x[i])
  }
  dd <- x - dump
  return(dd)
}

#dd_x <- drawdown(x)
#plot(dd_x, type='l')

## date drawdowns -- returns drawdown dates for zoo / xts objects

date_dd <- function(x){
  x <- ifelse(x==0, 0, 1)
  dateDip <- NULL
  for (i in 2:length(x)){
    ifelse(x[i]==1,
           ifelse(x[i-1]==0,
                  dateDip <- c(dateDip, index(x[i])),
                  NA),
           ifelse(x[i-1]==1,
                  dateDip <- c(dateDip, index(x[i])),
                  NA)
           )
  }
  if(last(x)==1) dateDip <- c(dateDip, index(last(x)))
  return(as.Date(dateDip))
}

#t_x <- as.Date(Sys.Date()+1:100)
#dd_x_xts <- xts(dd_x, t_x)
#x_ddDates <- date_dd(dd_x_xts)

# drawdown-length -- takes output of date_dd and returns time in days
dtime <- function(x){
  drawtime <- NULL
  step <- seq(1, length(x), 2)
  for (i in step) {
    drawtime <- c(drawtime, x[i+1] - x[i])
  }
  return(drawtime)
}

#x_dtime <- dtime(x_ddDates)
#summary(x_dtime)

# sample splitter

# split the sample

train_split <- function(x){
  eT <- end(x)
  sT <- index(x[floor(nrow(x)/2)])
  TrainSub <- x[as.Date(sT) + 0:(eT - sT)]
  return(TrainSub)
  }

proof_split <- function(x){
  eP <- index(x[floor(nrow(x)/2)]) -1
  sP <- start(x)
  ProofSub <- x[as.Date(sP) + 0:(eP - sP)]
  return(ProofSub)
  }

## create a trademap - reads an xts object with signal 'z-conditioner' + exit rule for long and short

trade_filler <- function(x, l_pr, s_pr){      #adds trading rules
  map = x
  map$posi <- map[,1]
  for (i in 2:nrow(x)){
    ifelse(map$posi[i]==0,  #if current value is ==0
           ifelse(map$posi[(i-1)] ==1, # then if long
                  ifelse(map$z[i] < (l_pr), # and it's cheap
                         map$posi[i]<- (1), # stay long
                         map$posi[i] <- 0), # else square up
                  NA),
           NA)
    }
  for (i in 2:nrow(x)){
    ifelse(map$posi[i]==0,  #if current value is ==0
           ifelse(map$posi[(i-1)] == (-1), # then if short
                  ifelse(map$z[i] > (s_pr), # and it's rich
                         map$posi[i]<- (-1), # stay short
                         map$posi[i] <- 0), # else square up
                  NA),
           NA)
    }
  return(map)
}



##calculate a sharpe ratio

## single stock strategy

require(quantmod)
getSymbols('GS', src='yahoo')

GSc  <- Cl(GS)
L_GSc  <- lag(Cl(GS))

dailyReturn_GS  <- log(GSc) - log(L_GSc)
xs_dR_GS  <- dailyReturn_GS - (0.04/252)
xs_dR_GS[1] = 0
Sharpe_GS  <- sqrt(252)*mean(xs_dR_GS, na.rm=T)/sd(xs_dR_GS, na.rm=T) #AR by sqrt trading days per yr

## second stock

getSymbols('MS', src='yahoo')

MSc  <- Cl(MS)
L_MSc  <- lag(Cl(MS))

dailyReturn_MS  <- log(MSc) - log(L_MSc)
xs_dR_MS  <- dailyReturn_MS - (0.04/252)
xs_dR_MS[1] = 0
Sharpe_MS  <- sqrt(252)*mean(xs_dR_MS)/sd(xs_dR_MS) #AR by sqrt trading days per yr

## pairs trade (long GS v. short MS)

pairDR <- (dailyReturn_GS - dailyReturn_MS)/2
pairDR[1]=0

Sharpe_pair  <- sqrt(252)*mean(pairDR, na.rm=T)/sd(pairDR, na.rm=T)
pairPnL <- cumsum(pairDR)
plot(pairPnL)

## drawdowns

max <- tradeMax(pairPnL)

dd_pairs <- drawdown(pairPnL)

summary(dd_pairs)
plot(dd_pairs)

# step 3  - find how long the drawdown lasts

d_dates <- date_dd(dd_pairs)

## find out how long drawdowns last

draw_times <- dtime(d_dates)

## summarise drawdowns

summary(draw_times)

## a long short model -- GS v MS ##

frame <- merge(GS$GS.Close, MS$MS.Close, all=F) #intersection of the data
mod <- lm(GS.Close ~ MS.Close, data=frame) # linear model
pred <- predict(mod) # predicted values
spread <- GS$GS.Close - pred

plot(index(frame), frame$GS.Close, type='l', col=3, lwd=2)
lines(index(frame), pred, type='l', col=2)
plot(spread)

trainSub <- train_split(frame)
proofSub <- proof_split(frame)

#trade: long GS when it's 2 sigma cheap (spread is low), short when 2 sigma rich, square @ [-1:1]

trainMod <- lm(GS.Close ~ MS.Close, data=trainSub)

trainPred <- predict(trainMod)
trainSpread <- trainSub$GS.Close - trainPred

zs <- (trainSpread - mean(trainSpread))/sd(trainSpread)
names(zs) <- "z"

trades <- ifelse(zs < -1.95,
                 1,
                 ifelse(zs > 1.95,
                        -1,
                        0)
                 )

names(trades) <- "signal"
checker <- merge(trades, zs)

trademap <- trade_filler(checker, -0.5, 0.5)

dailyReturns <- log(frame) - log(lag(frame)) #can use entire data set as zoo controls addition etc

PnL <- (trademap$posi*dailyReturns$GS.Close + trademap$posi*dailyReturns$MS.Close)/2
PnL[1] <- 0
returns <- cumsum(PnL)
plot(returns)

Training_Sharpe_GSMS  <- sqrt(252)*mean(PnL, na.rm=T)/sd(PnL, na.rm=T)
names(Training_Sharpe_GSMS) <- "Training_SR"
Training_Sharpe_GSMS

dd_train <- drawdown(returns)
plot(dd_train)

## proof

proofPred <- predict(trainMod, newdata=proofSub)
proofSpread <- proofSub$GS.Close - proofPred

p_zs <- (proofSpread - mean(trainSpread))/sd(trainSpread)
names(p_zs) <- "z"

p_trades <- ifelse(p_zs < -1.95,
                 1,
                 ifelse(p_zs > 1.95,
                        -1,
                        0)
                 )

names(p_trades) <- "signal"
p_checker <- merge(p_trades, p_zs)

p_trademap <- trade_filler(p_checker, -0.5, 0.5)

#dailyReturns <- log(frame) - log(lag(frame)) #can use entire data set as zoo controls addition etc


p_PnL <- (p_trademap$posi*dailyReturns$GS.Close + p_trademap$posi*dailyReturns$MS.Close)/2
p_PnL[1] <- 0
p_returns <- cumsum(p_PnL)
plot(p_returns)

proof_Sharpe_GSMS  <- sqrt(252)*mean(p_PnL, na.rm=T)/sd(p_PnL, na.rm=T)
names(proof_Sharpe_GSMS) <- "proof_SR"
proof_Sharpe_GSMS

dd_proof <- drawdown(p_returns)
plot(dd_proof)
