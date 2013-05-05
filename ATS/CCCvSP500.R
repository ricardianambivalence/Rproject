## High Yield v Equities ##

# clean up
rm(list=ls())
require(quantmod)
# Get data

getSymbols("BAMLH0A3HYC",src="FRED") # BAML CCC-down OAS from FRED
getSymbols("SP500",src="FRED") # BAML CCC-down OAS from FRED

CCC <- BAMLH0A3HYC

inst <- merge(CCC, SP500, all=F)
names(inst) <- c('CCC', 'SPX')
inst <- scale(inst)*100 + 10000

par(mar=c(5,4,4,5)+.1)
plot(index(inst), inst$CCC, type='l', xlab="", ylab="CCC", col=2)
par(new=T)
plot(index(inst), inst$SPX, type='l', xaxt='n', yaxt="n", xlab="", ylab="", col=3)
axis(4)
mtext("SPX",side=4,line=3) #
legend("topleft",col=c(2,3),lty=1,legend=c("CCC","SPX"))

mod <- lm(SPX ~ CCC, data=inst)
pred <- predict(mod)
spread <- inst$SPX - pred

plot(index(inst), inst$SPX, type='l', col=2)
lines(index(inst), pred, col=3)

# trading rule -- short SPX when pred is falling, and long when pred is rising

# 1/ subset the data

sT <- start(inst) # start
eT <- index(inst[floor(nrow(inst)/2)]) #middle
sP  <- eT +1 # start 'proof''
eP <- end(inst) # end of data

TrainSub <- inst[as.Date(sT) + 0:(eT-sT)]
ProofSub <- inst[as.Date(sP) + 0:(eP-sP)]

mod_train <- lm(SPX ~ CCC, data=TrainSub)
