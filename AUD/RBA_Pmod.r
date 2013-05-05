qmod <- read.table(file="P:/R/S/data/RBA_PC_qmod.txt", header=T) 
qmod <- qmod[nrow(qmod):1, ]

#qmod <- ts(qmod, start=c(1990,1), frequency=4) can't attach ts objects

attach(qmod)

lmfit <- lm(TM_CPI ~ MKT_BE_L1q + INV_UR_L2q + UR_yoy_L3q + W_Im_px, data=qmod)

summary(lmfit)

# print summary of model 
par(mfrow=c(2,2)) 
plot(lmfit) 
par(mfrow=c(1,1))

# print actual v forecast 
act <- ts(qmod$TM_CPI, start=c(1990,1), frequency=4) 
plot(act, type="l", col="grey", lwd=2) 
fit = fitted(lmfit) 
fitter <- ts(fit, start=c(1990,1), frequency=4) # can only print similar objects 
lines(fitter, type="l", col="red", lwd=2)