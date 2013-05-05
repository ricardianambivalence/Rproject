dataset <- read.csv("~/data/govBondCurve.csv", header=TRUE)
var <- as.matrix(dataset[,2:16])

for(i in 1:nrow(var)){
plot(var[i,], ylim=c(2,7), ylab="Yield", xlab="Years to Maturity",
     main=as.character(dataset[i,1]), type='o')
Sys.sleep(0.02)
}
