require(xts)

load("~/data/tourAveSpeed.RData")

png("~/r/misc/Rpics/aveTour.png")
plot(aveSpeed, las=2, major.format = "%Y", 
     main = "Tour de France: Winners Ave speed")
lines(aveSpeed, lwd=2)
mtext('www.ricardianambivalence.com', side=1, line = 4)
dev.off()

png("~/r/misc/Rpics/aveTour_90pp.png")
plot(aveSpeed['1990::'], las=2, major.format = "%Y", pch=19, type='o', 
     main = "Tour de France: Winners Ave speed (1990 -->)")
points(aveSpeed['1999/2005'], pch=15, cex=1.2, col=2)
mtext('www.ricardianambivalence.com', side=1, line = 4)
dev.off()
