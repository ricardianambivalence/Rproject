dd <- read.table(pipe('pbpaste'))
tt <- seq(as.Date("2001-07-01"), as.Date("2012-08-01"), by="month")
png("~/RA/pics/ABS_jobs_hrs.png")
plot(dd$V2~tt, las=1, xlab="", ylab="", main="Log Agg Hrs v. preGFC trend", type='l', col=4)
lines(dd$V1~tt, col=2)
mtext(text="ricardianambivalence.com", side=1, line=3, adj=1)
mtext(text="Source: ABS 6202", side=1, line=3, adj=0)
dev.off()



