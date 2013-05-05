require(xts)
require(ggplot2)
require(reshape2)
require(gridExtra)

projectPATH <- "~/R/aud/housing"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- "~/data/aud/housing/"

load("~/data/aud/housing/auction.Rdata")

# make some monthly average data
ddx_m <- apply.monthly(ddx, colMeans, na.rm = TRUE)

newRow <- xts((coredata(ddx_m['20121230']) + coredata(ddx_m['2013-02-24']))/2,
                                     order.by = as.Date('2013-01-27'))

ddx_m['2013-01-27', ] <- xts((coredata(ddx_m['20121230']) + coredata(ddx_m['2013-02-24']))/2,
                                     order.by = as.Date('2013-01-27'))

# plot part

png(file.path(plotPATH, "adelACR.png"))
plot(ddx$adel_cr * 100, major.format = "%b-%y", las=1,
     main = "Auction Clearance Rates: Adelaide")
lines(ddx_m$adel_cr * 100, lwd = 3, col=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: APM /RPdata', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "brisACR.png"))
plot(ddx$bris_cr * 100, major.format = "%b-%y", las=1,
     main = "Auction Clearance Rates: Brisbane")
lines(ddx_m$bris_cr * 100, lwd = 3, col=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: APM /RPdata', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "melbACR.png"))
plot(ddx$melb_cr * 100, major.format = "%b-%y", las=1,
     main = "Auction Clearance Rates: Melbourne")
lines(ddx_m$melb_cr * 100, lwd = 3, col=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: APM /RPdata', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "sydACR.png"))
plot(ddx$syd_cr * 100, major.format = "%b-%y", las=1,
     main = "Auction Clearance Rates: Sydney")
lines(ddx_m$syd_cr * 100, lwd = 3, col=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: APM /RPdata', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "perthACR.png"))
plot(ddx$perth_cr * 100, major.format = "%b-%y", las=1,
     main = "Auction Clearance Rates: Perth")
lines(ddx_m$perth_cr * 100, lwd = 3, col=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: APM /RPdata', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "wAveACR.png"))
plot(ddx$wAve_cr * 100, major.format = "%b-%y", las=1,
     main = "Auction Clearance Rates: National weighted ave")
lines(ddx_m$wAve_cr * 100, lwd = 3, col=3)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Source: APM /RPdata', side=1, line=4, adj=0)
dev.off()

ddx_melt <- meltx(ddx)

# re-order levels

ddx_melt$variable <- factor(ddx_melt$variable, levels(ddx_melt$variable)[c(16, 14, 12, 10, 13, 9, 11, 15, 1:8)])

# level stack -- pt + ft
gp_AuctionVolumes <- ggplot(subset(ddx_melt, date >= as.Date('2009-01-01') & variable %in% c('adel_no', 'bris_no', 'canb_no', 'melb_no', 'perth_no',
                                                       'syd_no', 'tas_no', 'total_no')),
                  aes(x = date, y = value, fill = variable)) +
                theme_grey() +
                facet_grid(variable ~ ., scale = 'free_y') + theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                # scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "Auctions") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'top') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "auctVols.png"))
grid.arrange(gp_AuctionVolumes, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
