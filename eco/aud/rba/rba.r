require(reshape2)
require(ggplot2)
require(xts)
require(gridExtra)
source("~/R/Rhelpers/helperFuncts.r")

## PATH stuff
projectPATH <- "~/R/AUD/rba"
plotPATH <- file.path(projectPATH, "pics")

load("~/data/aud/mkt/RBA1dOut.RData")

gg_cutHist <- ggplot(subset(melt(dayOut), variable == 'dayOutMkt' &
                                 decision %in% c('cut') & value < 0),
                  aes(x = value, fill = 'red')) +
                  geom_histogram(fill = 'red') +
                  xlim(-100, 0) +
                  labs(y = NULL, x = NULL) +
                  labs(title = "Market pricing (bps) Monday prior to cuts") +
                  theme_grey(16) +
                  theme(legend.position = 'none')
png(file.path(plotPATH, "mkt_1d_cut.png"))
grid.arrange(gg_cutHist, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gg_unchHist <- ggplot(subset(melt(dayOut), variable == 'dayOutMkt' &
                                 decision %in% c('unch') & value < 0),
                  aes(x = value, fill = 'blue')) +
                  geom_histogram(fill = 'blue') +
                  xlim(-25, 0) +
                  labs(y = NULL, x = NULL) +
                  labs(title = "Market pricing (bps) Monday prior to holds") +
                  theme_grey(16) +
                  theme(legend.position = 'none')
png(file.path(plotPATH, "mkt_1d_unch.png"))
grid.arrange(gg_unchHist, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
