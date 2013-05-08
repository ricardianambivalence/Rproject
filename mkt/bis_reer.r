# get the ABS file and make some labour market charts
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(TTR)
require(gridExtra)
require(timsac)

## get from web or saved xls?
getWeb <- FALSE

## PATH stuff
projectPATH <- "~/R/mkt"
plotPATH <- file.path(projectPATH, "pics")

## download from web and format or get from store?
if (getWeb)
{
    bisBroad <- "http://www.bis.org/statistics/eer/broad1304.xls"
    bisB <- read.xls(bisBroad, sheet = 'Real', skip=4, as.is=TRUE)
    bisB[,1] <- as.Date(paste0(substr(bisB[,1], 4, 7), "-", substr(bisB[,1], 1, 2), "-01"), format = "%Y-%m-%d")
    names(bisB)[1] <- 'date'
    # names(absT1) <- c('date', 'ftM', 'ftF', 'ft', 'ptM', 'ptF', 'pt', 'nM', 'nF', 'n',
    #                          'unFtM', 'unFtF', 'unFT', 'unPtM', 'unPtF', 'unPt', 'unM', 'unF', 'un',
    #                          'lfM', 'lfF', 'lf', 'urFtM', 'urFtF', 'urFt', 'urM', 'urF', 'ur',
    #                          'prM', 'prF', 'pr', 'nPopM', 'nPopF', 'nPop', 'unPopM', 'unPopF', 'unPop'
    #                          )
    save(bisB, file = "~/data/mkt/fx/bisB.RData")

} else {
    load("~/data/mkt/fx/bisB.RData")
}

bisB_2002_2008 <- subset(bisB, date >= as.Date('2002-01-01') & date <= as.Date('2008-12-31'))
means_2002_2008 <- colMeans(bisB_2002_2008[,-1])
bisB_adj <- bisB

for (i in seq_along(means_2002_2008))
{
    bisB_adj[,(i+1)] <- 100 * (bisB[, (i+1)] / means_2002_2008[i])
 }

REERrank <- rank(tail(bisB_adj[, -1], 1))
REERrank_order <- REERrank[order(REERrank)]
REER_top7 <- REERrank_order[(length(REERrank_order) - 7):(length(REERrank_order))]
bisB_adj_top7 <- bisB_adj[, names(REER_top7)]
names(bisB_adj_top7) <- c('Sing', 'China', 'Slvkia', 'Aus', 'Clmbia',
                          'Phllpns', 'Russia', 'Brzl')


gp_reerLines <- ggplot(bisB_adj, aes(date)) +
                    geom_line(aes(y = RBAU), color = 'gold', size = 1.3) +
                    geom_line(aes(y = RBNZ), color = 'black', size = 1.3) +
                    labs(y = "Ave [2002, 2008] = 100", x = NULL) +
                    labs(title = "BIS Broad REER (NZ = Black, Au = Gold)")
png(file.path(plotPATH, "gp_reerAuNz.png"))
grid.arrange(gp_reerLines, sub = textGrob("www.ricardianambivalence.com"))
dev.off()

gp_reerBar7 <- ggplot(melt(tail(bisB_adj_top7, 1)),
                    aes(x = variable, y = value - 100, color = variable, fill = variable)) +
                    labs(y = NULL, X = '') +
                    theme(axis.title.x = element_blank()) +
                    theme(legend.position = 'none') +
                    labs(title = "% Above 2002-2008 Average") +
                    geom_bar(stat = 'identity') +
                    scale_fill_brewer(palette = 'Set1') +
                    scale_color_brewer(palette = 'Set1')
png(file.path(plotPATH, "gp_reerBar7.png"))
grid.arrange(gp_reerBar7, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
