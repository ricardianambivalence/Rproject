# get the data and make labour market charts
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
projectPATH <- "~/R/JPY/money"
plotPATH <- file.path(projectPATH, "Rpics")

## download from web and format or get from store?
if (getWeb)
{
    BoJmm <- read.csv("~/data/jpy/money/m_en.csv", header=TRUE,
             as.is=TRUE, skip=3)
    save(BoJmm, file = "~/data/jpy/money/BoJ_m.RData")
} else {
    load("~/data/jpy/money/BoJ_m.RData")
}

BoJmm <- BoJmm[ -c(1:2), ]
BoJmm[, 1] <- as.Date(paste0(substr(BoJmm[,1], 1, 4), "-", substr(BoJmm[,1], 6, 7), "-01"), format = "%Y-%m-%d")
moneyB_plus <- BoJmm[, c(1, 8, 9, 10, 11, 20, 21, 75, 76)]
names(moneyB_plus) <- c('date', 'moneyBase', 'mB_notes', 'mB_coins', 'mB_CrntAcct', 'JPY_neer', 'JPY_reer',
                        'real_x', 'real_m')

moneyB_plus[, -1] <- sapply(moneyB_plus[, -1], as.numeric)
moneyB_plus[, 2:5] <- lapply(moneyB_plus[, 2:5], FUN = function(X) X/10000)

money_melt <- melt(moneyB_plus[-c(1:15),], measure.vars = c(2:9))

# moneyBase stack
gp_jpyMoneyBase <- ggplot(subset(money_melt, variable %in% c('mB_notes', 'mB_coins', 'mB_CrntAcct')),
                  aes(x = date, y = value, fill = variable)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1', guide = guide_legend(reverse = TRUE)) +
                labs(title = "Japanese Money Base by type (JPYtn)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'stack')
#
png(file.path(plotPATH, "jpyMoneyBase.png"))
grid.arrange(gp_jpyMoneyBase, sub = textGrob('www.ricardianambivalence.com'))
dev.off()



