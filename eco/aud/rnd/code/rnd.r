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
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")

## get from web or saved xls?
getWeb <- FALSE

## {{{ PATH stuff
projectPATH <- "~/R/AUD/rnd"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- file.path(projectPATH, "data")
codePATH <- file.path(projectPATH, "code")
# }}}


## {{{ get data and format it

rndXLS <- read.xls(file.path(dataPATH, "berd_1dig.xlsx"), sheet = 'Sheet1')

basicRnD <- rndXLS[, c('sector', 'pureBasic')]
basicRnD$ppn <- basicRnD$pureBasic / basicRnD$pureBasic[13]

stratBasicRnD <- rndXLS[, c('sector', 'strategicBasic')]
stratBasicRnD$ppn <- stratBasicRnD$strategicBasic / stratBasicRnD$strategicBasic[13]

appldRnD <- rndXLS[, c('sector', 'appliedResearch')]
appldRnD$ppn <- appldRnD$appliedResearch / appldRnD$appliedResearch[13]

experimentalRnD <- rndXLS[, c('sector', 'experimentalDev')]
experimentalRnD$ppn <- experimentalRnD$experimentalDev / experimentalRnD$experimentalDev[13]

totalRnD <- rndXLS[, c('sector', 'totalRnD')]
totalRnD$ppn <- totalRnD$totalRnD / totalRnD$totalRnD[13]

gp_basic <- ggplot(data = basicRnD[-13,], aes(x = sector, y = 100*ppn, fill = sector)) +
                    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
                    labs(title = "Basic RnD: percent of total BERD") +
                    labs(y = NULL, x = NULL) +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "basicRnD.png"))
grid.arrange(gp_basic, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_stratBasic <- ggplot(data = stratBasicRnD[-13,], aes(x = sector, y = 100*ppn, fill = sector)) +
                    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
                    labs(title = "Strategic Basic RnD: percent of total BERD") +
                    labs(y = NULL, x = NULL) +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "stratbasicRnD.png"))
grid.arrange(gp_stratBasic, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_appld <- ggplot(data = appldRnD[-13,], aes(x = sector, y = 100*ppn, fill = sector)) +
                    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
                    labs(title = "Applied Research: percent of total BERD") +
                    labs(y = NULL, x = NULL) +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "appliedRnD.png"))
grid.arrange(gp_appld, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_experimntl <- ggplot(data = experimentalRnD[-13,], aes(x = sector, y = 100*ppn, fill = sector)) +
                    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
                    labs(title = "Experimental Development: percent of total BERD") +
                    labs(y = NULL, x = NULL) +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "expDev.png"))
grid.arrange(gp_experimntl, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_RnDTtl <- ggplot(data = totalRnD[-13,], aes(x = sector, y = 100*ppn, fill = sector)) +
                    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
                    labs(title = "Total Research and Development: percent of total BERD") +
                    labs(y = NULL, x = NULL) +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "totalRnD.png"))
grid.arrange(gp_RnDTtl, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

