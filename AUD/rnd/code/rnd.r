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

experimentalRnD <- rndXLS[, c('sector', 'totalRnD')]
experimentalRnD$ppn <- experimentalRnD$totalRnD / experimentalRnD$totalRnD[13]


