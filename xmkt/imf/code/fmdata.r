# get the data and make something of it
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(timsac)
require(data.table)
require(RColorBrewer)
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")
#
## get from web or saved xls?
getWeb <- FALSE
#
## PATH stuff
projectPATH <- "~/R/xmkt/imf"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- file.path(projectPATH, "data")
codePATH <- file.path(projectPATH, "code")

## download from web and format or get from store?
if (getWeb)
{
    imfLink <- 'http://www.imf.org/external/pubs/ft/fm/2013/01/data/fmdata.xlsx'

    genGov_OBnPB <- read.xls(imfLink, sheet = 'STA-T1', as.is=TRUE, skip=4, header=FALSE)
    colnames(genGov_OBnPB) <- c('nation', paste0('cy_', 2006:2018))
    genGov_OverallBalance <- genGov_OBnPB[1:34, ]
    genGov_PrimaryBalance <- genGov_OBnPB[36:69, ]

    genGov_CABs <- read.xls(imfLink, sheet = 'STA-T2', as.is=TRUE, skip=3, header=FALSE)
    colnames(genGov_CABs) <- c('nation', paste0('cy_', 2006:2018))
    genGov_CABs[,-1] <- sapply(genGov_CABs[,-1], as.numeric)
    genGov_CABal <- genGov_CABs[1:34, ]
    genGov_CABal$nation <- c("Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark",
                            "Estonia", "Finland", "France", "Germany", "Greece", "Hong Kong SAR",
                            "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Netherlands",
                            "New Zealand", "Norway", "Portugal", "Singapore", "Slovak Republic",
                             "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
                             "United States", "Average", "Euro area", "G-7", "G-20 advanced")
    genGov_CAPBal <- genGov_CABs[36:69, ]
    genGov_CAPBal$nation <- genGov_CABal$nation

    genGov_RvnEx <- read.xls(imfLink, sheet = 'STA-T3', as.is=TRUE, skip=4, header=FALSE)
    colnames(genGov_RvnEx) <- c('nation', paste0('cy_', 2006:2018))
    genGov_Rev <- genGov_RvnEx[1:34, ]
    genGov_Exp <- genGov_RvnEx[36:69, ]

    save(genGov_OverallBalance, genGov_PrimaryBalance,
         genGov_CABal, genGov_CAPBal,
         genGov_Rev, genGov_Exp,
         file = file.path(dataPATH, "imfFM.rdata"))


} else {
    load(file.path(dataPATH, "imfFM.rdata"))
}

AAA <- c("Australia", "Canada", "Denmark", "Finland", "Germany", "Norway",
"Singapore", "Sweden", "Switzerland")

genGov_OverallBalance$nation <- factor(genGov_OverallBalance$nation, levels = genGov_OverallBalance$nation)
genGov_PrimaryBalance$nation <- factor(genGov_PrimaryBalance$nation, levels = genGov_PrimaryBalance$nation)
genGov_CABal$nation <- factor(genGov_CABal$nation, levels = genGov_CABal$nation)
genGov_CAPBal$nation <- factor(genGov_CAPBal$nation, levels = genGov_CAPBal$nation)
genGov_Rev$nation <- factor(genGov_Rev$nation, levels = genGov_Rev$nation)
genGov_Exp$nation <- factor(genGov_Exp$nation, levels = genGov_Exp$nation)
#
ggOB_melt <- melt(genGov_OverallBalance, id.vars=1)
ggPB_melt <- melt(genGov_PrimaryBalance, id.vars=1)
ggCABal_melt <- melt(genGov_CABal, id.vars=1)
ggCAPBal_melt <- melt(genGov_CAPBal, id.vars=1)
ggR_melt <- melt(genGov_Rev, id.vars=1)
ggX_melt <- melt(genGov_Exp, id.vars=1)

# barchart -- revenue to GDP, IMF data
gp_Rgdp <- ggplot(subset(ggR_melt,
                         nation %in% AAA & variable %in% c('cy_2012')),
                  aes(x = nation, y = value, fill = nation, color = nation)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                scale_color_brewer(palette = 'Set1') +
                labs(title = "Revenue: %GDP") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                theme(axis.text.x = element_text(angle = 90)) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "imfFM_revGDP.png"))
grid.arrange(gp_Rgdp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# barchart -- expenditure to GDP, IMF data
gp_Xgdp <- ggplot(subset(ggX_melt,
                         nation %in% AAA & variable %in% c('cy_2012')),
                  aes(x = nation, y = value, fill = nation, color = nation)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                scale_color_brewer(palette = 'Set1') +
                labs(title = "Expenditure: %GDP") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                theme(axis.text.x = element_text(angle = 90)) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "imfFM_expGDP.png"))
grid.arrange(gp_Xgdp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# barchart -- overall balance as %gdp, IMF data
gp_OBgdp <- ggplot(subset(ggOB_melt,
                         nation %in% AAA & variable %in% c('cy_2012')),
                  aes(x = nation, y = value, fill = nation, color = nation)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                scale_color_brewer(palette = 'Set1') +
                labs(title = "Overall Balance (gen govt): %GDP") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                theme(axis.text.x = element_text(angle = 90)) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "imfFM_obGDP.png"))
grid.arrange(gp_OBgdp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# barchart -- primary balance as %gdp, IMF data
gp_PBgdp <- ggplot(subset(ggPB_melt,
                         nation %in% AAA & variable %in% c('cy_2012')),
                  aes(x = nation, y = value, fill = nation, color = nation)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                scale_color_brewer(palette = 'Set1') +
                labs(title = "Primary Balance (gen govt): %GDP") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                theme(axis.text.x = element_text(angle = 90)) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "imfFM_pbGDP.png"))
grid.arrange(gp_PBgdp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()


# barchart -- CABal as % of GDP
gp_CABalgdp <- ggplot(subset(ggCABal_melt,
                         nation %in% AAA & variable %in% c('cy_2012')),
                  aes(x = nation, y = value, fill = nation, color = nation)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                scale_color_brewer(palette = 'Set1') +
                labs(title = "Cyclically Adjusted Balance: %GDP") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                theme(axis.text.x = element_text(angle = 90)) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "imfFM_caBal.png"))
grid.arrange(gp_CABalgdp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# barchart -- CAPBal as % of GDP
gp_CAPBalgdp <- ggplot(subset(ggCAPBal_melt,
                         nation %in% AAA & variable %in% c('cy_2012')),
                  aes(x = nation, y = value, fill = nation, color = nation)) +
                theme_grey() +
                scale_fill_brewer(palette = 'Set1') +
                scale_color_brewer(palette = 'Set1') +
                labs(title = "Cyclically Adjusted Primary Balance: %GDP") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                theme(axis.text.x = element_text(angle = 90)) +
                geom_bar(stat = 'identity')
#
png(file.path(plotPATH, "imfFM_caPBal.png"))
grid.arrange(gp_CAPBalgdp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
