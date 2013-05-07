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
require(RColorBrewer)
source("~/R/Rhelpers/helperFuncts.r")

# color palette
RApal <- brewer.pal(10, "RdYlBu")
RApal_front5 <- brewer.pal(5, "RdYlBu")[1:5]
RApal_back5 <- brewer.pal(10, "RdYlBu")[6:10]
RAPal_5 <- brewer.pal(5, 'RdYlBu')

## get from web or saved xls?
getWeb <- TRUE

## PATH stuff
projectPATH <- "~/R/AUD/trade"
plotPATH <- file.path(projectPATH, "Rpics")

## download from web and format or get from store?
if (getWeb)
{
    absT14a <- "http://abs.gov.au/ausstats/meisubs.NSF/log?openagent&5368014a.xls&5368.0&Time%20Series%20Spreadsheet&11CD414BE5BA1F31CA257B63001414BB&0&Mar%202013&07.05.2013&Latest"
    absT14aXlsD1 <- read.xls(absT14a, sheet = 'Data1')
    absT14aXlsD2 <- read.xls(absT14a, sheet = 'Data2')
    absT14aD1 <- absT14aXlsD1[-c(1:9),]
    absT14aD2 <- absT14aXlsD2[-c(1:9),]
    absT14aD1[,1] <- as.Date(paste0(substr(absT14aD1[,1], 5, 9), "-",
                                    substr(absT14aD1[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    absT14aD2[,1] <- as.Date(paste0(substr(absT14aD2[,1], 5, 9), "-",
                                    substr(absT14aD2[,1], 1, 3), "-01"), format = "%Y-%b-%d")
    absT14aD1[,-1] <- sapply(absT14aD1[, -1], FUN = function(X) as.numeric(as.character(X)))
    absT14aD2[,-1] <- sapply(absT14aD2[, -1], FUN = function(X) as.numeric(as.character(X)))
    absT14a <- cbind(absT14aD1, absT14aD2[, -1])
    names(absT14a)[1] <- 'date'
    rm(absT14aXlsD1, absT14aD1, absT14aD2, absT14aXlsD2)
    save(absT14a, file = "~/data/aud/trade/tradeXByDest.RData")
} else {
    load("~/data/aud/trade/tradeXByDest.RData")
}

xDestSub <- c('date', 'China..', 'Hong.Kong..SAR.of.China...', 'Korea..Republic.of..',
             'India..', 'Japan..', 'New.Zealand..', "Switzerland..",
             'United.Kingdom..', 'United.States.of.America..', 'ASEAN..','Euro.area..',
             'Total..Country.of.Destination...')

xSub <- absT14a[, xDestSub]

# build up the summed sub set
xSub_sum <- data.frame('date' = xSub[, 'date']) # make with date
xSub_sum$China_HK <- xSub[, 'China..'] + xSub[, 'Hong.Kong..SAR.of.China...']
xSub_sum$Japan <- xSub[, 'Japan..']
xSub_sum$ASEAN <- xSub[, 'ASEAN..']
xSub_sum$Korea <- xSub[, 'Korea..Republic.of..']
xSub_sum$India <- xSub[, 'India..']
xSub_sum$EU_Switz <- xSub[, 'Euro.area..'] + xSub[, 'Switzerland..']
xSub_sum$USA <- xSub[, 'United.States.of.America..']
xSub_sum$NZ <- xSub[, 'New.Zealand..']
xSub_sum$UK <- xSub[, 'United.Kingdom..']
xSub_sum$Other <- xSub[, 'Total..Country.of.Destination...'] - apply(xSub_sum[, 2:10], 1, sum)
xSub_sum$Total <- xSub[, 'Total..Country.of.Destination...']

xSub_sum <- xtsF(xSub_sum)
xSub_sum3 <- rollapplyr(xSub_sum, 3, sum)[-c(1:2)]
xSub_sum3_yoy <- 100 * (xSub_sum3 / lag(xSub_sum3, 12) - 1)
xSub_sum12 <- rollapplyr(xSub_sum, 12, sum)[-c(1:12), ]
xSub_sum12_melt <- melt(x2df(xSub_sum12), measure.vars = c(2:12))


gp_TradeValDest <- ggplot(data = subset(meltx(xSub_sum12), variable != 'Total'),
                  aes(x = date, y = value/1000, color = variable, fill = variable)) +
                theme_grey() +
                scale_color_manual(values = RApal, guide = 'none') +
                scale_fill_manual(values = RApal, guide = guide_legend(reverse = TRUE)) +
                labs(title = "Aus Exports (AUDbn, 12m sum)") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', position = 'stack')
#
png(file.path(plotPATH, "tradeValDest.png"))
grid.arrange(gp_TradeValDest, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_TradeValDestYoY_ff <- ggplot(data = subset(meltx(xSub_sum3_yoy),
                                           variable %in% c('China_HK', 'Korea', 'India',
                                                           'Japan', 'ASEAN')),
                      aes(x = date, y = value, color = variable,
                          fill = variable)) +
                    theme_grey() +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    scale_color_manual(values = RAPal_5, guide = 'none') +
                    scale_fill_manual(values = RAPal_5, guide = guide_legend(reverse = TRUE)) +
                    labs(title = "Growth of Aus Exports (3m YoY)") +
                    labs(y = NULL, x = NULL) +
                    theme(legend.position = 'right') +
                    theme(legend.title = element_blank()) +
                    geom_line(size = 1.5)
#
png(file.path(plotPATH, "tradeValDestYoY_ff.png"))
grid.arrange(gp_TradeValDestYoY_ff, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_TradeValDestYoY_bf <- ggplot(data = subset(meltx(xSub_sum3_yoy),
                                           variable %in% c('EU_Switz', 'USA', 'NZ',
                                                           'UK', 'Other')),
                      aes(x = date, y = value, color = variable,
                          fill = variable)) +
                    theme_grey() +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    scale_color_manual(values = RAPal_5, guide = 'none') +
                    scale_fill_manual(values = RAPal_5, guide = guide_legend(reverse = TRUE)) +
                    labs(title = "Growth of Aus Exports (3m YoY)") +
                    labs(y = NULL, x = NULL) +
                    theme(legend.position = 'right') +
                    theme(legend.title = element_blank()) +
                    geom_line(size = 1.5)
#
png(file.path(plotPATH, "tradeValDestYoY_bf.png"))
grid.arrange(gp_TradeValDestYoY_bf, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
