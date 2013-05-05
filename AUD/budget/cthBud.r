require(xts)
require(ggplot2)
require(TTR)
require(reshape2)
require(gridExtra)


## paths stuff
projectPATH <- "~/R/AUD/budget"
plotPATH <- file.path(projectPATH, "Rpics")

# get data
load("~/data/AUD/budget/cthBud.RData")

# add data in reverse chron order -- from http://www.finance.gov.au
datPlus <- data.frame('date' = c(as.Date('2013-02-01'), as.Date('2013-01-01')),
                      'revenue' = c(32761, 31844),
                      'expenditure' = c(29238, 33211),
                      'balance' = c(3523, -1367))

d2 <- rbind(datPlus, dat)

# check it! ... the save
dat <- d2
save(dat, file = "~/data/AUD/budget/cthBud.RData")

# make xts and find the rolling 12m sum
dat_x <- xts(dat[,-1], order.by = dat[,1])
dat_12M <- xts(sapply(dat_x, SMA, 12)*12/1000, order.by = index(dat_x))
dat_12M_growth <- diff(dat_12M[,-3], 12, log=TRUE)*100


dat_12M_melt <- melt( data.frame(date = index(dat_12M),
                                coredata(dat_12M)), measure.vars = c(2:4))

dat_12M_growth_melt <- melt( data.frame(date = index(dat_12M_growth),
                                coredata(dat_12M_growth)), measure.vars = c(2:3))

gp_12mBudget <- ggplot(subset(dat_12M_melt, date > as.Date('2001-01-01')),
                       aes(x = date, y = value, color = variable, fill = variable)) +
                    facet_grid(variable ~ ., scale = 'free_y') +
                    theme_grey() +
                    scale_fill_brewer(palette = 'Set1') +
                    labs(title = "Aus Cth Govt Budget: AUDbn (rolling 12m sum)") +
                    labs(y = NULL, x = NULL) +
                    theme(legend.position = 'null') +
                    theme(legend.title = element_blank()) +
                    geom_bar(stat = 'identity')
png(file.path(plotPATH, "CthBudgetBals.png"))
grid.arrange(gp_12mBudget, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

# figure out how to make this the same colors as the above --> get a pal going!
gp_12m_GBdgt <- ggplot(subset(dat_12M_growth_melt, date > as.Date('2001-01-01')),
                       aes(x = date, y = value, color = variable)) +
                    # facet_grid(variable ~ ., scale = 'free_y') +
                    theme_grey() +
                    scale_fill_brewer(palette = 'Set1') +
                    labs(title = "Aus Cth Govt Budget: growth (rolling 12m sum)") +
                    labs(y = NULL, x = NULL) +
                    theme(legend.position = 'top') +
                    theme(legend.title = element_blank()) +
                    geom_line()
png(file.path(plotPATH, "CthBudgetGrowth.png"))
grid.arrange(gp_12m_GBdgt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
