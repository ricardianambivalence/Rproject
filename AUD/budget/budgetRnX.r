require(gdata)
require(xts)
require(ggplot2)
require(TTR)
require(reshape2)
require(gridExtra)
require(RColorBrewer)

# colours
RApal <- brewer.pal(10, "RdBu")

## paths stuff
projectPATH <- "~/R/AUD/budget"
dataPATH <- "~/data/aud/budget"
plotPATH <- file.path(projectPATH, "Rpics")

budgetProjs <- file.path(dataPATH, "cthHistRevExp.xlsx")
budgetRnX <- read.xls(budgetProjs, sheet='histBud', as.is=TRUE, header=TRUE)

budgetRnX_melt <- melt(budgetRnX, measure.vars = 2:25)

## To reorder the levels:
## note, if x is not a factor use levels(factor(x))
budgetRnX_melt$variable <- factor(budgetRnX_melt$variable,
                                  levels(budgetRnX_melt$variable)[c(24:1)])

gp_budgetRec <- ggplot(data = subset(budgetRnX_melt, 
                                     variable %in% c('my1213_Rec', 'b1213_Rec',
                                                     'b1112_Rec', 'b1011_Rec', 
                                                     'b0910_Rec', 'b0809_Rec') & date > 2005),
                       aes(x = date, y = value/1000, color = variable)) +
                        scale_colour_brewer(palette="Set1") +
                        labs(title = "Commonwealth Budget forecasts: Revenue (AUDbn)") +
                        labs(y = NULL, x = NULL) +
                        theme(legend.position = 'right') +
                        theme(legend.title = element_blank()) +
                        geom_line(size = 1.3) +
                        coord_trans(y="log")
png(file.path(plotPATH, "gp_budgetRec.png"))
grid.arrange(gp_budgetRec, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_budgetpptRec <- ggplot(data = subset(budgetRnX_melt, 
                                     variable %in% c('my1213_pptRec', 'b1213_pptRec',
                                                     'b1112_pptRec', 'b1011_pptRec', 
                                                     'b0910_pptRec', 'b0809_pptRec') & date > 2005),
                       aes(x = date, y = value/1, color = variable)) +
                        scale_colour_brewer(palette="Set1") +
                        labs(title = "Commonwealth Budget forecasts: Revenue (%GDP)") +
                        labs(y = NULL, x = NULL) +
                        theme(legend.position = 'right') +
                        theme(legend.title = element_blank()) +
                        geom_line(size = 1.3) 
png(file.path(plotPATH, "gp_budgetpptRec.png"))
grid.arrange(gp_budgetpptRec, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_budgetExp <- ggplot(data = subset(budgetRnX_melt, 
                                     variable %in% c('my1213_Exp', 'b1213_Exp',
                                                     'b1112_Exp', 'b1011_Exp', 
                                                     'b0910_Exp', 'b0809_Exp') & date > 2005),
                       aes(x = date, y = value/1000, color = variable)) +
                        scale_colour_brewer(palette="Set1") +
                        labs(title = "Commonwealth Budget forecasts: Payments (AUDbn)") +
                        labs(y = NULL, x = NULL) +
                        theme(legend.position = 'right') +
                        theme(legend.title = element_blank()) +
                        geom_line(size = 1.3) +
                        coord_trans(y="log")
png(file.path(plotPATH, "gp_budgetExp.png"))
grid.arrange(gp_budgetExp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_budgetpptExp <- ggplot(data = subset(budgetRnX_melt, 
                                     variable %in% c('my1213_pptExp', 'b1213_pptExp',
                                                     'b1112_pptExp', 'b1011_pptExp', 
                                                     'b0910_pptExp', 'b0809_pptExp') & date > 2005),
                       aes(x = date, y = value/1, color = variable)) +
                        scale_colour_brewer(palette="Set1") +
                        labs(title = "Commonwealth Budget forecasts: Payments (%GDP)") +
                        labs(y = NULL, x = NULL) +
                        theme(legend.position = 'right') +
                        theme(legend.title = element_blank()) +
                        geom_line(size = 1.3) 
png(file.path(plotPATH, "gp_budgetpptExp.png"))
grid.arrange(gp_budgetpptExp, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
