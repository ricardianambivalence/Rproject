
# {{{ packages and functions
require(ggplot2)
require(reshape2)
require(gridExtra)
require(xts)
source('~/R/Rhelpers/helperFuncts.r')
source('~/R/Rhelpers/RAcolorpal.r')
# }}}

# {{{ PATHS
projectPATH <- "~/R/aud/bop/tot/"
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")
plotPATH <- file.path(projectPATH, "plot")
# }}}

# get data -- beef it up to get data from abs / rba later ...
load(file.path(dataPATH, "tot_CPX.rdata"))

# {{{ plots

gp_totLvl <- ggplot(subset(meltx(totx), variable %in% 'TermsOfTrade'),
                    aes(x = date, y = value)) +
                labs(y = NULL, x = NULL) +
                labs(title = "Aus. Terms of Trade (NSA): level and Annual Ave (%YoY)") +
                geom_line(size = 1.2) +
                geom_vline(xintercept = as.numeric(as.POSIXct("2012-12-31")),
                           linetype=4, col = 'red')
#
gp_annAveYoY <- ggplot(subset(meltx(totx), variable %in% 'tot_annAve_YoY'),
                aes(x = date, y = value)) +
                labs(y = NULL, x = NULL) +
                labs(title = NULL) +
                theme(legend.position = 'none') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', color = 'blue', fill = 'blue') +
                geom_vline(xintercept = as.numeric(as.POSIXct("2012-12-31")),
                           linetype=4, col = 'red')

png(file.path(plotPATH, "tot_twin.png"))
grid.arrange(gp_totLvl, gp_annAveYoY, heights = c(2/3, 1/3),
     sub = textGrob("www.ricardianambivalence.com", x=1, hjust=1, vjust=0))
dev.off()


gp_rbaCPxLvl <- ggplot(subset(meltx(rbacpxx['19840601::20130430']), variable %in% 'RBA_CmdPxIdx'),
                    aes(x = date, y = value)) +
                labs(y = NULL, x = NULL) +
                labs(title = "RBA AUD$ Export Prices (NSA): level and Annual Ave (%YoY)") +
                geom_line(size = 1.2)
#
gp_RBannAveYoY <- ggplot(subset(meltx(rbacpxx['19840601::20130430']), variable %in% 'RBA_CPx_aaYoY'),
                aes(x = date, y = value)) +
                labs(y = NULL, x = NULL) +
                labs(title = NULL) +
                theme(legend.position = 'none') +
                theme(legend.title = element_blank()) +
                geom_bar(stat = 'identity', color = 'blue', fill = 'blue')

png(file.path(plotPATH, "rba_twin.png"))
grid.arrange(gp_rbaCPxLvl, gp_RBannAveYoY, heights = c(2/3, 1/3),
     sub = textGrob("www.ricardianambivalence.com", x=1, hjust=1, vjust=0))
dev.off()

# }}}
