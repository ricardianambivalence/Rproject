require(ggplot2)
require(reshape2)
require(xts)
require(gridExtra)
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")

## {{{ PATHS
projectPATH <- "~/R/USD/pce/"
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")
plotPATH <- file.path(projectPATH, "Rpics")
# }}}

load(file.path(dataPATH, 'usCPI.rdata'))

usInflx <- xtsF(dd)

usInflx_3m <- rollapplyr(usInflx, 3, colMeans)
usInflx_6m <- rollapplyr(usInflx, 6, colMeans)
usInflx_12m <- rollapplyr(usInflx, 12, colMeans)


gp_3minfl <- ggplot(data = meltx(usInflx_3m['2001::']),
                    aes(x=date, y=value, colour=variable)) +
                geom_line(size = 1.05) +
                scale_colour_manual(values = RAPal_3) +
                labs(title = "Measures of US Inflation: 3mma AR") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank())
png(file.path(plotPATH, "gp_3mUSinfl.png"))
grid.arrange(gp_3minfl, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_6minfl <- ggplot(data = meltx(usInflx_6m['2001::']),
                    aes(x=date, y=value, colour=variable)) +
                geom_line(size = 1.05) +
                scale_colour_manual(values = RAPal_3) +
                labs(title = "Measures of US Inflation: 6mma AR") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank())
png(file.path(plotPATH, "gp_6mUSinfl.png"))
grid.arrange(gp_6minfl, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_12minfl <- ggplot(data = meltx(usInflx_12m['2001::']),
                    aes(x=date, y=value, colour=variable)) +
                geom_line(size = 1.05) +
                scale_colour_manual(values = RAPal_3) +
                labs(title = "Measures of US Inflation: 12mma AR") +
                labs(y = NULL, x = NULL) +
                theme(legend.position = 'right') +
                theme(legend.title = element_blank())
png(file.path(plotPATH, "gp_12mUSinfl.png"))
grid.arrange(gp_12minfl, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
