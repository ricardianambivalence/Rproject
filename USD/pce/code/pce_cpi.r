# set up environment
rm(list=ls()); gc()
Sys.setenv(TZ = 'UTC')
#
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

# {{{ get dallas fed TM PCE
getDF = TRUE

if(getDF)
{
    # get trimmed mean PCE data -- from the Dallas Fed
    TMpceURL <- "http://www.dallasfed.org/assets/documents/research/pce/pce_hist.xls"
    TMpce_df <- read.xls(TMpceURL, skip=2, as.is=TRUE)
    names(TMpce_df) <- c('data', 'AR1m', 'AR6m', 'AR12m')

    options(warn=-1)
    TMpce_df[,(2:ncol(TMpce_df))] <- sapply(TMpce_df[,(2:(ncol(TMpce_df)))], as.numeric)
    options(warn=0)

    tt <- as.Date(paste("01-", TMpce_df[,1], sep = ""), format = "%d-%b-%y")
    xmd <- xts(TMpce_df[,-c(1)], order.by=tt)
    xmd$AR3m <- SMA(xmd$AR1m, n=3)

    save(xmd, file = file.path(dataPATH, "US_tmPCE.rdata"))
} else {
    load(file.path(dataPATH, "US_tmPCE.rdata"))
}
# }}}

# {{{ get data from Clevaland Fed

getCF = TRUE

# the URLs ...
if (getCF)
{
    CF_cpi <- "http://www.clevelandfed.org/research/inflation/cfincludes/cpi.xls"
    CF_ccpi <- "http://www.clevelandfed.org/research/inflation/cfincludes/core.xls"
    CF_mcpi <- "http://www.clevelandfed.org/research/inflation/cfincludes/mcpi_revised.xls"
    CF_tmcpi <- "http://www.clevelandfed.org/research/inflation/cfincludes/trim_revised.xls"

    CF_cpi_x <- readClvFed(CF_cpi, LineSkip = 1)
    names(CF_cpi_x) <- c('index', 'AR1m')
    CF_ccpi_x <- readClvFed(CF_ccpi)
    names(CF_ccpi_x) <- c('index', 'AR1m')
    CF_mcpi_x <- readClvFed(CF_mcpi, LineSkip = 1, dateType = 'Y-b-d')
    names(CF_mcpi_x) <- c('MoMppt', 'm1AR')
    CF_tmcpi_x <- readClvFed(CF_tmcpi, LineSkip = 1, dateType = 'Y-b-d')[, 1:2]
    names(CF_tmcpi_x) <- c('MoMppt', 'm1AR')

    save(CF_cpi_x, CF_ccpi_x, CF_mcpi_x, CF_tmcpi_x, file = file.path(dataPATH, 'US_CPI.rdata'))

} else {
    load(file.path(dataPATH, 'US_CPI.rdata'))
}

# }}}

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
