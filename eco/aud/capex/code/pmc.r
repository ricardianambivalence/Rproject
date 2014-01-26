#{{{set-up Packs and Funs
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
#packages and functions
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(quantmod)
source("~/R/Rhelpers/helperFuncts.r")
source("~/R/Rhelpers/RAcolorpal.r")
# }}}

# {{{ PATHstuff
projPATH <- file.path("~/R/AUD/capex")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}

getABS <- TRUE

if(getABS) {
    pmcT1 <- "http://abs.gov.au/ausstats/ABS@Archive.nsf/log?openagent&8301_monthly.xls&8301.0&Time%20Series%20Spreadsheet&D19271F2A2C1EB18CA257B7B00125FBA&0&Apr%202013&31.05.2013&Latest"
    pmc_d <- readABS(pmcT1)
    names(pmc_d) <- c('pmc_nsw', 'pmc_vic', 'pmc_qld', 'pmc_sa', 'pmc_wa', 'pmc_tas',
                      'pmc_nt', 'pmc_act', 'pmc_aus')
    pmc_d <- pmc_d[, c(9, 1:8)]

    save(pmc_d, file = file.path(dataPATH, "pmc.rdata"))
} else {
    load(file.path(dataPATH, "pmc.rdata"))
}

pmc_SA <- mj_SAmat_m(pmc_d)
pmc_trend <- mj_SAmat_m(pmc_d, outGet = 'trend')


gg_pmcAuNV <- ggplot() +
            geom_line(data = subset(meltx(pmc_trend), variable %in% c('pmc_aus', 'pmc_nsw', 'pmc_vic')),
                      aes(x = date, y = value, fill = variable), size = 1.5) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(meltx(pmc_SA), variable %in% c('pmc_aus', 'pmc_nsw', 'pmc_vic')),
                      aes(x = date, y = value, fill = variable), size = 0.5, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Pre-Mix Concrete: '000 m^3")
png(file.path(plotPATH, "pmc_AuNV.png"))
grid.arrange(gg_pmcAuNV, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gg_pmcQWnt <- ggplot() +
            geom_line(data = subset(meltx(pmc_trend), variable %in% c('pmc_qld', 'pmc_nt', 'pmc_wa')),
                      aes(x = date, y = value, fill = variable), size = 1.5) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(meltx(pmc_SA), variable %in% c('pmc_qld', 'pmc_nt', 'pmc_wa')),
                      aes(x = date, y = value, fill = variable), size = 0.5, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Pre-Mix Concrete: '000 m^3")
png(file.path(plotPATH, "pmc_QWnt.png"))
grid.arrange(gg_pmcQWnt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gg_pmcSTact <- ggplot() +
            geom_line(data = subset(meltx(pmc_trend), variable %in% c('pmc_sa', 'pmc_tas', 'pmc_act')),
                      aes(x = date, y = value, fill = variable), size = 1.5) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(meltx(pmc_SA), variable %in% c('pmc_sa', 'pmc_tas', 'pmc_act')),
                      aes(x = date, y = value, fill = variable), size = 0.5, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Pre-Mix Concrete: '000 m^3")
png(file.path(plotPATH, "pmc_STact.png"))
grid.arrange(gg_pmcSTact, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
