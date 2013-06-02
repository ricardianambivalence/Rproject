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

    save(pmc_d, file = file.path(dataPATH, "pmc.rdata"))
} else {
    load(file.path(dataPATH, "pmc.rdata"))
}

pmc_SA <- mj_SAmat_m(pmc_d)
pmc_trend <- mj_SAmat_m(pmc_d, outGet = 'trend')
