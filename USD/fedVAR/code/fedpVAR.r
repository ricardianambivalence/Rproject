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
projPATH <- file.path("~/R/usd/fedvar")
codePATH <- file.path(projPATH, "code")
dataPATH <- file.path(projPATH, "data")
plotPATH <- file.path(projPATH, "plot")
# }}}

# data is CF_NAI, cf_fci, adj_cf_fci, unrate, fftarget, upper, lower, 3mTbill
dataNames <- c('CFNAI', 'NFCI', 'ANFCI', 'UNRATE', 'PCEPILFE',
               'DFEDTAR', 'DFEDTARU', 'DFEDTARL', 'WGS3MO'
               )
getSymbols(dataNames,src='FRED', return.class = 'xts')

save(CFNAI, NFCI, ANFCI, UNRATE, PCEPILFE, DFEDTAR, DFEDTARU, DFEDTARL, WGS3MO,
     file = file.path(dataPATH, "fedVARdata.rdata"))
