# get the weekly MOF data and make some charts

# {{{ clean up
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
# }}}

## {{{ packages and functions
require(gdata)
require(data.table)
require(xts)
require(ggplot2)
require(reshape2)
require(TTR)
require(gridExtra)
require(timsac)
source("~/r/Rhelpers/helperFuncts.r")
# }}}

## {{{ PATH stuff
projectPATH <- "~/r/jpy/flows"
plotPATH <- file.path(projectPATH, "pics")
codePATH <- file.path(projectPATH, "code")
dataPATH <- file.path(projectPATH, "data")
# }}}

# get from web or saved xls?
getWeb <- FALSE

## {{{ get the data
if (getWeb)
{
    MOF_weeklycsv <- "http://www.mof.go.jp/international_policy/reference/itn_transactions_in_securities/week.csv"
#
    MOF_data <- read.csv(MOF_weeklycsv,
                         header = FALSE,
                         stringsAsFactors = FALSE,
                         fileEncoding = 'latin1',
                         skip = 16
                         )
#
    MOF_data <- MOF_data[, 1:23]
    MOF_data[, 1] <- seq(as.Date("2005-01-08"), by = 'week', length.out = nrow(MOF_data))
    MOF_data[, -1] <- as.data.frame(lapply(MOF_data[,-1],  # convert to numbers
        function(d) type.convert(gsub(d, pattern=",", replace=""))))
#
    names(MOF_data) <- c('date',
                         'JIA_eq.sales', 'JIA_eq.purch', 'JIA_eq.net',
                         'JIA_bond.sales', 'JIA_bond.purch', 'JIA_bond.net',
                         'JIA_eqBond.subTtl',
                         'JIA_mmkt.sales', 'JIA_mmkt.purch', 'JIA_mmkt.net',
                         'JIA_total',
                         'FIJ_eq.purch', 'FIJ_eq.sales', 'FIJ_eq.net',
                         'FIJ_bonds.purch', 'FIJ_bonds.sales', 'FIJ_bonds.net',
                         'FIJ_eqBond.subTtl',
                         'FIJ_mmkt.purch', 'FIJ_mmkt.sales', 'FIJ_mmkt.net',
                         'FIJ_total'
                         )
    MOF_data[, 2:23] <- MOF_data[, 2:23] / 10 # native unit is Y100m => converted to Ybn
    save(MOF_data, file = file.path(dataPATH, "weeklyMOF.rdata"))

# +ve net = Japan money inflows to JPY => +ve JIA net is sales > purch: +FIJ is purch > sales
} else {
    load(file.path(dataPATH, "weeklyMOF.rdata"))
}

# }}}

## {{{ PLOTS

gp_MOFnet <- ggplot(data = MOF_data)
