## this file is designed to be the control spot for the RV analysis
## a flag in the main sheet indicates which is being used, and this 
## sheet controls what's done inside the code

## nodes are only the things that go into the PCA
## bbgSecurities go into the database - the things we keep updated

# AUD irs map
AUD_IRS_nodes <- c("ADSWAP1", "ADSWAP2", "ADSWAP3", "ADSWAP4", "ADSWAP5", "ADSWAP7", "ADSWAP10",
                   "ADSWAP12", "ADSWAP15") 
AUD_IRS_bbgUpdate <- TRUE
AUD_IRS_bbgSecurities <- c("RBACTRD Index", "BBSW1M Index", "BBSW3M Index", "BBSW6M Index", 
                           "ADSWAP1Q IAUS Curncy", "ADSWAP2Q IAUS Curncy", "ADSWAP3Q IAUS Curncy", 
                           "ADSWAP4 IAUS Curncy", "ADSWAP5 IAUS Curncy", "ADSWAP7 IAUS Curncy", 
                           "ADSWAP10 IAUS Curncy", "ADSWAP12 IAUS Curncy", "ADSWAP15 IAUS Curncy", 
                           "ADSWAP20 IAUS Curncy", "ADSWAP25 IAUS Curncy", "ADSWAP30 IAUS Curncy", 
                           "ADBBCF1 IAUS Curncy", "ADBBCF2 IAUS Curncy", "ADBBCF3 IAUS Curncy", 
                           "ADBS1 IAUS Curncy", "ADBS2 IAUS Curncy", "ADBS3 IAUS Curncy", "ADBS4 IAUS Curncy",
                           "ADBS5 IAUS Curncy", "ADBS7 IAUS Curncy", "ADBS10 IAUS Curncy", "ADBS12 IAUS Curncy", 
                           "ADBS15 IAUS Curncy", "ADBS20 IAUS Curncy", "ADBS25 IAUS Curncy", "ADBS30 IAUS Curncy")
AUD_IRS_bbgUnstackKey <- "ADSWAP3Q.IAUS.Curncy"
AUD_IRS_DB <- "S:/Rates Research/autodata/mkt data/IRS.mdb"
AUD_IRS_table <- "AUD"
AUD_IRS_startDate <- as.Date("2009-01-01")
AUD_IRS_list <- list('nodes' = AUD_IRS_nodes, 'bbgUpdate' = AUD_IRS_bbgUpdate, 'bbgSecurities' = AUD_IRS_bbgSecurities,
                     'unstackKey' = AUD_IRS_bbgUnstackKey, 'db' = AUD_IRS_DB, 'dbTable' = AUD_IRS_table, 
                     'startDate' = AUD_IRS_startDate)

# a list of Australian Bond ISINs -- note left out bonds sub-1yr
AUD_Bond_nodes <- c("AU3TB0000028", "AU3TB0000085", "AU0000XCLWI3", "AU3TB0000119", "AU3TB0000077", "AU300TB01208", 
                  "AU3TB0000127", "AU3TB0000093", "AU300TB01224", "AU3TB0000036", "AU0000XCLWM5", "AU3TB0000051", 
                  "AU3TB0000101", "AU3TB0000143", "AU3TB0000135", "AU3TB0000150")
AUD_Bond_bbgUpdate <- FALSE
AUD_Bond_DB <- "Y:/Desktools/Aussie Trading/Sheets/AussieDB/SimpleQuery_daillyHoldings.mdb"
AUD_Bond_table <- "Theos_Govandsemi"
AUD_Bond_startDate <- as.Date("2006-01-01")
AUD_Bond_list <- list('nodes' = AUD_Bond_nodes, 'bbgUpdate' = AUD_Bond_bbgUpdate, 'db' = AUD_Bond_DB, 
                      'dbTable' = AUD_Bond_table, 'startDate' = AUD_Bond_startDate)

# NZD irs map                  
NZD_IRS_nodes <- c("NDSWAP1", "NDSWAP2", "NDSWAP3", "NDSWAP4", "NDSWAP5", "NDSWAP7", "NDSWAP10")
NZD_IRS_bbgUpdate <- TRUE
NZD_IRS_bbgSecurities <- c("NZOCRS Index", "NFIX1FRA Index", "NFIX3FRA Index", "NFIX6FRA Index",
                           "NDSWAP1 ICPN Curncy", "NDSWAP2 ICPN Curncy", "NDSWAP3 ICPN Curncy", 
                           "NDSWAP4 ICPN Curncy", "NDSWAP5 ICPN Curncy", "NDSWAP7 ICPN Curncy", 
                           "NDSWAP10 ICPN Curncy", "NDSWAP12 ICPN Curncy", "NDSWAP15 ICPN Curncy")
NZD_IRS_bbgUnstackKey <- "NDSWAP2.ICPN.Curncy"
NZD_IRS_DB <- "S:/Rates Research/autodata/mkt data/IRS.mdb" 
NZD_IRS_table <- "NZD"
NZD_IRS_startDate <- as.Date("1999-11-01")
NZD_IRS_list <- list('nodes' = NZD_IRS_nodes, 'bbgUpdate' = NZD_IRS_bbgUpdate, 'bbgSecurities' = NZD_IRS_bbgSecurities,
                     'unstackKey' = NZD_IRS_bbgUnstackKey, 'db' = NZD_IRS_DB, 'dbTable' = NZD_IRS_table, 
                     'startDate' = NZD_IRS_startDate)

# USD irs map
USD_IRS_nodes <- c("USSW1", "USSW2", "USSW3", "USSW4", "USSW5", "USSW6", "USSW7", "USSW8", "USSW9",
                   "USSW10", "USSW12", "USSW15", "USSW20", "USSW25", "USSW30")
USD_IRS_bbgUpdate <- TRUE
USD_IRS_bbgSecurities <- c("FDTR Index", "FEDL01 Index", "US0001M Index", "US0003M Index", "US0006M Index", 
                           "USSW1 BGCU Curncy", "USSW2 ICUS Curncy", "USSW3 ICUS Curncy", "USSW4 ICUS Curncy", 
                           "USSW5 ICUS Curncy", "USSW6 ICUS Curncy", "USSW7 ICUS Curncy", 
                           "USSW8 ICUS Curncy", "USSW9 ICUS Curncy", "USSW10 ICUS Curncy", 
                           "USSW11 ICUS Curncy", "USSW12 ICUS Curncy",  "USSW15 ICUS Curncy", 
                           "USSW20 ICUS Curncy", "USSW25 ICUS Curncy", "USSW30 ICUS Curncy")
USD_IRS_bbgUnstackKey <- "USSW2.ICUS.Curncy"
USD_IRS_DB <- "S:/Rates Research/autodata/mkt data/IRS.mdb" 
USD_IRS_table <- "USD"
USD_IRS_startDate <- as.Date("1997-01-02")
USD_IRS_list <- list('nodes' = USD_IRS_nodes, 'bbgUpdate' = USD_IRS_bbgUpdate, "bbgSecurities" = USD_IRS_bbgSecurities, 
                     'unstackKey' = USD_IRS_bbgUnstackKey, "db" = USD_IRS_DB, "dbTable" = USD_IRS_table, 
                     'startDate' = USD_IRS_startDate)

# CAD irs map
CAD_IRS_nodes = c("CDSW1", "CDSW2", "CDSW3", "CDSW4", "CDSW5", "CDSW7", 
                  "CDSW10", "CDSW12", "CDSW15", "CDSW20", "CDSW25", "CDSW30")
CAD_IRS_bbgUpdate <- TRUE
CAD_IRS_bbgSecurities <- c("CABROVER Index", "CDSW1 Curncy", "CDSW2 ICPL Curncy", "CDSW3 ICPL Curncy", 
                           "CDSW4 ICPL Curncy", "CDSW5 ICPL Curncy", "CDSW7 ICPL Curncy", 
                           "CDSW10 ICPL Curncy", "CDSW12 ICPL Curncy", "CDSW15 ICPL Curncy", 
                           "CDSW20 ICPL Curncy", "CDSW25 ICPL Curncy", "CDSW30 ICPL Curncy")
CAD_IRS_bbgUnstackKey <- "CDSW2.ICPL.Curncy"
CAD_IRS_DB <- "S:/Rates Research/autodata/mkt data/IRS.mdb" 
CAD_IRS_table <- "CAD"
CAD_IRS_startDate <- as.Date("1997-05-01")
CAD_IRS_list <- list('nodes' = CAD_IRS_nodes, 'bbgUpdate' = CAD_IRS_bbgUpdate, 
                     "bbgSecurities" = CAD_IRS_bbgSecurities,
                     'unstackKey' = CAD_IRS_bbgUnstackKey, 
                     "db" = CAD_IRS_DB, "dbTable" = CAD_IRS_table, 
                     'startDate' = CAD_IRS_startDate)

mktMap <- list('audIRS' = AUD_IRS_list, 
               'audBond' = AUD_Bond_list, 
               'nzdIRS' = NZD_IRS_list, 
               'usdIRS' = USD_IRS_list, 
               'cadIRS' = CAD_IRS_list)
