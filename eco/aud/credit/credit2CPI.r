rm(list=ls())
Sys.setenv(TZ='GMT')

require(xts)
require(gdata)
# require(quantmod)
# require(timsac)

mj_lastof <- function(tt){
    ot <- as.POSIXlt(tt)
    ot$mon <- ot$mon + 1
    ot$mday <- 1
    ot <- ot - (1*60*60*24)
    return(as.POSIXct(ot))
}

# RBA credit data
RBA_Credit_xls <- "http://www.rba.gov.au/statistics/tables/xls/d01hist.xls"
R_Credit_df <- read.xls(RBA_Credit_xls, skip=8, as.is=TRUE, header=TRUE)
R_Credit_df <- R_Credit_df[,-18]

R_Credit_df <- R_Credit_df[complete.cases(R_Credit_df),]
names(R_Credit_df) <- c("period", "housingM", "housingY", "ooHousingM", "ooHousingY", "invHousingM",
                       "invHousingY", "persM", "persY", "busM", "busY", "allM", "allY", "m3M", "m3Y",
                       "broadM", "broadY")

tt <- as.Date(paste("01-", R_Credit_df[,1], sep = ""), format = "%d-%b-%Y")
ttl <- mj_lastof(tt)
credit_x <- xts(R_Credit_df[,-1], order.by=ttl)
creditQ_x <- apply.quarterly(credit_x, mean)
# a hack to extend the date to be a quarter
index(creditQ_x)[nrow(creditQ_x)] <- as.POSIXct("2012-12-31")

# RBA CPI data
RBA_CPI_xls <- "http://www.rba.gov.au/statistics/tables/xls/g01hist.xls"
R_CPI_df <- read.xls(RBA_CPI_xls, skip=8, as.is=TRUE, header=TRUE)
R_CPI_df <- R_CPI_df[,c(-25:-22)]

R_CPI_df <- R_CPI_df[complete.cases(R_CPI_df),]
names(R_CPI_df) <- c("period", "allY", "allY_xInT", "allY_sa", "allY_xVol",
                     "mktGoodsY_xVol", "mktServsY_xVol", "mktAllY_xVol",
                     "consY_ChainIdx", "wmY", "tmY", "allQ", "allQ_xInT",
                     "allQ_sa", "allQ_xVol", "mktGoodsQ_xVol", "mktServsQ_xVol",
                     "mktAllQ_xVol", "consQ_ChainIdx", "wmQ", "tmQ")

tt <- as.Date(paste("01-", R_CPI_df[,1], sep = ""), format = "%d-%b-%Y")
ttl <- mj_lastof(tt)
CPI_x <- xts(R_CPI_df[,-1], order.by=ttl)

# the econometrics!
CPIframe <- merge(CPI_x$tmY, lag(CPI_x$tmY, 1), lag(CPI_x$tmY, 2), lag(CPI_x$tmY, 3),
                  lag(CPI_x$tmY, 4))['1992::']

ARmod <- lm(tmY ~ tmY.1 + tmY.2 + tmY.3 + tmY.4, data=CPIframe)

# test down using the BIC
step(ARmod, k = log(nrow(CPIframe)))

ARmod.t <- lm(tmY ~ tmY.1 + tmY.2, data=CPIframe)

mj_poos <- function(Xmatx, Yvec=NULL, bmks= 'ALL', st.date = NULL, e.date = NULL)
{
    if (Yvec == NULL) {Yvec <- Xmatx[,1]}
    if (st.date == NULL) {
        if (nrow(Yvec) < 21) stop ('if st.date is blank, Xmatx must have more than 21 rows')
        startRow <- 21
    } else {
        startRow <- which(index(Yvec) == as.POSIXct(st.date))
    }

    bmkRows <- nrow(Yvec) - startRow
    bmkFrame <- matrix(rep(NA, bmkRows))

    # four benchmark models: RW, RWwD, AR(1), AR(1)+t
    ll <- list(m1 = "tmY ~ tmY.1", m2 = "tmY ~ tmY.1 + tmY.2", m3 = "tmY ~ tmY.1 + tmY.2 + tmY.3")
    for (mod in ll){
        results <- do.call("fastLm", args = list(as.formula(mod), data=CPIframe))
        print(results$coefficients)
    }


    # now it needs to iterate across the rows and to save the forecast each step
}

flm <- fastLmPure( cbind(1, log(trees$Girth)), log(trees$Volume) )

 update(y ~ x,    ~ . + x2) #> y ~ x + x2

