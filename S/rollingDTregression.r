require(data.table)
set.seed(1)
x  <- rnorm(1000)
DT <- data.table(x)
DTin <- data.table(x)

lagDT <- function(DTin, varname, l=5) {
    i = 0
    while ( i < l){
        expr <- parse(text =
                  paste0(varname, '_L', (i+1),
                     ':= c(rep(NA, (1+i)),', varname, '[-((length(',     varname, ') - i):length(', varname, '))])'
                 )
              )
    DTin[, eval(expr)]
    i <- i + 1
    }
    return(DTin)
}

rollRegDT <- function(DTin, varname, k=20, l=5) {
    adj <- k + l - 1
    .x <- 1:(nrow(DTin)-adj)
    DTin[, int:=1]
    dtReg <- function(dd) coef(lm.fit(y=dd[-c(1:l),1], x=dd[-c(1:l),-1]))
    eleNum <- nrow(DTin)*(l+1)
    outMatx <- matrix(rep(NA, eleNum), ncol = (l+1))
    colnames(outMatx) <- c('intercept', 'L1', 'L2', 'L3', 'L4', 'L5')
    for (i in .x){
        dt_m <- as.matrix(lagDT(DTin[i:(i+adj), ], varname, l))
        outMatx[(i+(adj)),] <- dtReg(dt_m)
    }
    return(outMatx)
}

rollCoef <- rollRegDT(DT, varname='x')
