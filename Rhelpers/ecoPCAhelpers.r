# make the scores index -- with or without subsetting
makeScore <- function(DF, SUB = NULL, MAP)
{
    if(!is.null(SUB)) { 
        s3mAve <- (DF[,SUB] + lag(DF[,SUB], 1) + lag(DF[,SUB], 2))/3
        binaryWeights <- MAP[match(names(DF[,SUB]), MAP$name), 'apd']
    } else {
        s3mAve <- (DF + lag(DF, 1) + lag(DF, 2))/3
        binaryWeights <- MAP[match(names(DF), MAP$name), 'apd']
    }
    Ds3mAve <- diff(s3mAve)
    Pulse <- ifelse(as.matrix(Ds3mAve) <= 0, -1, 1)
    Score <- xts(Pulse %*% binaryWeights, order.by = index(DF))
    Score3m <- rollapplyr(Score, 3, mean)
    Score6m <- rollapplyr(Score, 6, mean)
    Scores <- cbind(Score, Score3m, Score6m)
    names(Scores) <- c('p1', 'p3', 'p6')
    return(Scores)
}

# Seasonally Adjust and Make stationary
SAandStationary <- function(DF, map, PRD=12)
{
    if(!is.xts(DF)) stop("not an xts object")
    transDF <- DF
    # SA the matching
    lnD_SAcols <- which(map$tran == 'lnD' & map$SA == 'y')
    ln_SAcols <- which(map$tran == 'ln' & map$SA == 'y')
    D_SAcols <- which(map$tran == 'D' & map$SA == 'y')
    lvl_SAcols <- which(map$tran == 'lvl' & map$SA == 'y')
    if(length(lnD_SAcols)) { transDF[, lnD_SAcols] <- exp(mj_SAmat_m(log(DF[, lnD_SAcols]), PRD, to=2))}
    if(length(ln_SAcols)) { transDF[, ln_SAcols] <- exp(mj_SAmat_m(log(DF[, ln_SAcols]), PRD, to=1))}
    if(length(D_SAcols)) { transDF[, D_SAcols] <- mj_SAmat_m(DF[, D_SAcols], PRD, to=2)}
    if(length(lvl_SAcols)) { transDF[, lvl_SAcols] <- mj_SAmat_m(DF[, lvl_SAcols], PRD)}
    # and now make stationary
    lnD_cols <- which(map$tran == 'lnD')
    ln_cols <- which(map$tran == 'ln')
    D_cols <- which(map$tran == 'D')
    if(length(lnD_cols)) { transDF[, lnD_cols] <- diff(transDF[, lnD_cols], log=TRUE)}
    if(length(ln_cols)) { transDF[, ln_cols] <- log(transDF[, ln_cols])}
    if(length(D_cols)) { transDF[, D_cols] <- diff(transDF[, D_cols])}
    # and return the adjusted object
    return(transDF)
}

# merge xts dataframes according to grouping reference IN
xtsMerger <- function(dld, IN)
{
    mergedData <- xts()
    for (i in seq_along(dld[[1]])) {
        matchCols <- which(get(dld[[2]][i])$group %in% IN)
        if ( length (matchCols) == 0) { 
            mergedData <- merge(mergedData, xts())
        } else { 
            mergedData <- merge(mergedData, get(dld[[1]][i])[, matchCols]) }
    }
    return(mergedData)
}

# find the lag length - crit in {'AIC(n)', 'HQ(n)', 'SC(n)', 'FPE(n)'}
findMaxVARLag <- function(varData, firstMax=12, crit = "SC(n)")
{
    maxLag <- firstMax
    repeat {
        lagSelect <- VARselect(varData, lag.max=maxLag)
        if (lagSelect$selection[crit] < maxLag) {
            maxLag <- maxLag - 1L
        } else break
    }
    return(maxLag)
}

# get things from the VAR and place in global env
varSuite <- function(VARframe, dateRange, initMax = 9, infoCrit = "FPE", castAhead = 6, 
                     pSkip = 32)
{
    if (!is.xts(get(VARframe, envir = globalenv()))) stop('data must be an xts object')
    # find the periodicity 
    freq <- xPeriodToMon(get(VARframe, envir = globalenv()))
    # find the optimal lag for the VAR
    assign(paste0(VARframe, ".optLag"),
                  findMaxVARLag(get(VARframe, envir = globalenv())[dateRange],
                            firstMax = initMax, crit = paste0(infoCrit, "(n)")),
           envir = globalenv()
           )
    # generate POOS forecasts by vintage
    assign(paste0(VARframe, ".test", castAhead),
           testVar(get(VARframe, envir = globalenv())[dateRange], freq = freq,
                   skip = pSkip, nAhead = castAhead, Vlag = get(paste0(VARframe, ".optLag")), 
                   IC = infoCrit),
           envir = globalenv()
           )
    # assign output of sum test error 
    assign(paste0('sumTestError.', VARframe),
           errTstVar(get(paste0(VARframe, ".test", castAhead),
                         envir = globalenv()
                         )
                    ),
           envir = globalenv()
           )
    # assign to .global a VAR model with scaled data (best for IRF)
    assign(paste0(VARframe, ".mod"),
           VAR(scale(get(VARframe, envir = globalenv())[dateRange]),
               p = get(paste0(VARframe, ".optLag"), envir = globalenv()),
               ic = infoCrit),
           envir = globalenv()
           )
    # assign to global a VAR model using unscaled inputs (for predict)
    assign(paste0(VARframe, ".mod2"),
           VAR(get(VARframe, envir = globalenv())[dateRange],
               p = get(paste0(VARframe, ".optLag"), envir = globalenv()),
               ic = infoCrit),
           envir = globalenv()
           )
}

# measure POOS performance of a VAR
testVar <- function(dframe, nAhead = 6, IC = 'FPE', skip = NULL, Vlag = 12, RSTmtx = NULL)
{
    # setup
    if(is.null(skip)) { skip <- nrow(dframe) %/% 2 }
    dateList <- index(dframe)
    ttlRows <- length(dateList) + nAhead
    extraDate <- seq(index(dframe[1]), by=Xfreq(dframe), length.out=ttlRows)
    # make extended variables
    testPack <- list()
    for (i in 1:ncol(dframe))
    {
        testPack[[names(dframe)[i]]] <-
            as.xts(cbind(c(coredata(dframe[,i]), rep(NA, nAhead)),
                         matrix(rep(NA, ttlRows * (length(dateList) - skip)),
                                nrow = ttlRows, ncol = (length(dateList) - skip)
                                )
                         ), order.by = extraDate
                   )
    }
    # the test loop
    for (d in (skip+1):length(dateList))
    {
        testName <- paste0('from:', dateList[d]) # need to stick this on later
        if (is.null(RSTmtx))
        {
            optLag <- findMaxVARLag(dframe[1:d,], firstMax= Vlag, crit = paste0(IC, "(n)"))
            mod <- VAR(dframe[1:d,], p = optLag) # we may want to change the IC part later
        } else {
            Vlag <- (ncol(RSTmtx) - 1) / (nrow(RSTmtx))
            mod <- VAR(dframe[1:d,], p = Vlag)
            mod <- restrict(mod, method = 'manual', resmat = RSTmtx)
        }
        poosPred <- predict(mod, n.ahead=nAhead)
        #
        for (v in 1:ncol(dframe)) # loop across by column type
        {
            outvec <- rep(NA, ttlRows) # allocate the full length vector
            outvec[(d + 1):(d + nAhead)] <- poosPred$fcst[[v]][,1] # replace with 4casts
            testPack[[v]][, (d - skip + 1)] <- outvec # one variable at a time
        }
    }
    return(testPack)
}

# works with the above, to sum POOS rmse
errTstVar <- function(testRslt) 
{
    rmsError <- testRslt # copy object to pre-allocate
    for (v in 1:length(testRslt))
    {
        for (i in 2:ncol(testRslt[[v]]))
        {
            rmsError[[v]][,i] <- sqrt((rmsError[[v]][,1] - rmsError[[v]][,i])**2)
        }
        rmsError[['rmsfe']][[names(testRslt)[v]]] <- sum(rmsError[[v]], na.rm=TRUE)
    }
    return(rmsError)
}

# predict VAR model using new data
varsPredictNewData <- function(varsMODEL, varsDATA, projFWD = 1)
{
    varsDATA_pp <- varsDATA  # make a frame to extend
    # make the coeff vectors
    for (i in seq_along(get('varresult', varsMODEL)))
    {
        assign(paste0('modCoeffs_v', i),
               get('varresult', varsMODEL)[[i]]$coeff
               )
    }
    # find max lag length -- from coeff vector
    maxLag <- max(as.numeric(unlist(regmatches(names(modCoeffs_v1),
                                               gregexpr('\\(?[0-9]+',
                                                        names(modCoeffs_v1))))))
    n = 1
    # make an expanded DF so you can %*% using coeff matrix
    while (n < (1+projFWD))
    {
        env4step <- new.env()
        expandedDF <- varsDATA_pp # the df we build up
        for (j in 1:(maxLag-1))
        {
            expandedDF <- merge(expandedDF, lag(varsDATA_pp, j))
        }
        expandedDF$const <- 1
        for (k in seq_along(get('varresult', varsMODEL)))
        {
            assign(paste0('v', k, '_pp'),
                   tail(expandedDF, 1) %*% get(paste0('modCoeffs_v', k)),
                   envir = env4step)
        }
        # join the forecasts together
        vvlist = list() # an empty list to contain the variable names
        for (l in seq_along(get('varresult', varsMODEL)))
        {
            vvlist <- c(vvlist, paste0('v', l, '_pp'))
            l = l + 1
        } # this loop puts the names into the list ... use environments?
        print(vvlist)
        # now put it together
        nextDate <- seq(last(index(varsDATA_pp)), by = 'mon', length.out = 2)[-1]
        newRow <- xts(do.call(cbind, lapply(vvlist, get, envir = env4step)),
                      order.by = nextDate)
        names(newRow) <- names(varsDATA_pp)
        varsDATA_pp <- rbind(varsDATA_pp, newRow)
        rm(expandedDF)
        rm(list = ls(env4step), envir = env4step)
        n = n + 1
    }
    return(varsDATA_pp)
}

# VAR plot function -- POOS spider plots
spiderPOOS <- function(POOStest, series, startYr = 1993, MAINstring = NULL)
{
    startYrFlag <- paste0(startYr, '::')
    plot.zoo(get(series, POOStest)[startYrFlag],
             screen=1,
             col=c(1, rep(8, ncol(get(series, POOStest)-1))),
             las=1,
             lwd = c(3, rep(1, ncol(get(series, POOStest)) - 1)),
             type = c('s', rep('l', ncol(get(series, POOStest)) - 1)),
             main = MAINstring,
             xlab = "",
             ylab = ""
             )
}

