
##seasonally adjust vectors of irregular NA lengths - MJs function
mj_SAmat_m <- function(X, PRD = 12, adj = 'adjust', to=1, BUG = FALSE)
{
    SAmat <- X
    for (i in seq_along(names(X))) {
        #         cat(names(X)[i], "\n")
        begin <- min(which(!is.na(X[(1:(nrow(X)-12)),i])))
        beginYr <- as.POSIXlt(index(X[begin]))$year + 1900
        beginMth <- as.POSIXlt(index(X[begin]))$mon + 1
        end <- nrow(X) - sum(is.na(tail(X[,i],12)))
        dat <- X[begin:end,i] 
        if(BUG) print(paste('doing', names(X)[i])) # for bug testing
        sa <- baysea(coredata(dat), period=PRD, year=beginYr, month=beginMth, shift=1, plot=FALSE, out=0, zersum=1, trend.order=to)
        firstNA <- rep(NA, (begin-1))
        lastNA <- rep(NA, (nrow(X)-end))
        SAmat[,i] <- c(firstNA, get(adj, sa), lastNA)
    }
    return(SAmat)     
}

##send an xts object of some seasonally adjusted data, some non-seasonally adjusted data -
##flag the non seasonally adjusted by adding suffix to vector name of "_nsa" 
##function will seasonally adjust the subset that has "_nsa" and then rename suffix with "(sa)"
## Convention: _nsa is NSA, _t is trend, " (sa)" means we've done it & no _# means SA'd at source
al_subsetting_sa <- function(x, to=1){
    subset_adjusting <- x[,grep("_nsa$", names(x))]
    x[,grep("_nsa$", names(x))] <- mj_SAmat_m(subset_adjusting, to=to)
    x_names <- grep("_nsa$", names(x))
    names(x)[x_names] <- paste(substr(names(x)[x_names], 1, nchar(names(x)[x_names])-4), " (sa)", sep="")
    return(x)
}


##this wrapper looks to see if you have an xts object, if you do, s.a. it.
##if not, look to see if the first column of the dataframe is in POSIXct format,
##and then make it an xts object.
##
##lastly, if you enter some optional arguments, use those as 
al_easySA <- function(x){
    if (class(x)[1] == "xts"){
        x <- al_subsetting_sa(x)
    } else {
        if (class(x[,1])[1] == "POSIXct"){
            x <- xts(x[,-1], order.by=as.Date(x[,1]))
            x <- al_subsetting_sa(x)
        }
    }
    return(x)
}

