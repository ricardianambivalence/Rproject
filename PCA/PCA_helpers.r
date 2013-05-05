# MCJ
# S:\Rates Research\R_share\MCJ\nR\PCA_helpers.r
# to help with S:\Rates Research\R_share\MCJ\nR\PCA_RV.r

colADD <- function(x, w=NULL) 
{ 
	Dfww <- list(w1 = 1, w2 = c(-1, 1), w3 = c(-1, 2, -1), w4 = c(-1, 1, -1, 1),
		     w5 = c(-2, 3, -2, 3, -2))
	if (is.null(w)) w  <- Dfww[[length(x)]]
  #   print(w); print(mode(w)); print(class(w))
	Reduce('+', mapply('*', x, e2=w, SIMPLIFY = FALSE))
}

getTrades <- function(dd, Maxleg=3, stdDevs=NULL, eVectorMatx=NULL)
{
	if (!(Maxleg %in% c(1:5))) stop("invalid Maxleg; must be in [1,5]")
  w <- NULL
	tradeList <- list()
	for (i in 1:Maxleg)
  {
		tradeLeg <- paste0('legs', i)
		tradeLegsList <- combn(names(dd), i, function(x) dd[x], simplify = FALSE)
		nameMtx <- combn(names(dd), i)
		names(tradeLegsList) <- apply(nameMtx, MARGIN=2, FUN=function(x) paste(x, collapse='*'))
    smashedTrades <- strsplit(names(tradeLegsList), "\\*")
		if (!is.null(stdDevs)) w <- relVols(smashedTrades, stdDevs, eVectorMatx)

    outlist <- list()
    for ( i in 1:length(tradeLegsList))
    {
      outlist[[i]] <- colADD(tradeLegsList[[i]], w[[i]])
    }
    names(outlist) <- names(tradeLegsList)
    tradeList[[tradeLeg]] <- outlist
  }
	return(tradeList)
}

mtxNameSticker <- function(mtx, prepend = NULL, MARGIN=2)
{
	if (MARGIN == 1) max <- nrow(mtx) else
		max  <- ncol(mtx)
	if (is.null(prepend)) if (MARGIN == 2) prepend <- 'C' else
		prepend <- 'R'
	if (length(prepend) == 1) prepend <- paste0(prepend, 1:dim(mtx)[[MARGIN]]) 
	dimnames(mtx)[[MARGIN]] <- seq(from=1, by=1, length.out=dim(mtx)[[MARGIN]]) 
	for (i in 1:max){
		dimnames(mtx)[[MARGIN]][i] <- prepend[i]
	}
	return(mtx)
}

# TODO: there is similarity here, so the methods may be combined
# Also, add a switch for the factors we will hedge non-default 
# default to H = F1 in curve, H = c(F1, F2) in fly, and H = c(F1, F2, F3) for condors
# finally add the 5 leg trade method
relVols <- function(tradeList, stdDevs, eVectorMtx)
{
  # the second leg is the numeraire
  switch(length(tradeList[[1]]), 
         return(NULL), 
         curveWeights(tradeList, stdDevs, eVectorMtx), 
         flyWeights(tradeList, stdDevs, eVectorMtx),
         condorWeights(tradeList, stdDevs, eVectorMtx),
         return(NULL)
         )
}


# the two leg case
curveWeights <- function(tradeList, stdDevs, eVectorMtx)
{
	ffs <- c('F1')
	relVolList <- list()
	for (i in 1:length(tradeList)) {
		tradeSD <- stdDevs[tradeList[[i]]]
    tSDratio <- tradeSD[2]/tradeSD

		Ti <- tradeList[[i]]
		W <- t(eVectorMtx)[ffs, Ti[c(1)], drop=FALSE]
		g  <- t(eVectorMtx)[ffs, Ti[2], drop=FALSE]
		x <- solve(W, g)
		relVolList[[i]] <- c(-1*x[1]*tSDratio[1], tSDratio[2])
	}
	return(relVolList)
    	
}

# the three leg case
flyWeights <- function(tradeList, stdDevs, eVectorMtx)
{
	ffs <- c('F1', 'F2')
	relVolList <- list()
	for (i in 1:length(tradeList)) {
		tradeSD <- stdDevs[tradeList[[i]]]
    tSDratio <- tradeSD[2]/tradeSD

		Ti <- tradeList[[i]]
		W <- t(eVectorMtx)[ffs, Ti[c(1,3)], drop=FALSE]
		g  <- t(eVectorMtx)[ffs, Ti[2], drop=FALSE]
		x <- solve(W, g)
		relVolList[[i]] <- c(-1*x[1]*tSDratio[1], tSDratio[2], -1*x[2]*tSDratio[3])
	}
	return(relVolList)
    	
}

# the four leg case
condorWeights <- function(tradeList, stdDevs, eVectorMtx)
{
	ffs <- c('F1', 'F2', 'F3')
	relVolList <- list()
	for (i in 1:length(tradeList)) {
		tradeSD <- stdDevs[tradeList[[i]]]
    tSDratio <- tradeSD[2]/tradeSD

		Ti <- tradeList[[i]]
		W <- t(eVectorMtx)[ffs, Ti[c(1,3,4)], drop=FALSE]
		g  <- t(eVectorMtx)[ffs, Ti[2], drop=FALSE]
		x <- solve(W, g)
		relVolList[[i]] <- c(-1*x[1]*tSDratio[1], tSDratio[2], -1*x[2]*tSDratio[3], 
                         -1*x[3]*tSDratio[4])
	}
	return(relVolList)
    	
}
