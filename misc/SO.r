require(gtools)
M1 <- list(A=1, B=2, C=3)
M2 <- list(A=4, B=5, C=6)

perms <- t(permutations(3, 2, 1:3))

comboList <- list()
for (i in 1:ncol(perms)) {
    nameString <- paste0(names(M2)[perms[1,i]], names(M1)[perms[2,i]])
    comboList[[nameString]] <- mapply("/", M2[[perms[,i][1]]], M1[[perms[,i][2]]])
}


df1 <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
df2 <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
ldf <- list(df1, df2)

selector <- function(DF, col, row=NULL, worst=FALSE){
    if(!is.null(row)) return(DF[row, col])
    if(!missing("col")) if(col > ncol(DF)) return(NA)
    if(!is.null(row)) if(row > nrow(DF)) return(NA)
    if(worst) {
        tail(DF[,col, drop=F],1)
    } else {
        DF[row, col, drop=FALSE]
    }
}

lapply(ldf, selector, worst=T)
lapply(ldf, selector, col=5, worst=T)

# http://stackoverflow.com/questions/19457679/transforming-long-format-data-to-short-format-by-segmenting-dates-that-include-r

DF <- data.frame(ID = rep(1:5, each=5),
                 Date = as.POSIXct(sample(as.POSIXct(Sys.Date()):as.POSIXct(Sys.Date() + 700), 25),
                                   origin = "1970-01-01"),
                 Fl = sample(1:100, 25),
                 Er = sample(1:100, 25),
                 Cmp = sample(1:50, 25)
                 )

month2Season <- function(x) {
    ifelse(x %in% c(1:2), 'Winter',
           ifelse(x %in% c(3:5), 'Spring',
                  ifelse(x %in% c(8:10), 'Fall',
                         'non')))
}

DF$season <- month2Season(as.POSIXlt(DF$Date)$mon + 1)

# read in test data
rn = read.csv("~/Downloads/long.csv", as.is=TRUE)

# convert strings to dates
rn$Date <- strptime(gsub("/", "-", rn$Date), format="%m-%d-%Y %H:%M")

# get month from date, convert to a factor
rn$Seasons <- factor(month(rn$Date), levels = 1:12)
levels(rn$Seasons) <- list(Winter = c(1:2),
                           Spring = c(3:5),
                           Fall = c(8:10),
                           Other = c(6, 7, 11, 12))

# drop 'Other'
rn <- rn[rn$Seasons != 'Other',]

# SO answer
mydf = read.csv("~/Downloads/long.csv", as.is=TRUE)
## Convert dates to actual date formats
mydf$Date <- strptime(gsub("/", "-", mydf$Date), format="%m-%d-%Y %H:%M")

month <- function(D, base = 0){
    as.POSIXlt(D)$mon + base
}

year <- function(D, base = 1900){
    as.POSIXlt(D)$year + base
}

## Factor the months so we can get the "seasons" that you want
Months <- factor(month(mydf$Date, 1), levels=1:12)
levels(Months) <- list(Fall = c(8:10),
                       Winter = c(1:2),
                       Spring = c(3:5),
                       Other = c(6, 7, 11, 12))
mydf$Seasons <- Months

## Drop the "Other" seasons
mydf <- mydf[!mydf$Seasons == "Other", ]

## Add a "Year" column
mydf$Year <- year(mydf$Date)

## Add a "Times" column
mydf$Times <- as.numeric(ave(as.character(mydf$Seasons),
                             mydf$ID, mydf$Year, FUN = seq_along))

## Load "reshape2" and use `dcast` on just one variable.
##   Repeat for other variables by changing the "value.var"
dcast(mydf, ID ~ Seasons + Times, value.var="Fluency")

#

# example data
df <- data.frame(Site = 1:3,
                 Count1 = 0:2,
                 Count2 = c(0,2,0),
                 Count3 = c(0,3,0),
                 Habitat = c("Forest", "Field", "Field")
                 )

countSplit_2df <- function(DF){
    CountCols <- grep("Count", names(DF))
    HabCol <- grep("Habitat", names(DF))
    for (z in CountCols) {
        assign(paste("df", z, sep="."),
               rbind(df[,c(1, z, HabCol)]),
               envir = .GlobalEnv)
    }
}

countSplit_2df(df)
