# get the ABS file and make some labour market charts
rm(list=ls()); gc()
Sys.setenv(TZ = 'GMT')
#
require(gdata)
require(xts)
require(ggplot2)
require(reshape2)
require(TTR)
require(gridExtra)
require(timsac)

## get from web or saved xls?
getWeb <- FALSE

## PATH stuff
projectPATH <- "~/R/AUD/alm"
plotPATH <- file.path(projectPATH, "Rpics")

load("~/data/aud/alm/6291_ltUnemployment.RData")


lt_unploy[,1] <- as.Date(paste0(substr(lt_unploy[,1], 5, 9), "-", substr(lt_unploy[,1], 1, 3), "-01"),
                         format = "%Y-%b-%d")

lt_un_t <- lt_unploy[, c(1:13)]
lt_un_sa <- lt_unploy[, c(1, 14:25)]

names(lt_un_t)[-1] <- substr(names(lt_un_t)[-1], 1, nchar(names(lt_un_t)[-1]) - 2)
names(lt_un_sa)[-1] <- substr(names(lt_un_sa)[-1], 1, nchar(names(lt_un_sa)[-1]) - 3)

lt_unemployT_melt <- melt(lt_un_t, id = 'date')
lt_unemploySA_melt <- melt(lt_un_sa, id = 'date')


gg_ltURratio <- ggplot() +
            geom_line(data = subset(lt_unemployT_melt, variable %in% c('ltUR_m', 'ltUR_f', 'ltUR')),
                      aes(x = date, y = value, fill = variable), size = 1.2) +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(y = NULL, x = NULL) +
            geom_line(data = subset(lt_unemploySA_melt, variable %in% c('ltUR_m', 'ltUR_f', 'ltUR')),
                      aes(x = date, y = value, fill = variable), size = 0.35, color = 'red') +
                      theme_grey() +
                      facet_grid(variable ~ ., scale = 'free_y') +
                      labs(title = "Long term unemployment (%) -- by gender")
png(file.path(plotPATH, "ltURratio_gender.png"))
grid.arrange(gg_ltURratio, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
