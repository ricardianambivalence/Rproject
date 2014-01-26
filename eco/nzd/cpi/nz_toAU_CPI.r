require(ggplot2)
require(reshape2)
require(gridExtra)

## paths stuff
projectPATH <- "~/R/NZD/CPI"
plotPATH <- file.path(projectPATH, "Rpics")


anzacCPI
anzacCPI_df <- data.frame(date = index(anzacCPI), coredata(anzacCPI))
anzacCPI_melt <- melt(anzacCPI_df, measure.vars = c(2:7))


gp_CPI <- ggplot(subset(anzacCPI_melt, variable %in% c('nzCPI', 'auCPI')),
                        aes(x=date, y = value, color = variable)) +
                 geom_line() +
                 scale_color_manual(values=c("black", "orange")) +
                 theme_grey() +
                 labs(title = "NZ & Aus CPI: %QoQ") +
                 labs(y = NULL, x = NULL) +
                 theme(legend.position = 'top') +
                 theme(legend.title = element_blank())
png(file.path(plotPATH, "NZ_AUCPI_qq.png"))
grid.arrange(gp_CPI, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_TradeableCPI <- ggplot(subset(anzacCPI_melt, variable %in% c('nzTrade', 'auTrade')),
                        aes(x=date, y = value, color = variable)) +
                 geom_line() +
                 scale_color_manual(values=c("black", "orange")) +
                 theme_grey() +
                 labs(title = "NZ & Aus Tradable CPI: %QoQ") +
                 labs(y = NULL, x = NULL) +
                 theme(legend.position = 'top') +
                 theme(legend.title = element_blank())
png(file.path(plotPATH, "NZ_AU_tradableCPI_qq.png"))
grid.arrange(gp_TradeableCPI, sub = textGrob('www.ricardianambivalence.com'))
dev.off()


gp_NonTradableCPI <- ggplot(subset(anzacCPI_melt, variable %in% c('nzNonTrade', 'auNonTrade')),
                        aes(x=date, y = value, color = variable)) +
                 geom_line() +
                 scale_color_manual(values=c("black", "orange")) +
                 theme_grey() +
                 labs(title = "NZ & Aus Non-Tradable CPI: %QoQ") +
                 labs(y = NULL, x = NULL) +
                 theme(legend.position = 'top') +
                 theme(legend.title = element_blank())
png(file.path(plotPATH, "NZ_AU_nonTradCPI_qq.png"))
grid.arrange(gp_NonTradableCPI, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
