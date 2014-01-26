require(ggplot2)
require(lattice)
require(reshape2)
require(xts)
require(gridExtra)

## PATHS
projectPATH <- "~/R/aud/cpi/"
plotPATH <- file.path(projectPATH, "pics")
dataPATH <- "~/data/aud/cpi/"

load(file.path(dataPATH, "trimmedMeans.RData"))


# model -- qoq and yoy
modQ <- lm(tmx$AU_TMq ~ lag(tmx$AU_TMq) + lag(tmx$AU_TMq, 2) + tmx$US_TMq)
modY <- lm(tmx$AU_TMy ~ lag(tmx$AU_TMy) + lag(tmx$AU_TMy, 2) +
           tmx$US_TMy + lag(tmx$US_TMy) + lag(tmx$US_TMy, 2))


tmx$predQ <- (modQ$coefficients[1] +
          modQ$coefficients[2] * lag(tmx$AU_TMq) +
          modQ$coefficients[3] * lag(tmx$AU_TMq, 2) +
          modQ$coefficients[4] * tmx$US_TMq)

tmx$predY <- (modY$coefficients[1] +
          modY$coefficients[2] * lag(tmx$AU_TMy) +
          modY$coefficients[3] * lag(tmx$AU_TMy, 2) +
          modY$coefficients[4] * tmx$US_TMy) +
          modY$coefficients[5] * lag(tmx$US_TMy) +
          modY$coefficients[6] * lag(tmx$US_TMy, 2)

# prep
tmx_melt <- melt(data.frame(date = index(tmx), coredata(tmx)),
                 measure.vars = c(2:7))
tmx_melt$variable <- factor(tmx_melt$variable, levels(tmx_melt$variable)[6:1])

gp_tmYXmkt <- ggplot(data = subset(tmx_melt, variable %in% c('AU_TMy', 'US_TMy')),
                     aes(x = date, y = value, color = variable)) +
                     theme_grey() +
                     labs(y = NULL, x = NULL) +
                     labs(title = "Trimmed Mean CPI: %YoY") +
                     theme(legend.title = element_blank()) +
                     geom_line(size = 1.2) +
                     scale_color_manual(values = c('orange', 'blue'))
png(file.path(plotPATH, "YoY_TMXmkt.png"))
grid.arrange(gp_tmYXmkt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()


gp_tmQXmkt <- ggplot(data = subset(tmx_melt, variable %in% c('AU_TMq', 'US_TMq')),
                     aes(x = date, y = value, color = variable)) +
                     theme_grey() +
                     labs(y = NULL, x = NULL) +
                     labs(title = "Trimmed Mean CPI: %QoQ") +
                     theme(legend.title = element_blank()) +
                     geom_line(size = 1.2) +
                     scale_color_manual(values = c('orange', 'blue'))
png(file.path(plotPATH, "YoY_TMQmkt.png"))
grid.arrange(gp_tmQXmkt, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_tmQpred <- ggplot(data = subset(tmx_melt, variable %in% c('AU_TMq', 'predQ')),
                     aes(x = date, y = value, color = variable)) +
                     theme_grey() +
                     labs(y = NULL, x = NULL) +
                     labs(title = "Trimmed Mean CPI: %QoQ") +
                     theme(legend.title = element_blank()) +
                     geom_line(size = 1.2) +
                     scale_color_manual(values = c('orange', 'blue'))
png(file.path(plotPATH, "YoY_TMQpred.png"))
grid.arrange(gp_tmQpred, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

gp_tmYpred <- ggplot(data = subset(tmx_melt, variable %in% c('AU_TMy', 'predY')),
                     aes(x = date, y = value, color = variable)) +
                     theme_grey() +
                     labs(y = NULL, x = NULL) +
                     labs(title = "Trimmed Mean CPI: %YoY") +
                     theme(legend.title = element_blank()) +
                     geom_line(size = 1.2) +
                     scale_color_manual(values = c('orange', 'blue'))
png(file.path(plotPATH, "YoY_TMYpred.png"))
grid.arrange(gp_tmYpred, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

png(file.path(plotPATH, "qoqPredXTS.png"))
plot(tmx$AU_TMq['2004::'], las=1, main="Australian QoQ Trimmed Mean CPI", type='o', pch=20)
lines(tmx$AU_TMq['2004::'], las=1, lwd=2, type='o', pch=20, cex=1.3)
lines(tmx$predQ['2004::'], las=1, lwd=2, type='o', pch=4, col=2)
legend('topleft', c('Aus TM CPI', 'Model Pred'), pch = c(20, 4), col = c(1,2), 
       bg = 'lightgrey', horiz=FALSE)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Sources: ABS, Cleveland Fed', side=1, line=4, adj=0)
dev.off()

png(file.path(plotPATH, "yoyPredXTS.png"))
plot(tmx$AU_TMy['2004::'], las=1, main="Australian YoY Trimmed Mean CPI", type='o', pch=20)
lines(tmx$AU_TMy['2004::'], las=1, lwd=2, type='o', pch=20, cex=1.3)
lines(tmx$predY['2004::'], las=1, lwd=2, type='o', pch=4, col=2)
legend('topleft', c('Aus TM CPI', 'Model Pred'), pch = c(20, 4), col = c(1,2), 
       bg = 'lightgrey', horiz=FALSE)
mtext('www.ricardianambivalence.com', side=1, line=4, adj=1)
mtext('Sources: ABS, Cleveland Fed', side=1, line=4, adj=0)
dev.off()

L_qoqXY <- xyplot(tmx$AU_TMq['1994::'] ~ tmx$US_TMq['1994::'], ylim = c(0.2, 1.5),
       panel = function(x, y)
       {
           panel.xyplot(x,y)
           panel.abline(lm(y ~ x), lty=2, col='orange', lwd=2)
       },
       xlab = "US Trimmed mean CPI",
       ylab =  "Aus Trimmed mean CPI",
       main = "AU TM CPI ~ US TM CPI (%QoQ)"
       )
png(file.path(plotPATH, "L_qoqXY.png"))
grid.arrange(L_qoqXY, sub = textGrob('www.ricardianambivalence.com'))
dev.off()

L_yoyXY <- xyplot(tmx$AU_TMy['1994::'] ~ tmx$US_TMy['1994::'], ylim = c(1.5, 5.0),
       panel = function(x, y)
       {
           panel.xyplot(x,y)
           panel.abline(lm(y ~ x), lty=2, col='orange', lwd=2)
       },
       xlab = "US Trimmed mean CPI",
       ylab =  "Aus Trimmed mean CPI",
       main = "AU TM CPI ~ US TM CPI (%YoY)"
       )
png(file.path(plotPATH, "L_yoyXY.png"))
grid.arrange(L_yoyXY, sub = textGrob('www.ricardianambivalence.com'))
dev.off()
