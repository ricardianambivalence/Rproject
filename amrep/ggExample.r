df <- structure(c(106487, 495681, 1597442,
     2452577, 2065141, 2271925, 4735484, 3555352,
     8056040, 4321887, 2463194, 347566, 621147,
     1325727, 1123492, 800368, 761550, 1359737,
     1073726, 36, 53, 141, 41538, 64759, 124160,
     69942, 74862, 323543, 247236, 112059, 16595,
     37028, 153249, 427642, 1588178, 2738157,
     2795672, 2265696, 11951, 33424, 62469,
     74720, 166607, 404044, 426967, 38972, 361888,
     1143671, 1516716, 160037, 354804, 996944,
     1716374, 1982735, 3615225, 4486806, 3037122,
     17, 54, 55, 210, 312, 358, 857, 350, 7368,
     8443, 6286, 1750, 7367, 14092, 28954, 80779,
     176893, 354939, 446792, 33333, 69911, 53144,
     29169, 18005, 11704, 13363, 18028, 46547,
     14574, 8954, 2483, 14693, 25467, 25215,
     41254, 46237, 98263, 185986), .Dim = c(19,
     5), .Dimnames = list(c("1820-30", "1831-40",
     "1841-50", "1851-60", "1861-70", "1871-80",
     "1881-90", "1891-00", "1901-10", "1911-20",
     "1921-30", "1931-40", "1941-50", "1951-60",
     "1961-70", "1971-80", "1981-90", "1991-00",
     "2001-06"), c("Europe", "Asia", "Americas",
     "Africa", "Oceania")))


df.m <- melt(df)
names(df.m) <- c("period", "region", "value")

a <- ggplot(df.m, aes(x = period, y = value/1e+06, fill = region)) +
     labs(x = NULL, y = "Number of People (in millions)",
         title = "Migration to the United States by Source Region (1820-2006)",
         fill = NULL)

b <- a + geom_bar(stat = "identity", position = "stack")

b <- b + scale_fill_brewer(palette = 'Set1')

# now tweaking the plot details
immigration_theme <- theme_update(axis.text.x = element_text(angle = 90, hjust = 1),
                                  panel.grid.major = element_line(colour = 'grey90'),
                                  panel.grid.minor = element_blank(),
                                  panel.background = element_blank(),
                                  axis.ticks = element_blank(),
                                  legend.position = 'none')

print(b)

# this facets the data by region
c <- b + facet_grid(region ~ .) + theme(legend.position = 'none')

# this adds on a total facet
total <- dcast(df.m, period ~ ., sum)
names(total) <- c('period', 'value')
total$region <- 'total'
df.m.t <- rbind(total, df.m)
cl <- c %+% df.m.t

# growth in the small regions is hard to see due to scale
c2 <- cl + facet_grid(region ~ ., scale = 'free_y')
print(c2)

d <- a + geom_bar(aes(x = region, y = value/1e+06)) +
    theme_grey() + facet_wrap(~period) + scale_fill_brewer(palette = 'Set1') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.ticks = element_blank()) +
    theme(legend.position = 'none') +
    theme(panel.grid.minor = element_blank())

print(d)

# overlapping density plots
require(ggplot2)
require(reshape2)
dp  <- data.frame(x = rnorm(1000,0,1), y=rnorm(1000, 0,2), z = rnorm(1000, 2, 1.5))

dp.m <- melt(dp)

ggplot(dp.m) + geom_density(aes(x = value, colour = variable), size=2) + labs(x = NULL) +
    theme(legend.position = 'none') +
    labs(title = 'densities from kernel')

ggplot(dp.m) + geom_freqpoly(aes(x = value, y = ..density.., colour = variable)) +
    labs(x = NULL) +
    theme(legend.position = 'none') +
    labs(title = 'freq poly from binned counts')

