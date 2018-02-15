# Description: Map 2015 NL ivei for state gobernador election - mariana@irational.ly
# Author: Mariana (mariana@irrational.ly)

setwd('')
options(scipen = 999)
require(data.table)
require(dplyr)
require(doBy)
source('../../_misc/themes/theme_maps.R')

st <- fread('out/ivei_states.csv', header = TRUE, sep = ',', stringsAsFactors = F)

map.data <- readOGR('./raw', 'mexico') # Takes a while -  be patient
map.df <- data.frame(id = rownames(map.data@data), map.data@data)
map.f <- fortify(map.data)
map <- merge(map.f, map.df, by = "id")
tbl_df(map)

dat <- as.data.frame(subset(map, select = c(long, lat, group, ENTIDAD)))
names(dat) <- c('long', 'lat', 'group', 'CODIGO_ESTADO')
dat$CODIGO_ESTADO <- as.integer(dat$CODIGO_ESTADO)

z <- left_join(dat, st)

m <- ggplot(z, aes(x = long, y = lat, group = CODIGO_ESTADO, fill = IVEI)) +
  ggtitle(expression(atop("ÍNDICE DE VOLATILIDAD"))) +
  coord_equal() +
  geom_polygon(aes(fill = IVEI)) +
  geom_path(colour = "black", size = .1) +
  scale_fill_gradient2(low = '#2c7bb6', mid = '#ffffbf', high = '#7b3294', na.value = 'white', name = "Índice") +
  theme_maps()

png('viz/map_ivei_states.png', res = 300, height = 6000, width = 8000) 
plot(m) 
dev.off()