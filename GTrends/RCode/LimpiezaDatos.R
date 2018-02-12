# Fusión y limpieza de datos INEGI vs GTrends

library(tidyverse)
library(corrplot)
library(lattice)

# Archivo que limpie y le puse los códigos a mano
gtrends <- read.csv("../dat/PartidosPorEstado.csv")
gtrends$CODIGO_ESTADO <- as.factor(gtrends$CODIGO_ESTADO)

inegi <- read.csv("../dat/INEGI.csv")
names(inegi)
inegi[,1:4] <- lapply(inegi[,1:4], as.factor)
sapply(inegi, class)

# Agrupo los datos
resumen_inegi <- inegi %>% group_by(CODIGO_ESTADO) %>%  
    summarise_if(is.numeric, mean, na.rm = TRUE)

final <- right_join(gtrends, resumen_inegi, by = "CODIGO_ESTADO")

write.csv(final, "../dat/TablaFinal.csv", row.names = FALSE)

