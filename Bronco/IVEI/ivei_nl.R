# Description: Primer on volatility index
# Author: Paolo (paolo@irrational.ly)

library(data.table)
library(tidyverse)
library(FactoMineR) # Para PCA

ine_raw <- fread("dat/tbl_ine.csv",header = TRUE, sep = ",", stringsAsFactors = F)
ine_raw
names(ine_raw)
levels(ine_raw$NOMBRE_ESTADO)

# Solo nuevo león por ahora
nl <- ine_raw %>% filter(NOMBRE_ESTADO == "nuevo leon")
nl <- nl %>% filter(CODIGO_MUNICIPIO != "NA")
length(unique(nl$CODIGO_MUNICIPIO))
max(nl$CODIGO_MUNICIPIO)
setdiff(1:51,nl$CODIGO_MUNICIPIO) # Están todos

indices <- 1:9 # vector para subsetear los índices de las columnas de info
# Reviso cuales tienen NA's para quitarlos
nl[,-indices] %>% sapply(FUN = function(x){sum(is.na(x))})

# Solo hay un CI para diputado federal en el 2015 y es en el municipio de Guadalupe. AJUA!
# Morena solo participa en las del 2015 entonces las quito
partidos <- c("PAN", "PRI", "PRD", "PNA", "PT", "PVEM")

# Subseteo solo esas columnas
partidos_index <- match(partidos, names(nl))

# La buena, para ya hacer la multiplicación de matrices
nl.clean <- nl[,c(indices,partidos_index)]
write.csv(nl.clean, "out/volatility_nl.csv", row.names = FALSE)

# Necesitamos tener el listado nominal para todos...
# Debería de sacar la tasa de crecimiento para cada una de las secciones y llenarlas (admeás de que hay tasas negativas y asi...)
avg.nom <- nl.clean %>% group_by(SECCION) %>% 
                        summarise(avg = mean(NOMINAL, na.rm = TRUE))

# Las metemos a nl.clean
nl.clean.nominal <- left_join(nl.clean, avg.nom, by = "SECCION")

# Hacemos un ID único para cada eleeción (son 5 en teoría por sección)
ID_ELEC <- paste(nl.clean$ELECCION,nl.clean$ANO, sep = "-")

# Sacamos los porcentajes relativos
nl.percent <- as_tibble(nl.clean.nominal[ ,10:15]/nl.clean.nominal$avg*100)

# Hay algunas secciones que se pasan de 100% hmmmm
sum(apply(nl.percent, 1, sum)> 100)

# Fuck it 
nl.percent <- add_column(nl.percent, SECCION = nl.clean$SECCION, ID_ELEC)
rm(ID_ELEC)

# Ordenamos con base en las secciones
nl.ordered <- arrange(nl.percent, SECCION, ID_ELEC)
nl.ordered
# write.csv(nl.ordered, "out/ordered_NL.csv", row.names = FALSE)

#Antes de hacer un análisis, algúnas gráficas
grafica_partidos <- function(df, seccion){
    # Wrap para un ggplot que grafica las tendencias historicas de 
    
    df <- nl.ordered %>% filter(SECCION == seccion) %>% 
        gather(key = "PARTIDO", value = "PORCENTAJE", -ID_ELEC, -SECCION) 
    
    ggplot(df, aes(x = ID_ELEC, y = PORCENTAJE, color = PARTIDO)) +
        geom_line(aes(group = PARTIDO)) + geom_point() + 
        labs(x = "Elección-Año", y = "Porcentaje de Votos") +
        ggtitle(paste("Análisis Historico de Votos en Sección:", seccion)) +
        theme(plot.title = element_text(hjust = 0.5))
}

# PAN, PRI, PRD, PNA, PT, PVEM
grafica_partidos(nl.ordered,943)
grafica_partidos(nl.ordered,100)
grafica_partidos(nl.ordered,349)
grafica_partidos(nl.ordered,1951)

# No jaló lo de los colores
# colores <- rep(c("blue","red","yellow","lightblue","red4","limegreen"),each = 5)

# Hay algunas secciones que tienen menos eleciones... 
dif_5 <- table(nl.ordered$SECCION) != 5
sum(dif_5)
# Las voy a quitar

malos <- as.integer(names(dif_5)[which((dif_5) == TRUE)])
malos

# Quitamos las malas
nl.matrix <- nl.ordered[!(nl.ordered$SECCION %in% malos),]
nl.matrix

# El orden de las eleciones queda con sus pesos:
# dif-2009 - .05
# dif-2012 - .1
# dif-2015 - .3 - Más reciente
# prs-2012 - .45 - Más relevante
# sen-2012 - .1 - Meh

# Le doy mucho más peso a la elección presidencial que a las de diputados y senadores y más peso a la más reciente
w <- c(.05,.1,.3,.45,.1)
sum(w)

multiplica_data_frames <- function(df, pesos, num_part, num_elec){
    m <- length(w)
    n <- dim(df)[1]
    
    X <- matrix(0L, nrow = n/m, ncol = num_part)
    colnames(X) <- names(df)[1:num_part]
    
    for(i in seq(0,n/m-1)){
        # print(i)
        x <- t(df[(1 + num_elec*i):(num_elec*(i+1)), 1:num_part])
        # print(x)
        z <- x %*% w
        # print(z)
        X[i,] <- t(z/norm(z)*100)
    }
    
    
    return(as_tibble(X))
}

temp <- multiplica_data_frames(nl.matrix, pesos = w, num_part = 6, num_elec = 5)

nl.afinidad <- add_column(temp, SECCION = unique(nl.matrix$SECCION))
# write.csv(nl.afinidad, "NL_afinidad_partido.csv", row.names = FALSE)

# Análisis de Componentes Principales para el índice
# No vale la pena escalarlos porque ya están todos en la misma escala, solo se necesitan centrar
escala <- FALSE
cp <- PCA(scale(nl.afinidad[,-7], scale = escala), scale.unit = escala,
          graph = TRUE)

cp$eig #La primera coordenada explica el 80% de la variablidad de tus datos

# solo necesitamos la primera coordenada y con esa nos sale el índice
indice <- cp$ind$coord[,1]
indice <- indice - min(indice) + 1
# indice <- log(indice)
indice <- indice/max(indice)*100
hist(indice, col = "steelblue", breaks = 50)

# Sacamos "índice de Volatilidad 1.0 TM"
indice2 <- -0.04*(indice - 50)^2 + 100 # Parábola
hist(indice, col = "steelblue", breaks = 50)

nl.final <- add_column(nl.afinidad, INDICE_PAN_PRI = indice, INDICE_VOLATILIDAD = indice2)
nl.final <- arrange(nl.final, desc(INDICE_VOLATILIDAD))
nl.final

# Revisamos para las 4 que habíamos graficado antes
nl.final[which(nl.final$SECCION == 943),]
nl.final[which(nl.final$SECCION == 100),]
nl.final[which(nl.final$SECCION == 349),]
nl.final[which(nl.final$SECCION == 1951),]

write.csv(nl.final, "out/ivei_nl.csv", row.names = FALSE)