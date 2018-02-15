# IVEI para todos los estados de la república

library(tidyverse)

source(file = "IVEI.R")

ine_raw <- as_tibble(read.csv(file = "raw/tbl_ine.csv"))
names(ine_raw)
ine_raw$CODIGO_ESTADO <- as.factor(ine_raw$CODIGO_ESTADO)
levels(ine_raw$CODIGO_ESTADO)

ine <- select(ine_raw, -ANO, -ELECCION, -(CODIGO_MUNICIPIO:SECCION)) %>% add_column(ELECCIONES = paste(ine_raw$ELECCION, ine_raw$ANO, sep = "-"))

# Happy Function... (me tarde demasiado haciendo esta estupidez)
estados <- ine %>% 
    group_by(CODIGO_ESTADO, NOMBRE_ESTADO, ELECCIONES) %>% 
    summarise_all(sum, na.rm = TRUE) %>% 
    arrange(CODIGO_ESTADO, ELECCIONES)

write.csv(estados, file = "out/Votos_Estados.csv", row.names = FALSE)

# Problema de Nominal, fuck los que son ceros.
estados.nom <- estados %>% 
    group_by(CODIGO_ESTADO) %>% 
    summarise(NOMINAL_PROM = mean(NOMINAL[NOMINAL != 0]))

# Las metemos a estados de la manera más dirty posible y aprovechando que si hay 5 por categoría y saco los porcentajes
estados.clean <- estados
estados.clean$NOMINAL <- rep(estados.nom$NOMINAL_PROM, each = 5)

# Revisamos que partidos son relevantes para la multiplicación de matrices
sapply(estados.clean[, 5:21], function(x){sum(x == 0)})

# ALV los dos independientes y las remoras
estados.clean <- estados.clean %>% select(-PCONV, -PH, -PMC, -PMOR, -PPM, -PS, -PSD, -PSM, -IND1_DIF15, -IND2_DIF15, -NO_REG)

#----------------------------------------------------------------------------
# NOTA: MEJORAR ALGORITMO DE MULTIPLICACIÓN PARA CONSIDERAR ÍNDICE DE PARTIDO

# Puto HW
estados.porcentaje <- as_tibble(data.frame(estados.clean[,1:4], 
                estados.clean[ ,5:ncol(estados.clean)]/estados.clean$NOMINAL))

# El orden de las eleciones queda con sus pesos:
# dif-2009 - .05
# dif-2012 - .1
# dif-2015 - .3 - Más reciente
# prs-2012 - .45 - Más relevante
# sen-2012 - .1 - Meh

# Le doy mucho más peso a la elección presidencial que a las de diputados y senadores y más peso a la más reciente
w <- c(.05,.1,.3,.45,.1)
sum(w)

multiplica_data_frames <- function(df, pesos){
    # Función que calcúla de forma super rough una medida de afinidad al partido con base en los pesos especificados en w
    # df:= data frame númerico convertible en matriz
    # w:= vector de pesos para multiplicar
    # Regresa una matriz de tamaño n/m * l 
    
    m <- length(w) # Número de eleciones
    n <- dim(df)[1] # Total de entradas
    l <- dim(df)[2] # Número de partidos
    
    # print(m)
    # print(n)
    # print(l)
    
    if((160 %% 5) == 0){
        X <- matrix(0L, nrow = n/m, ncol = l)
        colnames(X) <- names(df)
        
        for(i in seq(0,n/m-1)){
            # print(i)
            x <- t(df[(1 + m*i):(m*(i+1)), ])
            # print(x)
            z <- x %*% w
            # print(z)
            X[i + 1,] <- t(z/norm(z))
        }
        
        return(X)
        
    }else{
        stop("Error en las dimensiones de la matriz")
    }
}

temp <- multiplica_data_frames(estados.porcentaje[, 5:ncol(estados.porcentaje)], pesos = w)

estados.final <- as_tibble(data.frame(CODIGO_ESTADO = 1:32, 
                                      unique(estados.porcentaje$NOMBRE_ESTADO),
                                      temp, IVEI = IVEI(temp)))

write.csv(estados.final, "out/IVEI_Estados.csv", row.names = FALSE)
