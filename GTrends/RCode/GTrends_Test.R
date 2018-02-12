# Prueba de Google Trends

library(gtrendsR)
library(tidyverse)

# 0 . Preamble exploratorio del paquete

# Esta cosa trae una base de datos de la ONU para codificar los nombres y las claves de los estados, además vienen los municipíos por nombre. Sin embargo, está ligeramente desactualizada (trae 700 municipios). 
# El código de Mexico es MX
paises <- countries
# write.csv(paises, "CodigosPaisesGoogle.csv", row.names = FALSE)
head(paises)
mexico <- paises %>% filter(country_code == "MX")
head(mexico)

# Categorías de google
categorias <- categories
# write.csv(categorias, "CategoriasGoogleID.csv", row.names = FALSE)
head(categorias)

# 1. Definición de variables
# El tiempo es todavía más granular con este paquete, puedo sacarlo entre 2 fechas especificas. 
ano <- "2017"
mes <- "07"
dia <- "24"
fecha_inicio <- paste(ano,mes,dia,sep = "-")
fecha_personal <- paste(fecha_inicio, Sys.Date())
fecha_anual <- "today 12-m"

# Definición de terminos por agrupar en cada categoría
terminos <- c("Partido Acción Nacional", 
              "Partido Revolucionario Institucional", 
              "Movimiento Regeneración Nacional")

trends <- gtrends(terminos, geo = "MX", time = fecha_anual)

# el argumento de gprop puede ser: "web", "news", "images"
# usualmente web es más repreesntativo (ish)

# Nota: se mide la popularidad relativa
# Las categorías llegan a ser muy especificas por si queremos comparar en ciertos temas
# Se puede hacer un query mucho más narrow con las categorías. 
# Vale la pena hacerlo a la par en la página para visualizar cuales son relevantes
# Muchos muchos datos
# El objeto que regresa trae cosas interesantes como los related seaches
# Aguas con los acentos

plot(trends) #Tenemos un paquete para graficar

# Exploramos Trends
head(trends$related_queries)




