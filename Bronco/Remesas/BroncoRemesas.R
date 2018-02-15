# Google Trends vs Bronco

library(banxicoR)
library(tidyverse)
library(readxl)


setwd("MisDocumentos/Irrational_Full_Mode/BroncoRemesas/")

rem_trim <- read_excel("Remesas03-17 - Banxico.xlsx")
bronco_Gtrend <- read.csv("PopularidadBronco.csv")

rem_anual <- rem_trim %>% 


rem_17 <- rowSums(rem_trim[ ,grep(pattern = "2017", x = names(rem_trim))])
rem_17 <- data.frame(CODIGO_ESTADO = rem_trim$CODIGO_ESTADO, ESTADO = rem_trim$ESTADO, remesas_2017 = rem_17)

bronco_rem <- left_join(bronco_Gtrend, rem_17, by = "CODIGO_ESTADO")

plot(x = log(bronco_rem$POPULARIDAD), y = log(bronco_rem$remesas_2017))
cor(x = bronco_rem$POPULARIDAD, y = bronco_rem$remesas_2017)

# Aguas, siempre va el más grande primero
index <- setdiff(1:32, bronco_Gtrend$CODIGO_ESTADO)
rem_17$ESTADO[index]

total <- sum(rem_17$remesas_2017)
prom <- mean(rem_17$remesas_2017)

# Porcentaje de las remesas que capturan estados donde buscan al bronco
1 - sum(rem_17$remesas_2017[index])/total

# Promedio ponderado
pond <- weighted.mean(bronco_rem$remesas_2017, w = bronco_rem$POPULARIDAD/sum(bronco_rem$POPULARIDAD))

write.csv(bronco_rem, "Remesas_Bronco_Consolidado.csv", row.names = FALSE)










