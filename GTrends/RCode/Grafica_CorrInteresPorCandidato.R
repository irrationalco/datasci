# Código para crear la gráfica de interés por candidato.

library(corrplot)
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

candidatos_interes <- read.csv("dat/CandidatosVsInteres.csv")

cormat <- cor(candidatos_interes)

corrplot(cormat, type = "upper", method = "color", order = "hclust", 
         title = "Correlación entre Candidatos e Interés ciudadano", 
         tl.col = "black", tl.srt = 45, mar = c(2,1,3,1))
# Te hace un ordenamiento jerarquico 
# mar = c(bottom, left, top, right)