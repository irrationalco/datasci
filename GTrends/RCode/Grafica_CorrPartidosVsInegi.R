# Gráfica de correlaciones entre hists partidos y features INEGI.
# Es una matriz de correlaciones glorificada

tabla <- read.csv("../dat/TablaFinal.csv")

partidos_est <- 7:10
partidos_names <- c("PAN","PRI","PRD","MORENA")
cols_relevantes <- c(11,16:20)
cols_names <- c("Núm Hijos", "Analfabetismo", "Educación Avanzada", "PEA", "Pob sin Serv. Salud", "Pob. con Auto")

# Lleno Matriz
cormat <- matrix(0L, nrow = length(partidos_est), ncol = length(cols_relevantes))
for(i in 1:dim(cormat)[1]){
    cormat[i, ] <- cor(tabla[, partidos_est[i]], tabla[, cols_relevantes])
}

# Asigno nombres
colnames(cormat) <- cols_names
rownames(cormat) <- partidos_names

# Colores decentes
rgb.palette <- colorRampPalette(c("blue","white", "red"), space = "rgb")
levelplot(t(cormat), ylab = "", xlab ="",
          col.regions = rgb.palette(200), cuts = 200, at=seq(-1,1,0.01), 
          scales=list(x=list(rot = 30)), aspect="iso")
