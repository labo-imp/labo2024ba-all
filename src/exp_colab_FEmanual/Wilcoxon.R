
# Buscamos path de los resultados de los tres modelos con file.choose(), cargamos el archivo y creamos datasets separados
# para los tres modelos distintos

path <- file.choose()
datos <- read.csv2(path)
modeloFinal <- datos[datos$modelo=="Modelo Final",]
modeloBase <- datos[datos$modelo=="Variables Gustavo",]
modeloSinFe <- datos[datos$modelo=="sin FE",]
options(scipen = 999)


# Comparamos la línea de mejor ganancia promedio de nuestro modelo final (11500 envíos) con el resto de las líneas para ver
# si es realmente la mejor. Usamos Wilcoxon.

comparacion_modeloFinal <- data.frame(envios=NA,pvalue=NA)
r <- 1

for (i in c(9000,9500,10000,10500,11000,12000,12500,13000)){
  comparacion_modeloFinal[r,1] <- i
  comparacion_modeloFinal[r,2] <- wilcox.test(modeloFinal[modeloFinal$corte==11500,'ganancia'],
                                              modeloFinal[modeloFinal$corte==i,'ganancia'],paired = T)$p.value
  r <- r+1
}


comparacion_modeloFinal

# De acuerdo a la evidencia muestral, no podemos afirmar que los 11500 envíos son realmente mejores que 9000,
# 9500, 10000, 10500, 11000 y 12000. No obstante, siendo que debemos elegir alguna línea, nos quedaremos con 11500 porque
# tiene el mayor promedio.



# Comparamos nuestro modelo final con 11500 envíos contra el modelo del workflow base.

comparacion_11500_vs_modeloBase <- data.frame(envios=NA,pvalue=NA)

r <- 1

for (i in c(9000,9500,10000,10500,11000,11500,12000,12500,13000)){
  comparacion_11500_vs_modeloBase[r,1] <- i
  comparacion_11500_vs_modeloBase[r,2] <- wilcox.test(modeloFinal[modeloFinal$corte==11500,'ganancia'],
                                                      modeloBase[modeloBase$corte==i,'ganancia'],paired = T)$p.value
  r <- r+1
}
comparacion_11500_vs_modeloBase


# Podemos observar que nuestro modelo con 11500 envíos siempre es superior al modelo del workflow base.





# Comparamos nuestro modelo final con 11500 envíos contra el modelo sin FE.

comparacion_11500_vs_modeloSinFE <- data.frame(envios=NA,pvalue=NA)

r <- 1

for (i in c(9000,9500,10000,10500,11000,11500,12000,12500,13000)){
  comparacion_11500_vs_modeloSinFE[r,1] <- i
  comparacion_11500_vs_modeloSinFE[r,2] <- wilcox.test(modeloFinal[modeloFinal$corte==11500,'ganancia'],
                                                       modeloSinFe[modeloSinFe$corte==i,'ganancia'],paired = T)$p.value
  r <- r+1
}
comparacion_11500_vs_modeloSinFE


# No podemos afirmar que nuestro modelo sea mejor que el modelo sin FE para los 11500, 12000, 12500 y 13000 envíos.



# Comparamos el modelo sin FE consigo mismo.


comparacion_modeloSinFE <- data.frame(envios=NA,pvalue=NA)
r <- 1

for (i in c(9000,9500,10000,10500,11000,12000,12500,13000)){
  comparacion_modeloSinFE[r,1] <- i
  comparacion_modeloSinFE[r,2] <- wilcox.test(modeloSinFe[modeloSinFe$corte==11500,'ganancia'],
                                              modeloSinFe[modeloSinFe$corte==i,'ganancia'],paired = T)$p.value
  r <- r+1
}


comparacion_modeloSinFE

# Elegimos 11500 envíos para comparar contra el modelo base.


# Comparamos el modelo sin FE contra el modelo del workflow base.

comparacion_11500sinFE_vs_modeloBase <- data.frame(envios=NA,pvalue=NA)

r <- 1

for (i in c(9000,9500,10000,10500,11000,11500,12000,12500,13000)){
  comparacion_11500sinFE_vs_modeloBase[r,1] <- i
  comparacion_11500sinFE_vs_modeloBase[r,2] <- wilcox.test(modeloSinFe[modeloSinFe$corte==11500,'ganancia'],
                                                           modeloBase[modeloBase$corte==i,'ganancia'],paired = T)$p.value
  r <- r+1
}
comparacion_11500sinFE_vs_modeloBase


# Podemos ver que el modelo sin FE siempre es mejor que el modelo base.