
###Me traigo los vectores de la corrida de 6 hiperparametros
envio1600 <- c(5699,6899,6919,6559,6139,6549,5339,6189,6519,5819,6519,6549,6059,6169,5469,7329,5759,6559,6969,6139,7189,6179,7719,5769,6899,7829,5509,6709,6619,5729,6519,6410)
envio1700 <- c(6229,6649,6609,6229,6619,7099,5789,5939,6659,5459,7029,6659,6239,6149,5639,7039,5479,6339,6739,5799,6999,5909,7479,5879,7109,7479,6029,6869,6749,5849,6474,6425)
envio1800 <- c(6389,6759,6629,6019,6759,7219,5959,5999,7229,5929,7179,6789,6319,6679,6049,6719,5639,6489,7179,5919,6719,6009,7169,6779,6799,7139,6089,6529,7229,6349,6654,6555)
envio1900 <- c(6389,6859,6359,6889,6859,7309,6019,5629,6919,6459,6899,6899,6409,6369,6129,6469,6099,6129,6869,5629,6439,5709,6889,6539,6469,7169,6959,6939,6969,6409,6469,6536)
envio2000 <- c(6899,6599,6889,6979,6499,7409,5739,5739,7099,6589,7039,7069,6509,6069,5899,6219,6559,6689,6529,6199,6579,5439,6569,6239,6499,6849,7139,6639,6739,6509,6574,6547)
envio2100 <- c(6599,6289,6679,6739,6569,7119,6239,6649,6809,6669,6739,6799,6249,5829,5559,5869,7049,7239,6189,6359,6269,5479,6229,5929,6989,6549,6799,6359,6809,6309,6559,6465)
envio2200 <- c(6369,6379,6399,6869,6219,6829,6409,6659,6519,6299,6449,6499,6479,6289,5209,6369,6709,6909,6369,7239,6349,5159,6739,6069,7079,7079,6479,6399,6919,6429,6439,6472)
#Hago wilcoxon entre todos los vectores de la de 6 hiperparametros
# Lista para almacenar los p-valores
resultados_pvalores <- data.frame(Comparacion = character(), PValor = numeric(), stringsAsFactors = FALSE)
# Supongamos que ya tienes las filas de tus envíos (por ejemplo, envio1600, envio1700, etc.)
envios <- list(envio1600, envio1700, envio1800, envio1900, envio2000, envio2100, envio2200)
names(envios) <- c("1600", "1700", "1800", "1900", "2000", "2100", "2200")

# Crear una lista para almacenar los p-valores
resultados_pvalores <- data.frame(Comparacion = character(), PValor = numeric(), stringsAsFactors = FALSE)

# Iterar sobre cada par de envíos y realizar la prueba de Wilcoxon
for (i in 1:(length(envios) - 1)) {
  for (j in (i + 1):length(envios)) {
    # Realizar la prueba de Wilcoxon
    test_result <- wilcox.test(envios[[i]], envios[[j]], paired = TRUE, exact = FALSE)
    
    # Nombre de la comparación usando los nombres asignados
    comparacion <- paste0("envio", names(envios)[i], " vs envio", names(envios)[j])
    
    # Agregar los resultados al dataframe
    resultados_pvalores <- rbind(resultados_pvalores, data.frame(Comparacion = comparacion, PValor = test_result$p.value))
  }
}

# Mostrar la tabla de resultados correctamente formateada
print(resultados_pvalores)


#No me dio nada concluyente. Entonces, me quedo con el de mejor mediana o el de mejor promedio (a mi el test de wilcoxon entre el de mejor mediana y el de mejor promedio me dio que no habia
#diferencias significativas, asi que me daba igual cual tomar)

#Me traigo los vectores de la corrida de 13 hiperparametros. Hago wilcoxon entre el mejor de la de 6 hip (en mi caso la de 2000 envios) y todos los vectores de la de 13 hiperparametros.
envio1600_13 <-c(6619,7009,5089,5839,4659,7459,7349,6269,5699,5849,6219,5049,5139,6169,5759,5769,7079,6379,6239,4689,5339,5479,5779,6179,5399,5799,5439,6399,5969,5379,5819,5916)
envio1700_13 <-c(6319,7139,5539,5889,5179,7139,7059,6779,6249,5569,6709,5119,4809,6309,5469,5499,6759,6469,5999,4449,5419,5219,5839,5829,5099,6349,5169,6059,6009,5179,5864,5887)
envio1800_13 <-c(5969,6859,5219,6009,5639,6879,6809,6869,6799,5349,6889,4769,4519,6409,5989,5569,6389,6599,5749,4579,5119,5229,5539,5999,5159,6869,5189,5799,5739,4839,5774,5845)
envio1900_13 <-c(5719,6609,5359,6119,6509,6869,6929,6999,6959,5139,6619,5239,4199,6549,6899,6089,6479,7149,5469,4659,5239,5729,5249,5709,5329,6889,4899,5909,5479,4909,5819,5930)
envio2000_13 <-c(5829,6279,5449,6239,6969,6629,7009,6669,6689,6069,6249,5349,4299,6289,6589,6229,6179,6789,5579,5599,4899,6219,5749,5439,5479,6959,4589,6009,5129,4929,6124,5946)
envio2100_13 <-c(5909,6359,5169,6339,6589,6339,7019,6819,6429,5829,6739,5009,4419,6369,7099,6409,5869,6899,5279,5249,5059,6599,5929,5559,5589,6659,5919,5809,4779,5019,5924,5969)
envio2200_13 <-c(6019,6419,6079,6469,6329,6069,6789,6549,6189,5519,6859,5849,4939,6509,7189,6109,6789,6509,5389,5669,4789,6319,5579,5209,5669,6389,6029,5459,4469,5189,6074,5978)
# Lista para almacenar los p-valores
resultados_pvalores <- data.frame(Comparacion = character(), PValor = numeric(), stringsAsFactors = FALSE)
# Supongamos que ya tienes las filas de tus envíos (por ejemplo, envio1600, envio1700, etc.)
envios <- list(envio1800,envio1600_13, envio1700_13, envio1800_13, envio1900_13, envio2000_13, envio2100_13, envio2200_13)
names(envios) <- c("1800_6hip","1600", "1700", "1800", "1900", "2000", "2100", "2200")

# Crear una lista para almacenar los p-valores
resultados_pvalores <- data.frame(Comparacion = character(), PValor = numeric(), stringsAsFactors = FALSE)

# Iterar sobre cada par de envíos y realizar la prueba de Wilcoxon
for (i in 1:(length(envios) - 1)) {
  for (j in (i + 1):length(envios)) {
    # Realizar la prueba de Wilcoxon
    test_result <- wilcox.test(envios[[i]], envios[[j]], paired = TRUE, exact = FALSE)
    
    # Nombre de la comparación usando los nombres asignados
    comparacion <- paste0("envio", names(envios)[i], " vs envio", names(envios)[j])
    
    # Agregar los resultados al dataframe
    resultados_pvalores <- rbind(resultados_pvalores, data.frame(Comparacion = comparacion, PValor = test_result$p.value))
  }
}

# Mostrar la tabla de resultados correctamente formateada
print(resultados_pvalores)



#Hago lo mismo con la corrida base, de 1 hiperparametro
#Me traigo los vectores de esa corrida y hago wilcoxon contra el mejor de la corrida de 6 hiperparametros

envio1600_base<-c(5319,4089,7239,4639,3839,4639,4259,6369,4999,3709,4449,5169,6459,4849,5779,4779,3549,4329,5499,5149,4389,4939,3189,3929,4199,5229,4539,4319,5979,3709,4639,4784)
envio1700_base<-c(5339,4989,7009,5149,3489,4299,4399,6889,6249,4229,4179,5299,6549,4989,5499,4539,3249,3979,5219,4859,4459,5059,3689,3669,5019,4959,4209,4029,5709,3849,4909,4835)
envio1800_base<-c(5469,4739,6709,5309,3589,3989,4429,6609,5949,4269,4339,5029,6689,5019,5619,4149,3659,4119,5299,4569,5059,5149,3779,3779,4739,5069,4009,3779,5829,4339,4739,4836)
envio1900_base<-c(5969,4419,6879,5439,3309,3679,4499,6739,5679,4019,4069,4709,6339,4729,5279,4239,3809,4189,5399,4279,5219,5219,3899,3859,4469,4839,4029,3819,5599,4169,4484,4760)
envio2000_base<-c(6439,4199,6929,5499,4209,4249,4089,6729,5379,4539,3719,4489,6409,4779,6199,4359,3559,4659,5089,3969,5369,4979,3629,3899,4499,4489,3709,3949,5669,3879,4494,4785)
envio2100_base<-c(6059,3819,6629,5199,4339,4759,4949,6419,5539,4229,3879,4139,6099,4569,5989,4079,4029,4439,5229,4059,5089,4699,4129,4099,4239,4199,3779,4029,5369,3559,4389,4721)
envio2200_base<-c(5729,3989,6349,4899,4449,4529,4669,6149,5319,4339,3999,4269,5759,4659,5739,4189,4499,4129,5349,4959,5279,4409,3929,3879,4699,3939,3509,3819,5899,3309,4514,4688)
# Lista para almacenar los p-valores
resultados_pvalores <- data.frame(Comparacion = character(), PValor = numeric(), stringsAsFactors = FALSE)
# Supongamos que ya tienes las filas de tus envíos (por ejemplo, envio1600, envio1700, etc.)
envios <- list(envio1800,envio1600_base, envio1700_base, envio1800_base, envio1900_base, envio2000_base, envio2100_base, envio2200_base)
names(envios) <- c("1800_6hip","1600", "1700", "1800", "1900", "2000", "2100", "2200")

# Crear una lista para almacenar los p-valores
resultados_pvalores <- data.frame(Comparacion = character(), PValor = numeric(), stringsAsFactors = FALSE)

# Iterar sobre cada par de envíos y realizar la prueba de Wilcoxon
for (i in 1:(length(envios) - 1)) {
  for (j in (i + 1):length(envios)) {
    # Realizar la prueba de Wilcoxon
    test_result <- wilcox.test(envios[[i]], envios[[j]], paired = TRUE, exact = FALSE)
    
    # Nombre de la comparación usando los nombres asignados
    comparacion <- paste0("envio", names(envios)[i], " vs envio", names(envios)[j])
    
    # Agregar los resultados al dataframe
    resultados_pvalores <- rbind(resultados_pvalores, data.frame(Comparacion = comparacion, PValor = test_result$p.value))
  }
}

# Mostrar la tabla de resultados correctamente formateada
print(resultados_pvalores)

#Todas las dif son significativas, tiene sentido.