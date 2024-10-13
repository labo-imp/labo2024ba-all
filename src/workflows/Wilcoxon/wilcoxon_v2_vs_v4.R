# Cargar ambos archivos .txt
data_v1 <- read.table("/home/consultorio_grazianimp/buckets/b1/flow/wf_julio-002/011-EV_evaluate_conclase_gan/ganancias_01_069_v2.txt", header = TRUE, sep = "\t")
data_v2 <- read.table("/home/consultorio_grazianimp/buckets/b1/flow/wf_julio-004/011-EV_evaluate_conclase_gan/ganancias_01_001_v4.txt", header = TRUE, sep = "\t")

# Asegurarse de que la columna de "envios" sea numérica y verificar los nombres de las columnas
data_v1$envios <- as.numeric(data_v1$envios)
data_v2$envios <- as.numeric(data_v2$envios)

# Filtrar envíos múltiplos de 100 en ambos archivos
data_v1 <- subset(data_v1, envios %% 100 == 0)
data_v2 <- subset(data_v2, envios %% 100 == 0)

# Inicializar vectores para almacenar p-valores y ganancias promedio
p_values <- numeric(nrow(data_v1))
ganancia_promedio_exp1 <- numeric(nrow(data_v1))
ganancia_promedio_exp2 <- numeric(nrow(data_v2))

# Realizar el test de Wilcoxon para cada envío múltiplo de 100
for (i in 1:nrow(data_v1)) {
  # Extraer los vectores de ganancias para el envío actual en ambos archivos
  vector_v1 <- as.numeric(data_v1[i, 3:12])  # Ajusta el rango según el número de columnas de ganancias
  vector_v2 <- as.numeric(data_v2[i, 3:12])  # Ajusta el rango según el número de columnas de ganancias
  
  # Calcular la ganancia promedio para cada envío
  ganancia_promedio_exp1[i] <- mean(vector_v1, na.rm = TRUE)
  ganancia_promedio_exp2[i] <- mean(vector_v2, na.rm = TRUE)
  
  # Realizar el test de Wilcoxon
  test_result <- wilcox.test(vector_v1, vector_v2, paired = FALSE, alternative = "two.sided")
  p_values[i] <- test_result$p.value
}

# Agregar los p-valores y las ganancias promedio a los datos del primer archivo para crear la tabla final
data_v1$ganancia_promedio_exp1 <- ganancia_promedio_exp1
data_v1$ganancia_promedio_exp2 <- ganancia_promedio_exp2
data_v1$p_value <- p_values

# Seleccionar y reordenar las columnas para la tabla final
final_data <- data_v1[, c("envios", "ganancia_promedio_exp1", "ganancia_promedio_exp2", "p_value")]

# Renombrar las columnas de ganancia
colnames(final_data)[2] <- "ganancia_promedio_exp2"
colnames(final_data)[3] <- "ganancia_promedio_exp4"

# Mostrar los resultados
print(final_data)

# Exportar el resultado a un archivo CSV
write.csv(final_data, "/home/consultorio_grazianimp/buckets/b1/comparaciones/resultado_comparacion_v2_vs_v4.csv", row.names = FALSE)
