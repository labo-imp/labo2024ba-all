# Cargar ambos archivos .txt
data_v0 <- read.table("/home/consultorio_grazianimp/buckets/b1/flow/wf_julio-010/011-EV_evaluate_conclase_gan/ganancias_01_037_v0.txt", header = TRUE, sep = "\t")
data_v1 <- read.table("/home/consultorio_grazianimp/buckets/b1/flow/wf_julio-001/011-EV_evaluate_conclase_gan/ganancias_01_020_v1.txt", header = TRUE, sep = "\t")

# Asegurarse de que la columna de "envios" sea numérica y verificar los nombres de las columnas
data_v0$envios <- as.numeric(data_v0$envios)
data_v1$envios <- as.numeric(data_v1$envios)

# Filtrar envíos múltiplos de 100 en ambos archivos
data_v0 <- subset(data_v0, envios %% 100 == 0)
data_v1 <- subset(data_v1, envios %% 100 == 0)

# Inicializar vectores para almacenar p-valores y ganancias promedio
p_values <- numeric(nrow(data_v0))
ganancia_promedio_exp1 <- numeric(nrow(data_v0))
ganancia_promedio_exp2 <- numeric(nrow(data_v1))

# Realizar el test de Wilcoxon para cada envío múltiplo de 100
for (i in 1:nrow(data_v0)) {
  # Extraer los vectores de ganancias para el envío actual en ambos archivos
  vector_v0 <- as.numeric(data_v0[i, 3:12])  # Ajusta el rango según el número de columnas de ganancias
  vector_v1 <- as.numeric(data_v1[i, 3:12])  # Ajusta el rango según el número de columnas de ganancias
  
  # Calcular la ganancia promedio para cada envío
  ganancia_promedio_exp1[i] <- mean(vector_v0, na.rm = TRUE)
  ganancia_promedio_exp2[i] <- mean(vector_v1, na.rm = TRUE)
  
  # Realizar el test de Wilcoxon
  test_result <- wilcox.test(vector_v0, vector_v1, paired = FALSE, alternative = "two.sided")
  p_values[i] <- test_result$p.value
}

# Agregar los p-valores y las ganancias promedio a los datos del primer archivo para crear la tabla final
data_v0$ganancia_promedio_exp1 <- ganancia_promedio_exp1
data_v0$ganancia_promedio_exp2 <- ganancia_promedio_exp2
data_v0$p_value <- p_values

# Seleccionar y reordenar las columnas para la tabla final
final_data <- data_v0[, c("envios", "ganancia_promedio_exp1", "ganancia_promedio_exp2", "p_value")]

# Renombrar las columnas de ganancia
colnames(final_data)[2] <- "ganancia_promedio_exp0"
colnames(final_data)[3] <- "ganancia_promedio_exp1"

# Mostrar los resultados
print(final_data)

# Exportar el resultado a un archivo CSV
write.csv(final_data, "/home/consultorio_grazianimp/buckets/b1/comparaciones/resultado_comparacion_v0_vs_v1.csv", row.names = FALSE)
