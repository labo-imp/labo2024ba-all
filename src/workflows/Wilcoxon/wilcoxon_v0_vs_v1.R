# Cargar los datos de ambos experimentos
data_exp1 <- read.table("/home/consultorio_grazianimp/buckets/b1/flow/wf_julio-001/011-EV_evaluate_conclase_gan/ganancias_01_020.txt", header = TRUE, sep = "\t")
data_exp2 <- read.table("/home/consultorio_grazianimp/buckets/b1/flow/wf_julio-002/011-EV_evaluate_conclase_gan/ganancias_01_069.txt", header = TRUE, sep = "\t")



# Convertir la columna de envíos a numérica
data_exp1$envios <- as.numeric(data_exp1$envios)
data_exp2$envios <- as.numeric(data_exp2$envios)

# Filtrar los envíos múltiplos de 100
multiples_100_exp1 <- data_exp1[data_exp1$envios %% 100 == 0, ]
multiples_100_exp2 <- data_exp2[data_exp2$envios %% 100 == 0, ]

# Asegurarse de que las filas estén alineadas por 'envios' y que ambas tablas tengan la misma cantidad de filas
aligned_data <- merge(multiples_100_exp1, multiples_100_exp2, by = "envios", suffixes = c("_exp1", "_exp2"))

# Inicializar un vector para guardar los p-valores
p_values <- numeric(nrow(aligned_data))

# Calcular el test de Wilcoxon para cada par de envíos
for (i in 1:nrow(aligned_data)) {
  test_result <- wilcox.test(aligned_data[i, "gan_sum_1_exp1"], aligned_data[i, "gan_sum_1_exp2"], 
                             paired = FALSE, alternative = "two.sided")
  p_values[i] <- test_result$p.value
}

# Añadir los p-valores a la tabla
aligned_data$p_value <- p_values

# Seleccionar las columnas que queremos mostrar
final_table <- aligned_data[, c("envios", "gan_sum_1_exp1", "gan_sum_1_exp2", "p_value")]


# Guardar la tabla en formato TXT con tabulaciones
write.table(final_table, "/home/consultorio_grazianimp/buckets/b1/comparaciones/resultado_comparacion_v1_v2.txt", sep = "\t", row.names = FALSE)
