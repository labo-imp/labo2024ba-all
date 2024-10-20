# Verifica si el archivo está en el directorio correcto
list.files()

# Si confirmas que el archivo está ahí, procede a cargarlo
file_path <- "conceptual_competencia_2024.csv.gz"
data <- read_csv(file_path)

# Resto del análisis
summary(data)
dim(data)
colnames(data)

# Conteo de valores nulos por columna
missing_data <- data %>%
  summarise_all(~ sum(is.na(.)))

print(missing_data)

# Gráfico de barras para visualizar los valores nulos por columna
library(ggplot2)
missing_data_long <- gather(missing_data, key = "Column", value = "Missing_Values")
ggplot(missing_data_long, aes(x = reorder(Column, -Missing_Values), y = Missing_Values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Conteo de Valores Nulos por Columna",
       x = "Columnas",
       y = "Número de Valores Nulos") +
  theme_minimal()