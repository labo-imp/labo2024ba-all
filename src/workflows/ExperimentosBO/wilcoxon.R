# Limpio memoria
rm(list=ls())
options(scipen = 999)

library(rstudioapi)
script_full_path <- rstudioapi::getSourceEditorContext()$path
script_path <- file.path(dirname(script_full_path), "")
setwd(script_path)

# Cargar la librería necesaria
library(data.table)

# Función para procesar y obtener el mejor vector basado en la media
procesar_archivo_dt <- function(ruta_archivo) {
  # Leer el archivo
  datos <- fread(ruta_archivo)
  
  # Filtrar los envíos que sean múltiplos de 100 y hasta 10000
  datos_filtrados <- datos[envios %% 100 == 0 & envios <= 10000]
  
  # Seleccionar las columnas que queremos considerar para calcular la media
  columnas_media <- setdiff(names(datos), c("envios", "gan_sum_1", "gan_suavizada"))
  
  # Función para calcular la media de las columnas seleccionadas en una fila
  obtener_media_fila <- function(fila) {
    return(mean(as.numeric(fila)))
  }
  
  # Aplicar la función de media a cada fila
  datos_filtrados[, media := apply(.SD, 1, obtener_media_fila), .SDcols = columnas_media]
  
  # Obtener la fila con la mayor media
  mejor_fila <- datos_filtrados[which.max(media)]
  
  # Retornar la fila con la mayor media (sin la columna extra 'media')
  return(mejor_fila[, !'media', with = FALSE])
}

# Función para realizar el test de Wilcoxon
realizar_test_wilcoxon <- function(mejor_vector, archivo_ganancias, nombre_salida) {
  # Leer el archivo de ganancias
  datos_ganancias <- fread(archivo_ganancias)
  
  # Filtrar los envíos que sean múltiplos de 100 y hasta 10000
  datos_filtrados <- datos_ganancias[envios %% 100 == 0 & envios <= 10000]
  
  # Seleccionar las columnas para el test (excluyendo "envios", "gan_sum_1" y "gan_suavizada")
  columnas_test <- setdiff(names(datos_filtrados), c("envios", "gan_sum_1", "gan_suavizada"))
  
  # Realizar el test de Wilcoxon para cada fila del archivo contra el mejor vector
  p_values <- apply(datos_filtrados[, ..columnas_test], 1, function(fila) {
    wilcox.test(as.numeric(fila), as.numeric(mejor_vector), paired = TRUE, exact = FALSE)$p.value
  })
  
  # Crear tabla con los resultados y agregar los p-valores
  resultados <- datos_filtrados[, .(envios, gan_sum_1)]
  resultados[, p_value := p_values]
  
  # Guardar los resultados en un archivo
  fwrite(resultados, nombre_salida, sep = "\t")
  
  return(paste("Resultados guardados en:", nombre_salida))
}

# Especificar las rutas de los archivos originales
rutas_archivos <- c(
  "/home/icelaye363/labo2024ba/src/workflows/ExperimentosBO/wf_julio-060/011-EV_evaluate_conclase_gan/ganancias_01_012.txt",
  "/home/icelaye363/labo2024ba/src/workflows/ExperimentosBO/wf_julio-061/011-EV_evaluate_conclase_gan/ganancias_01_058.txt",
  "/home/icelaye363/labo2024ba/src/workflows/ExperimentosBO/wf_julio-062/011-EV_evaluate_conclase_gan/ganancias_01_701.txt"
)

# Crear una lista para almacenar las rutas de los archivos de mejores vectores
rutas_mejores_vectores <- list()

# Procesar cada archivo y obtener el mejor vector
for (i in 1:length(rutas_archivos)) {
  mejor_vector <- procesar_archivo_dt(rutas_archivos[i])
  
  # Guardar el mejor vector en un archivo
  ruta_salida_mejor_vector <- paste0("mejorvector_", gsub(".txt", "", basename(rutas_archivos[i])), ".txt")
  fwrite(mejor_vector, ruta_salida_mejor_vector, sep = "\t")
  
  # Almacenar la ruta del mejor vector
  rutas_mejores_vectores[[i]] <- ruta_salida_mejor_vector
}

# Realizar los tests de Wilcoxon comparando cada mejor vector contra los archivos base
for (i in 1:length(rutas_mejores_vectores)) {
  # Leer el mejor vector
  mejor_vector <- fread(rutas_mejores_vectores[[i]])[, -c("envios", "gan_sum_1", "gan_suavizada"), with = FALSE]
  
  # Comparar contra los archivos originales que no corresponden al mejor vector
  for (j in setdiff(1:length(rutas_archivos), i)) {
    nombre_salida <- paste0("wilcoxon_", gsub("ganancias_01_", "", basename(rutas_archivos[j])), "_vs_", gsub("resultado_", "", gsub(".txt", "", rutas_mejores_vectores[[i]])), ".txt")
    
    # Realizar el test de Wilcoxon y guardar los resultados
    realizar_test_wilcoxon(mejor_vector, rutas_archivos[j], nombre_salida)
  }
}