rm(list = ls())
gc(full = TRUE)

require(data.table)
require(gt)

# Definir el path del script
script_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "")
setwd(script_path)

# Cargar los datos
archivos <- c("deflacion_tb_ganancias.txt", "dolar_blue_tb_ganancias.txt", "Indice_medias_tb_ganancias.txt", 
              "dolar_oficial_tb_ganancias.txt", "estandarizar_tb_ganancias.txt", "rank_simple_tb_ganancias.txt",
              "UVA_tb_ganancias.txt", "ninguno_tb_ganancias.txt", "rank_cero_fijo_tb_ganancias.txt")

# Obtener los nombres de experimentos sin el sufijo
nombres_experimentos <- sub("_tb_ganancias\\.txt$", "", archivos)

lista_datos <- lapply(archivos, function(x) fread(x, dec = ","))

# Modificar cada archivo para eliminar los puntos en las columnas 'm' y convertir a numérico
for (i in 1:length(lista_datos)) {
  data <- lista_datos[[i]]
  
  cols_m <- names(data)[grepl("^m", names(data))]
  data[, (cols_m) := lapply(.SD, function(x) as.numeric(gsub("\\.", "", x))), .SDcols = cols_m]
  
  # Actualizar los datos en la lista
  lista_datos[[i]] <- data
}

# Función para comparar envíos dentro de un archivo
comparar_envios_internos <- function(data) {
  n <- nrow(data)
  p_valores <- matrix(NA, nrow = n, ncol = n, dimnames = list(data$envios, data$envios))
  
  for (j in 1:(n - 1)) {
    for (k in (j + 1):n) {
      envio_j <- data[j, .SD, .SDcols = patterns("^m")]
      envio_k <- data[k, .SD, .SDcols = patterns("^m")]
      
      resultado <- tryCatch({
        wilcox.test(as.numeric(envio_j), as.numeric(envio_k), exact = FALSE)
      }, error = function(e) {
        return(NA)
      })
      
      p_valor <- if (is.list(resultado)) resultado$p.value else NA
      
      # Guardar el p-valor o NA si no es significativo
      if (!is.na(p_valor) && p_valor < 0.05) {
        p_valores[j, k] <- round(p_valor, 3)
        
        promedio_j <- mean(as.numeric(envio_j), na.rm = TRUE)
        promedio_k <- mean(as.numeric(envio_k), na.rm = TRUE)
        mayor <- if (promedio_j > promedio_k) paste0("Mayor: ", data[j, envios]) else paste0("Mayor: ", data[k, envios])
        p_valores[j, k] <- paste0(p_valores[j, k], " (", mayor, ")")
      } else {
        p_valores[j, k] <- "NA"
      }
    }
  }
  
  # Crear una tabla que incluya la cantidad de envíos como la primera columna
  p_valores_df <- as.data.frame(p_valores)
  p_valores_df <- cbind(Cantidad_Envios = data$envios, p_valores_df)
  return(p_valores_df)
}

# Función para comparar envíos entre archivos
comparar_envios_entre_archivos <- function(data1, data2, nombre1, nombre2) {
  p_valores <- matrix(NA, nrow = nrow(data1), ncol = nrow(data2), 
                      dimnames = list(data1$envios, data2$envios))
  
  for (j in 1:nrow(data1)) {
    for (k in 1:nrow(data2)) {
      envio_1 <- data1[j, .SD, .SDcols = patterns("^m")]
      envio_2 <- data2[k, .SD, .SDcols = patterns("^m")]
      
      resultado <- tryCatch({
        wilcox.test(as.numeric(envio_1), as.numeric(envio_2), exact = FALSE)
      }, error = function(e) {
        return(NA)
      })
      
      p_valor <- if (is.list(resultado)) resultado$p.value else NA
      
      # Guardar el p-valor o NA si no es significativo
      if (!is.na(p_valor) && p_valor < 0.05) {
        p_valores[j, k] <- round(p_valor, 3)
        
        promedio_1 <- mean(as.numeric(envio_1), na.rm = TRUE)
        promedio_2 <- mean(as.numeric(envio_2), na.rm = TRUE)
        mayor <- if (promedio_1 > promedio_2) paste0("Mayor: ", nombre1, "-", data1[j, envios]) else paste0("Mayor: ", nombre2, "-", data2[k, envios])
        p_valores[j, k] <- paste0(p_valores[j, k], " (", mayor, ")")
      } else {
        p_valores[j, k] <- "NA"
      }
    }
  }
  
  # Crear una tabla que incluya la cantidad de envíos como la primera columna
  p_valores_df <- as.data.frame(p_valores)
  p_valores_df <- cbind(Cantidad_Envios = data1$envios, p_valores_df)
  return(p_valores_df)
}

# Comparaciones internas del archivo Indice_medias
indice_medias_index <- which(nombres_experimentos == "deflacion")
comparacion_interna_indice_medias <- comparar_envios_internos(lista_datos[[indice_medias_index]])

# Comparaciones entre el archivo Indice_medias y los demás
comparaciones_entre_indice_medias <- list()
for (i in 1:length(lista_datos)) {
  if (i != indice_medias_index) {
    comparaciones_entre_indice_medias[[paste(nombres_experimentos[indice_medias_index], "vs", nombres_experimentos[i], sep = "_")]] <- 
      comparar_envios_entre_archivos(lista_datos[[indice_medias_index]], lista_datos[[i]], 
                                     nombres_experimentos[indice_medias_index], nombres_experimentos[i])
  }
}

# Función para mostrar tabla con estilo personalizado
mostrar_tabla_gt <- function(tabla, titulo, nombre_filas, nombre_columnas) {
  # Reorganizar la tabla para garantizar que los grupos existan
  tabla <- as.data.frame(tabla)
  names(tabla)[1] <- "Cantidad_Envios"
  
  # Crear el gt con el título, colores y estilos personalizados
  gt_tbl <- gt(tabla) %>%
    tab_header(title = titulo) %>%
    fmt_missing(columns = everything(), missing_text = "NA") %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(
      style = list(
        cell_borders(sides = "all", color = "#355D4B", weight = px(2)),  # Bordes personalizados
        cell_fill(color = "#202729"),  # Fondo de la tabla
        cell_text(color = "#C1EDD5")   # Texto de la tabla
      ),
      locations = cells_body(columns = everything())
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#63D297"),  # Texto del título
        cell_borders(sides = "bottom", color = "#C1EDD5", weight = px(2))  # Línea debajo del título
      ),
      locations = cells_title()
    ) %>%
    tab_style(
      style = cell_text(color = "#63D297"),  # Texto de las etiquetas de las columnas
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(color = "#63D297"),  # Texto de las etiquetas de las filas
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_text(color = "#63D297"),  # Texto de la primera columna (Cantidad de envíos)
      locations = cells_stub()  # Para el modelo en la primera columna
    ) %>%
    tab_style(
      style = cell_text(color = "#63D297"),  # Texto de los nombres de los modelos en el encabezado de columnas
      locations = cells_column_labels()  # Para el modelo en los encabezados de columnas
    ) %>%
    tab_options(
      table.background.color = "#202729",  # Fondo de la tabla completo
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(14),
      table.border.top.color = "#355D4B",  # Bordes superiores
      table.border.bottom.color = "#355D4B",  # Bordes inferiores
      column_labels.border.top.color = "#355D4B",  # Borde superior de las etiquetas
      column_labels.border.bottom.color = "#355D4B",  # Borde inferior de las etiquetas
      row_group.border.top.color = "#355D4B",  # Borde superior de los grupos
      row_group.border.bottom.color = "#355D4B"  # Borde inferior de los grupos
    ) %>%
    tab_spanner(label = nombre_columnas, columns = 2:ncol(tabla)) %>%
    tab_row_group(group = nombre_filas, rows = everything())  # Agrupamiento de las filas
  
  # Cambiar el color y negrita del nombre de los modelos en los spanners
  gt_tbl <- gt_tbl %>%
    tab_style(
      style = cell_text(color = "#63D297", weight = "bold"),  # Cambiar color y negrita
      locations = cells_column_spanners(spanners = nombre_columnas)  # Establecer el spanner desde el nombre de columnas
    ) %>%
    tab_style(
      style = cell_text(color = "#63D297", weight = "bold"),  # Cambiar color y negrita
      locations = cells_row_groups()  # Aplicar estilo a los grupos de filas
    )
  
  return(gt_tbl)
}

# Mostrar la comparación interna del archivo Indice_medias
print(mostrar_tabla_gt(comparacion_interna_indice_medias, paste("Comparaciones internas del archivo", nombres_experimentos[indice_medias_index]), 
                       nombres_experimentos[indice_medias_index], nombres_experimentos[indice_medias_index]))

# Mostrar las comparaciones entre Indice_medias y los demás
for (nombre_comparacion in names(comparaciones_entre_indice_medias)) {
  nombre_filas <- strsplit(nombre_comparacion, "_vs_")[[1]][1]
  nombre_columnas <- strsplit(nombre_comparacion, "_vs_")[[1]][2]
  
  print(mostrar_tabla_gt(comparaciones_entre_indice_medias[[nombre_comparacion]], 
                         paste("Comparación entre", nombre_comparacion), nombre_filas, nombre_columnas))
}