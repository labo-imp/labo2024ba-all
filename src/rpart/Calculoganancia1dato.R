# Limpiar el entorno
rm(list = ls())
gc()

# Cargar las librerías necesarias
require("data.table")
require("rpart")

# Configuración de los parámetros específicos
PARAM <- list()
PARAM$semilla_primigenia <- 480107
PARAM$training_pct <- 70L  # porcentaje de datos para entrenamiento

# Seleccionar el dataset
PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

# Función para particionar el dataset
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

# Función para estimar la ganancia
ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  # Particionar el dataset estratificadamente
  particionar(dataset,
              division = c(training_pct, 100L - training_pct), 
              agrupa = "clase_ternaria",
              seed = semilla
  )
  
  # Generar el modelo de árbol
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], # 70% para entrenamiento
                  xval = 0,
                  control = param_basicos
  )
  
  # Aplicar el modelo a los datos de prueba
  prediccion <- predict(modelo, 
                        dataset[fold == 2], # 30% para prueba
                        type = "prob"
  )
  
  # Calcular la ganancia en los datos de prueba
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  # Escalar la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / ((100 - training_pct) / 100)
  
  return(c(
    "semilla" = semilla,
    param_basicos,
    "ganancia_test" = ganancia_test_normalizada
  ))
}

# Configurar los parámetros específicos para el cálculo
param_basicos <- list(
  "cp" = -1,
  "maxdepth" = 18,
  "minsplit" = 92,
  "minbucket" = 46
)

# Establecer el directorio de trabajo
setwd("~/buckets/b1/")

# Cargar el dataset
dataset <- fread(PARAM$dataset_nom)
dataset <- dataset[clase_ternaria != ""]  # Filtrar datos con clase

# Calcular la ganancia con la semilla específica
ganancia_resultado <- ArbolEstimarGanancia(
  semilla = PARAM$semilla_primigenia,
  training_pct = PARAM$training_pct,
  param_basicos = param_basicos
)

# Imprimir el resultado
print(ganancia_resultado)

# Guardar el resultado en un archivo
resultado_df <- as.data.table(ganancia_resultado)
fwrite(resultado_df,
       file = "ganancia_resultado.csv",
       sep = ",",
       quote = TRUE)
