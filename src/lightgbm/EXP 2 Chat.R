# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("yaml")
require("rlist")

# Defino la función loguear
loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(substitute(reg), ext)
  
  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    cat(linea, file = archivo)
  }
  
  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)
  
  # imprimo por pantalla
  if (verbose) cat(linea)
}

# defino los parametros de la corrida
PARAM <- list()
PARAM$experimento <- "KA4210"
PARAM$input$training <- c(202107)
PARAM$input$future <- c(202109)

PARAM$finalmodel$num_iterations <- 1500
PARAM$finalmodel$learning_rate <- 0.01  # Tasa de aprendizaje un poco más suave
PARAM$finalmodel$feature_fraction <- 0.9  # Incluir un poco más de características en cada iteración
PARAM$finalmodel$min_data_in_leaf <- 50  # Capturar más patrones reduciendo el número mínimo en hoja
PARAM$finalmodel$num_leaves <- 31  # Aumentar levemente el número de hojas para captar más complejidad
PARAM$finalmodel$max_bin <- 31  # Bajar discretización para simplificar el ajuste del modelo
PARAM$finalmodel$lambda_l1 <- 1.75  # Aumento ligero para mayor regularización L1
PARAM$finalmodel$lambda_l2 <- 3.0  # Aumento en L2 para evitar sobreajuste
PARAM$finalmodel$bagging_fraction <- 0.86  # Mantener bagging fracción
PARAM$finalmodel$bagging_freq <- 5  # Aumentar frecuencia de bagging para mayor robustez
PARAM$finalmodel$max_depth <- 6  # Limitar la profundidad para evitar sobreajuste
PARAM$finalmodel$min_gain_to_split <- 0.05  # Control para evitar divisiones sin ganancia significativa
PARAM$finalmodel$min_sum_hessian_in_leaf <- 6  # Regularización más suave para hojas
PARAM$finalmodel$n_estimators <- 2300  # Similar al número de iteraciones
PARAM$finalmodel$cat_smooth <- 25   # Mantener suavizado para variables categóricas

# Cargo el dataset
setwd("~/buckets/b1")
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")
dataset <- fread(miAmbiente$dataset_pequeno, stringsAsFactors = TRUE)

# Paso clase a binaria
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# Divido en conjunto de entrenamiento y validación
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]
set.seed(miAmbiente$semilla_primigenia)
train_idx <- dataset[train == 1L, sample(.I, .N * 0.8)]
valid_idx <- setdiff(dataset[train == 1L, .I], train_idx)

dtrain <- lgb.Dataset(data.matrix(dataset[train_idx, campos_buenos, with = FALSE]), label = dataset[train_idx, clase01])
dvalid <- lgb.Dataset(data.matrix(dataset[valid_idx, campos_buenos, with = FALSE]), label = dataset[valid_idx, clase01])

# Genero el modelo con validación y early stopping
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    num_iterations = PARAM$finalmodel$num_iterations,
    learning_rate = PARAM$finalmodel$learning_rate,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    num_leaves = PARAM$finalmodel$num_leaves,
    max_bin = PARAM$finalmodel$max_bin,
    lambda_l1 = PARAM$finalmodel$lambda_l1,
    lambda_l2 = PARAM$finalmodel$lambda_l2,
    bagging_fraction = PARAM$finalmodel$bagging_fraction,
    bagging_freq = PARAM$finalmodel$bagging_freq,
    max_depth = PARAM$finalmodel$max_depth,
    min_gain_to_split = PARAM$finalmodel$min_gain_to_split,
    min_sum_hessian_in_leaf = PARAM$finalmodel$min_sum_hessian_in_leaf,
    n_estimators = PARAM$finalmodel$n_estimators,
    cat_smooth = PARAM$finalmodel$cat_smooth,
    seed = miAmbiente$semilla_primigenia
  ),
  valids = list(valid = dvalid),
  early_stopping_rounds = 100
)

# Importancia de las variables
tb_importancia <- as.data.table(lgb.importance(modelo))
fwrite(tb_importancia, file = "impo.txt", sep = "\t")

# Grabo el modelo
lgb.save(modelo, "modelo.txt")

# Aplico el modelo a los datos futuros
dapply <- dataset[foto_mes == PARAM$input$future]
prediccion <- predict(modelo, data.matrix(dapply[, campos_buenos, with = FALSE]))

# Genero tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]
fwrite(tb_entrega, file = "prediccion.txt", sep = "\t")

# Ordeno por probabilidad descendente y genero archivos de envíos
setorder(tb_entrega, -prob)
cortes <- seq(9000, 13500, by = 500)

for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  nom_arch_kaggle <- paste0(PARAM$experimento, "_", envios, ".csv")
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)], file = nom_arch_kaggle, sep = ",")
  
  comentario <- paste0(
    "'envios=", envios,
    " num_iterations=", PARAM$finalmodel$num_iterations,
    " learning_rate=", PARAM$finalmodel$learning_rate,
    " num_leaves=", PARAM$finalmodel$num_leaves,
    " min_data_in_leaf=", PARAM$finalmodel$min_data_in_leaf,
    " feature_fraction=", PARAM$finalmodel$feature_fraction, "'"
  )
  
  comando <- paste0("~/install/proc_kaggle_submit.sh TRUE ", miAmbiente$modalidad, " ", nom_arch_kaggle, " ", comentario)
  ganancia <- system(comando, intern = TRUE)
  
  linea <- c(list("ganancia" = ganancia), PARAM$finalmodel)
  loguear(linea, arch = "tb_ganancias.txt")
}

cat("\n\nSe han realizado los submits a Kaggle\n")
