library(reticulate)
library(data.table)

# Cargar Optuna desde Python
optuna <- import("optuna")

# Definir la función para loguear los resultados en un archivo
loguear <- function(reg, arch = "HT4222.txt") {
  if (!file.exists(arch)) {
    encabezado <- paste0(
      "fecha\tobjective\tmetric\tfirst_metric_only\tboost_from_average\tfeature_pre_filter\tverbosity\tmax_bin\tnum_iterations\tforce_row_wise\tseed\tmax_depth\tlearning_rate\tnum_leaves\tfeature_fraction\tmin_data_in_leaf\tenvios\tmin_gain_to_split\tlambda_l1\tlambda_l2\tbagging_fraction\tbagging_freq\tcat_smooth\tganancia\titeracion\n"
    )
    cat(encabezado, file = arch)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = arch, append = TRUE)
}

# Definir la función objetivo para la optimización con Optuna
objective <- function(trial) {
  
  # Definir los hiperparámetros a optimizar
  params <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    max_bin = 31,
    num_iterations = 9999,
    force_row_wise = TRUE,
    seed = ksemilla_azar1,
    max_depth = trial$suggest_int("max_depth", -1, 20),
    learning_rate = trial$suggest_loguniform("learning_rate", 0.0001, 0.1),
    num_leaves = trial$suggest_int("num_leaves", 8, 1024),
    feature_fraction = trial$suggest_uniform("feature_fraction", 0.1, 1.0),
    min_data_in_leaf = trial$suggest_int("min_data_in_leaf", 10, 2000),
    envios = trial$suggest_int("envios", 7000, 17000),
    min_gain_to_split = trial$suggest_uniform("min_gain_to_split", 0.0, 0.5),
    lambda_l1 = trial$suggest_uniform("lambda_l1", 0.0, 1.5),
    lambda_l2 = trial$suggest_uniform("lambda_l2", 0.0, 2.0),
    bagging_fraction = trial$suggest_uniform("bagging_fraction", 0.5, 1.0),
    bagging_freq = trial$suggest_int("bagging_freq", 1, 15),
    cat_smooth = trial$suggest_uniform("cat_smooth", 1.0, 13.0)
  )
  
  # Usar tu función de evaluación con LightGBM para calcular la ganancia
  ganancia <- EstimarGanancia_lightgbm(params)
  
  # Loguear los parámetros y la ganancia
  reg <- c(params, ganancia = ganancia, iteracion = trial$number)
  loguear(reg)
  
  return(ganancia)
}

# Crear el estudio y realizar la optimización
study <- optuna$create_study(direction = "maximize")
study$optimize(objective, n_trials = 100)

# Obtener los mejores hiperparámetros
best_params <- study$best_params
print(best_params)

# Loguear los mejores hiperparámetros
loguear(c(best_params, ganancia = "N/A", iteracion = "best"), arch = "HT4222.txt")
