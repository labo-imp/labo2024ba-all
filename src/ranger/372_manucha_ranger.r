# Limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("yaml")
require("ranger")
require("randomForest") # Solo se usa para imputar nulos

PARAM <- list()
PARAM$experimento <- 3720

# Definición de los valores ampliados para cada hiperparámetro
num_trees_values <- seq(from = 500, to = 1000, by = 30)
mtry_values <- seq(from = 10, to = 14, by = 1)
min_node_size_values <- seq(from = 10, to = 70, by = 10)
max_depth_values <- seq(from = 5, to = 50, by = 5)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/") # Establezco el Working Directory

# Creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(carpeta_experimento, showWarnings = FALSE)

setwd(carpeta_experimento)

# Cargo miAmbiente
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")

# Cargo los datos
dataset <- fread(miAmbiente$dataset_pequeno)

# Asigno un valor muy negativo
if ("Master_Finiciomora" %in% colnames(dataset))
  dataset[is.na(Master_Finiciomora), Master_Finiciomora := -999]

if ("Visa_Finiciomora" %in% colnames(dataset))
  dataset[is.na(Visa_Finiciomora), Visa_Finiciomora := -999]

# Defino donde entreno y donde aplico el modelo
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

set.seed(miAmbiente$semilla_primigenia) # Establezco la semilla aleatoria

# Ranger necesita la clase de tipo factor
factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# Imputo los nulos
dtrain <- na.roughfix(dtrain)
setorder(dtrain, clase_ternaria)

# Inicializo variables para el promedio de ganancias
num_combinaciones <- 1
ganancia_maxima <- 57.457

# Guardo la ganancia inicial en el archivo de texto
cat(paste0(ganancia_init, "\t", "KA3720_500_10_10_10.csv", "\n"),
    file = "tb_ganancias.txt",
    append = TRUE
)

# Iteración sobre las combinaciones de hiperparámetros
for (num_trees in num_trees_values) {
  for (mtry in mtry_values) {
    for (min_node_size in min_node_size_values) {
      for (max_depth in max_depth_values) {
        
        # Genero el modelo de Random Forest llamando a ranger()
        modelo <- ranger(
          formula = "clase_ternaria ~ .",
          data = dtrain,
          probability = TRUE,
          num.trees = num_trees,
          mtry = mtry,
          min.node.size = min_node_size,
          max.depth = max_depth
        )
        
        # Carpintería necesaria sobre dapply
        dapply[, clase_ternaria := NULL]
        dapply <- na.roughfix(dapply)
        
        # Aplico el modelo recién creado a los datos del futuro
        prediccion <- predict(modelo, dapply)
        
        # Genero la entrega para Kaggle
        entrega <- as.data.table(list(
          "numero_de_cliente" = dapply[, numero_de_cliente],
          "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
        ))
        
        # Genero el archivo para Kaggle
        nom_arch_kaggle <- paste0("KA3720_", num_trees, "_", mtry, "_", min_node_size, "_", max_depth, ".csv")
        fwrite(entrega, file = nom_arch_kaggle, sep = ",")
        
        # Preparo todo para el submit
        comentario <- paste0("'",
                             "num.trees=", num_trees,
                             " mtry=", mtry,
                             " min.node.size=", min_node_size,
                             " max.depth=", max_depth,
                             "'"
        )
        
        comando <- paste0("~/install/proc_kaggle_submit.sh ",
                          "TRUE ",
                          miAmbiente$modalidad, " ",
                          nom_arch_kaggle, " ",
                          comentario
        )
        
        # Ejecutar el comando para obtener la ganancia
        ganancia <- as.numeric(system(comando, intern = TRUE))
        
        # Solo si la ganancia supera el promedio, se actualizan estadísticas y se sube a Kaggle
        if (ganancia > promedio_ganancias) {
          # Actualizo el cálculo del promedio de las ganancias
          num_combinaciones <- num_combinaciones + 1
          total_ganancia <- total_ganancia + ganancia
          promedio_ganancias <- total_ganancia / num_combinaciones
          
          # Subo a Kaggle
          cat(paste0("Subiendo a Kaggle: ", nom_arch_kaggle, " con ganancia: ", ganancia, "\n"))
          system(comando)
          
          # Guardo la ganancia y el nombre del archivo en el archivo de texto
          cat(paste0(ganancia, "\t", nom_arch_kaggle, "\n"),
              file = "tb_ganancias.txt",
              append = TRUE
          )
        } else {
          # Mensaje de ganancia ignorada
          cat(paste0("Ganancia: ", ganancia, " no supera el promedio: ", promedio_ganancias, "\n"))
        }
      }
    }
  }
}
