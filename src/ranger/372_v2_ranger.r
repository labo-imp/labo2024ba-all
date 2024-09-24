# Ranger  una libreria que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("yaml")

require("ranger")
require("randomForest") # solo se usa para imputar nulos


PARAM <- list()
PARAM$experimento <- 3720

# hiperparámetros de Random Forest
# PARAM$ranger <- list(
#  "num.trees" = 500, # cantidad de arboles
#  "mtry" = 6, # cantidad de atributos que participan en cada split
#  "min.node.size" = 869, # tamaño minimo de las hojas
#  "max.depth" = 20 # 0 significa profundidad infinita
#)

# version dataframe con hiperparametros para hacer loop y que suba automatico
hyperparams <- data.frame(
  num.trees = c(500, 448, 500, 500, 397, 384, 217, 385, 500, 446, 407, 330, 465, 247, 500, 188, 236, 342, 238, 500),
  mtry = c(16, 18, 6, 10, 6, 5, 5, 5, 19, 13, 6, 11, 13, 5, 17, 5, 13, 6, 3, 5),
  min.node.size = c(714, 843, 240, 901, 728, 518, 781, 849, 471, 796, 1000, 875, 820, 163, 982, 544, 939, 320, 667, 384),
  max.depth = c(16, 20, 25, 10, 21, 18, 13, 26, 19, 12, 25, 25, 23, 29, 14, 30, 26, 24, 27, 27)
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/") # Establezco el Working Directory

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# cargo los datos
dataset <- fread( miAmbiente$dataset_pequeno )


# asigno un valor muy negativo
#  estas dos lineas estan relacionadas con el Data Drifting
if( "Master_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Master_Finiciomora) , Master_Finiciomora := -999 ]

if( "Visa_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Visa_Finiciomora) , Visa_Finiciomora :=  -999 ]

# defino donde entreno y donde aplico el modelo
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]



set.seed( miAmbiente$semilla_primigenia ) # Establezco la semilla aleatoria


# ranger necesita la clase de tipo factor
factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temias a los nulos?
dtrain <- na.roughfix(dtrain)

setorder(dtrain, clase_ternaria) # primero quedan los BAJA+1, BAJA+2, CONTINUA

for (i in 1:nrow(hyperparams)) {
  # Extraer los hiperparámetros para esta iteración
  loop.num.trees = hyperparams$num.trees[i]
  loop.mtry = hyperparams$mtry[i]
  loop.min.node.size = hyperparams$min.node.size[i]
  loop.max.depth = hyperparams$max.depth[i]
  

  # genero el modelo de Random Forest llamando a ranger()
  modelo <- ranger(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    probability = TRUE, # para que devuelva las probabilidades
    num.trees = loop.num.trees,
    mtry = loop.mtry,
    min.node.size = loop.min.node.size,
    max.depth = loop.max.depth
  )
  
  
  # Carpinteria necesaria sobre  dapply
  # como quiere la Estadistica Clasica, imputar nulos por separado
  # ( aunque en este caso ya tengo los datos del futuro de anteman
  #  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
  dapply[, clase_ternaria := NULL]
  dapply <- na.roughfix(dapply)
  
  
  # aplico el modelo recien creado a los datos del futuro
  prediccion <- predict(modelo, dapply)
  
  # Genero la entrega para Kaggle
  entrega <- as.data.table(list(
    "numero_de_cliente" = dapply[, numero_de_cliente],
    "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
  )) # genero la salida
  
  
  
  nom_arch_kaggle <- "KA3720_001.csv"
  
  # genero el archivo para Kaggle
  fwrite(entrega,
    file = nom_arch_kaggle,
    sep = ","
  )
  
  
  # subo a Kaggle
  # preparo todo para el submit
  comentario <- paste0( "'",
    "num.trees=", loop.num.trees,
    " mtry=", loop.mtry,
    " min.node.size=", loop.min.node.size,
    " max.depth=", loop.max.depth,
    "'"
  )
  
  comando <- paste0( "~/install/proc_kaggle_submit.sh ",
    "TRUE ",
    miAmbiente$modalidad, " ",
    nom_arch_kaggle, " ",
    comentario
  )
  
  ganancia <- system( comando, intern=TRUE )
  
  cat( paste0( ganancia, "\t", nom_arch_kaggle, "\n"),
    file="tb_ganancias.txt",
    append=TRUE
  )

} 
