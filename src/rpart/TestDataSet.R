rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")

# Establezco el Working Directory
setwd("~/buckets/b1/")

# cargo los datos,  alternar comentario segun corresponda
dataset <- fread("~/datasets/vivencial_dataset_pequeno.csv")

# Corta las primeras 1000 filas del dataset
testdataset <- dataset[1:1000]

# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("~/buckets/b1/exp/TestYo/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/TestYo/" )

# Guarda el nuevo dataset en un archivo CSV sin comillas en los nombres de las columnas y con ';' como separador
fwrite(testdataset, "testdataset.csv", sep = ";", quote = FALSE)
