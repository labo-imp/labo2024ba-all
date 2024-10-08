
# Generamos 20 números primos. Éstos serán nuestras semillas.

library(primes)

primos <- generate_primes(min = 100000, max = 1000000)

set.seed(100103)


semillitas <- sample(primos, 20 )


# Creamos la función de tiradoras.

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


# Cazatalentos 5: Calculamos el valor esperado de encestes de la candidata usando los resultados conocidos en 
# cada ronda.

resultados5 <- c(68,74,78,70,68,63,80,68,67,65)
valor_esperado_candidata_5 <- mean(resultados5)
valor_esperado_candidata_5


# A partir de ahora vamos a utilizar este valor esperado para calcular las probabilidades de encestar por mera 
# suerte en el resto de los casos.

# Cazatalentos 1: Elegimos a la mejor jugadora con una probabilidad de enceste igual al valor esperado de la
# candidata de la cazatalentos 5. el resto tendrán probabilidades cercanas a 0.5.

taurasi    <- 0.701
peloton    <- ( 501:599 ) / 1000
jugadoras <- c(peloton,taurasi)

resultados1 <- c()

for (s in 1:20){
  set.seed(semillitas[s])
  contador <- 0
  for (i in 1:10000) {
    vaciertos  <- mapply(ftirar, jugadoras, 100)
    sumador <- sum(vaciertos>=80)
    contador  <- contador + sumador
  }
  resultados1[s] <- contador/10000
}

resultados1
mean(resultados1)
paste("La probabilidad de obtener alguna jugadora que haya hecho los 80 encestes por suerte es de",
      mean(resultados1))

# Cazatalentos 2: Elegimos a la mejor jugadora con una probabilidad de enceste igual al valor esperado de la
# candidata de la cazatalentos 5. el resto tendrán probabilidades cercanas a 0.5.

taurasi_2 <- 0.701
peloton2 <- ( 501:599 ) / 1000
peloton2bis <- ( 501:599 ) / 1000
faltante2 <- 0.599
jugadoras2 <- c(peloton2,peloton2bis,faltante2,taurasi_2)


resultados2 <- c()


for (s in 1:20){
  set.seed(semillitas[s])
  contador2 <- 0
  for (i in 1:10000) {
    vaciertos  <- mapply(ftirar, jugadoras2, 100)
    sumador2 <- sum(vaciertos>=80)
    contador2 <- contador2 + sumador2
  }
  resultados2[s] <- contador2/10000
}


resultados2
mean(resultados2)

paste("La probabilidad de obtener alguna jugadora que haya hecho los 80 encestes por suerte es de",
      mean(resultados2))








# Cazatalentos 3: Elegimos a jugadora con una probabilidad de enceste igual al valor esperado de la
# candidata de la cazatalentos 5.

taurasi_3 <- 0.701

resultados3 <- c()

for (s in 1:20){
  set.seed(semillitas[s])
  contador3 <- 0
  for (i in 1:10000) {
    vaciertos  <- mapply(ftirar, taurasi_3, 100)
    if(max(vaciertos)>=80)  contador3  <- contador3 + 1
  }
  resultados3[s] <- contador3/10000
}


resultados3
mean(resultados3)

paste("La probabilidad de que la jugadora haya hecho los 80 encestes por suerte es de",
      mean(resultados3))



# Cazatalentos 4: Elegimos a la mejor jugadora con una probabilidad de enceste igual al valor esperado de la
# candidata de la cazatalentos 5. La otra jugadora tendrá probabilidad de 0.55.

taurasi_4 <- 0.701
jugadora_extra_4 <- 0.55

resultados4 <- c()

jugadoras4 <- c(jugadora_extra_4,taurasi_4)


for (s in 1:20){
  set.seed(semillitas[s])
  contador4 <- 0
  for (i in 1:10000) {
    vaciertos  <- mapply(ftirar, jugadoras4, 100)
    sumador4 <- sum(vaciertos>=80)
    contador4 <- contador4 + sumador4
  }
  resultados4[s] <- contador4/10000
}


resultados4
mean(resultados4)



paste('Probabilidad de suerte de la cazatalentos 1:',mean(resultados1))
paste('Probabilidad de suerte de la cazatalentos 2:',mean(resultados2))
paste('Probabilidad de suerte de la cazatalentos 3:',mean(resultados3))
paste('Probabilidad de suerte de la cazatalentos 4:',mean(resultados4))
