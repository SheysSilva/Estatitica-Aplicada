#POPULACAO#
desvio <- 10
medio <- 500
tamanho <- 10000
#AMOSTRAS#
n_10 <- 10
n_20 <- 20
n_30 <- 30 
n_50 <- 50
n_100 <- 100

matrix()

amostra_n_10 <- matrix(NA, tamanho, n_10)
amostra_n_20 <- matrix(NA, tamanho, n_20)
amostra_n_30 <- matrix(NA, tamanho, n_30)
amostra_n_50 <- matrix(NA, tamanho, n_50)
amostra_n_100 <- matrix(NA, tamanho, n_100)

for(i in 1:10000){
  amostra_n_10[i, ] <- rnorm(n_10, mean = medio, sd = desvio)
  amostra_n_20[i, ] <- rnorm(n_20, mean = medio, sd = desvio)
  amostra_n_30[i, ] <- rnorm(n_30, mean = medio, sd = desvio)
  amostra_n_50[i, ] <- rnorm(n_50, mean = medio, sd = desvio)
  amostra_n_100[i, ] <- rnorm(n_100, mean = medio, sd = desvio)
}

S2_function = function(amostra, n, media){
  somatorio_quadratico <- 0
  for(i in 1:n){
    somatorio_quadratico <- somatorio_quadratico + (amostra[i] - media)**2
  }
  
  return(somatorio_quadratico/(n-1))
}

O2_function = function(amostra, n, media){
  somatorio_quadratico <- 0
  for(i in 1:n){
    somatorio_quadratico <- somatorio_quadratico + (amostra[i] - media)**2
  }
  
  return(somatorio_quadratico/n)
}

S_quadrado <- matrix(NA, tamanho, 5)
O_quadrado <- matrix(NA, tamanho, 5)

for(i in 1:10000){
  S_quadrado[i, 1] <- S2_function(amostra_n_10[i, ], n_10, mean(amostra_n_10[i, ]))
  S_quadrado[i, 2] <- S2_function(amostra_n_20[i, ], n_20, mean(amostra_n_20[i, ])) 
  S_quadrado[i, 3] <- S2_function(amostra_n_30[i, ], n_30, mean(amostra_n_30[i, ])) 
  S_quadrado[i, 4] <- S2_function(amostra_n_50[i, ], n_50, mean(amostra_n_50[i, ])) 
  S_quadrado[i, 5] <- S2_function(amostra_n_100[i, ], n_100, mean(amostra_n_100[i, ])) 
  
  O_quadrado[i, 1] <- O2_function(amostra_n_10[i, ], n_10, mean(amostra_n_10[i, ]))
  O_quadrado[i, 2] <- O2_function(amostra_n_20[i, ], n_20, mean(amostra_n_20[i, ])) 
  O_quadrado[i, 3] <- O2_function(amostra_n_30[i, ], n_30, mean(amostra_n_30[i, ])) 
  O_quadrado[i, 4] <- O2_function(amostra_n_50[i, ], n_50, mean(amostra_n_50[i, ])) 
  O_quadrado[i, 5] <- O2_function(amostra_n_100[i, ], n_100, mean(amostra_n_100[i, ])) 
}

hist(S_quadrado[,1])
hist(S_quadrado[,2])
hist(S_quadrado[,3])
hist(S_quadrado[,4])
hist(S_quadrado[,5])

hist(O_quadrado[,1])
hist(O_quadrado[,2])
hist(O_quadrado[,3])
hist(O_quadrado[,4])
hist(O_quadrado[,5])

mean(S_quadrado[,1])
mean(S_quadrado[,2])
mean(S_quadrado[,3])
mean(S_quadrado[,4])
mean(S_quadrado[,5])

mean(O_quadrado[,1])
mean(O_quadrado[,2])
mean(O_quadrado[,3])
mean(O_quadrado[,4])
mean(O_quadrado[,5])

var(S_quadrado[,1])
var(S_quadrado[,2])
var(S_quadrado[,3])
var(S_quadrado[,4])
var(S_quadrado[,5])

var(O_quadrado[,1])
var(O_quadrado[,2])
var(O_quadrado[,3])
var(O_quadrado[,4])
var(O_quadrado[,5])

#O valor da media e variancia de S**2, conforme aumentam o tamanho da amostra, 
#eles tendem a se aproximar mais rapido do valor da população do que os valores de media e variancia de sigma**2

#QUESTÃO2#
amostra <- c()
t1 <- c()
t2 <- c()

T1 = function(media){
  return(2*media)
}

T2 = function(amostra, n){
  resultado <- ((n+1)/n)*max(amostra)
  return(resultado)
}

for(i in 1:10000){
  amostra <- runif(n_100, min = 0, max = 500)
  t1[i] <- T1(mean(amostra))
  t2[i] <- T2(amostra, n_100)
}


Vies = function(amostra){
  return(mean(amostra) - 500)
}

Erro = function(amostra, variancia){
  erro <- variancia + (Vies(amostra))**2
  return(erro)
}


vies_t1 <- Vies(t1)
vies_t2 <- Vies(t2)

erro_t1 <-Erro(t1, var(t1))
erro_t2 <-Erro(t2, var(t2))

hist(t1)
hist(t2)

