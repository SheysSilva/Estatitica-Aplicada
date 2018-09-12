#Questão 1
#########################CRIANDO O VETOR POPULACAO###########################
populacao <- c()

for(i in 1:75){
  populacao[i] <- 1
}
for(i in 75:100){
  populacao[i] <- 0
}

######################CRIANDO AMOSTRAS COM REPOSICAO##############################
comReposicao <- TRUE
k <- 1000

#A matriz vai possuir k colunas, onde cada coluna possui 10 elementos(no caso, cada linha é uma amostra). 
n_10 <- 10
amostra1 <- matrix(NA, n_10, k)

n_30 <- 30
amostra2 <- matrix(NA, n_30, k)

n_50 <- 50
amostra3 <- matrix(NA, n_50, k)

for(i in 1:1000){
  amostra1[ ,i] <- sample(populacao, n_10, replace=comReposicao) 
  amostra2[ ,i] <- sample(populacao, n_30, replace=comReposicao)  
  amostra3[ ,i] <- sample(populacao, n_50, replace=comReposicao)  
}

#####################CRIANDO AMOSTRAS SEM REPOSICAO###############################
semReposicao <- FALSE
k <- 1000

#A matriz vai possuir k colunas, onde cada coluna possui 10 elementos(no caso, cada linha é uma amostra). 
n_10 <- 10
amostra4 <- matrix(NA, n_10, k)

n_30 <- 30
amostra5 <- matrix(NA, n_30, k)

n_50 <- 50
amostra6 <- matrix(NA, n_50, k)

for(i in 1:k){
  amostra4[ ,i] <- sample(populacao, n_10, replace=semReposicao) 
  amostra5[ ,i] <-sample(populacao, n_30, replace=semReposicao)  
  amostra6[ ,i] <-sample(populacao, n_50, replace=semReposicao)  
}

#####################MEDIA DAS AMOSTRAS COM REPOSICAO###############################
mediaAmostra1 <- mean(amostra1)
mediaAmostra2 <- mean(amostra2)
mediaAmostra3 <- mean(amostra3)

#####################MEDIA DAS AMOSTRAS SEM REPOSICAO###############################
mediaAmostra4 <- mean(amostra4)
mediaAmostra5 <- mean(amostra5)
mediaAmostra6 <- mean(amostra6)

#####################MEDIA POPULACIONAL###############################
mediaPopulacional <- mean(populacao)

#####################VARIANCIA DAS AMOSTRAS COM REPOSICAO###############################
variancia1 <- var(amostra1)
variancia2 <- var(amostra2)
variancia3 <- var(amostra3)

#####################VARIANCIA DAS AMOSTRAS SEM REPOSICAO###############################
variancia4 <- var(amostra4)
variancia5 <- var(amostra5)
variancia6 <- var(amostra6)

#####################VARIANCIA POPULACIONAL###############################
varianciaPopulacional <- var(populacao)

#####################HISTOGRAMAS DAS AMOSTRAS COM REPOSICAO###############################
variavel1 <- apply(amostra1, 2, mean)
variavel2 <- apply(amostra2, 2, mean)
variavel3 <- apply(amostra3, 2, mean)

hist(variavel1)
hist(variavel2)
hist(variavel3)

#####################HISTOGRAMAS DAS AMOSTRAS SEM REPOSICAO###############################
variavel4 <- apply(amostra4, 2, mean)
variavel5 <- apply(amostra5, 2, mean)
variavel6 <- apply(amostra6, 2, mean)

hist(variavel4)
hist(variavel5)
hist(variavel6)

#####################QUESTAO 2###############################
k <- 1000

#A matriz vai possuir k colunas, onde cada coluna possui 10 elementos(no caso, cada linha é uma amostra). 
n_100 <- 100
amostraPois <- matrix(NA, n_100, k)
lambda <- 10
amostra <- c()
mediaAmostras <- c()
varianciaAmostras <- c()

for(i in 1:k){
  amostra <- rpois(n_100, lambda)
  amostraPois[ ,i] <- amostra
  mediaAmostras[i] <- mean(amostra)
  varianciaAmostras[i] <-var(amostra)
}

hist(mediaAmostras)
hist(varianciaAmostras)

#####################QUESTAO 3###############################

