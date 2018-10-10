#Questão 1
tamanho <- 10000
M <- 8000
pseudo = function(){
    aleatorios <- c()
    variante <- 7
    a <- 1
    c= 5
    for(i in 1:tamanho){
      variante <- (a*variante + c) %% M
      aleatorios[i] <- variante
    }
    return(aleatorios)
}

tabela <- pseudo()
hist(tabela)

# Teve uma grande ditinção entre a média dos valores entre 0 e 2000 em comparação aos valores entre 2000 e 8000
#Logo observa-se que o comportamento dos dados ocorrem com a inserção do valor a = 1, que permite que esses valores possuam tal comportamento

#Questão 2
tabelaUniforme= function(valor){
  resultado = valor / 8000
  return(resultado)
}

tabelaUniforme = tabelaUniforme(tabela)

inversaFDA = function(y){
  logaritmando =  1- y
  resultado = log(logaritmando) / (-10)
  return(resultado)
}

exponencial <- inversaFDA(tabelaUniforme)
hist(exponencial)

curve(rexp(exponencial, rate=10))

#FAZER CURVA
#Questão 3

poisson <- rpois(1000, 5)
hist(poisson)

#Questão 4

somatorio = function(){
  soma <- c()
  for(i in 1:1000) soma[i]  <- sum(rnorm(100, mean = 5, sd = 1))
  return(soma)
}

soma <- somatorio()
hist(soma)
