#################################################################
#              Unidade 1: Analise de regressao                  #
#     Secao 1.3: A estimativa de um modelo linear por MQO       #
#################################################################

# Usaremos o dataset "imoveiscwbav" que contem uma amostra de 541
# imoveis com preco dos imoveis e as caracteristicas de cada um
# destes imoveis.

# Vamos carregar o dataset
load("imoveiscwbav.RData")

# Vamos ver a base de dados
View(imoveiscwbav)

# Vamos guardar o dataset em um outro objeto chamado "imoveis"
imoveis <- imoveiscwbav

# Vamos evitar a notacao cientifica nos resultados
options(scipen = 999)

# Vamos agora estimar o modelo linear por MQO com o preco em
# logaritmo neperiano que no "R" eh "log"; o "." quer dizer que 
# estamos regredindo o ln do preco contra todas as demais 
# variaveis, ou seja, todas as demais variaveis do dataset sao
# variaveis explicativas (Xs) no nosso modelo. Estamos guardando
# os resultados da estimativa em um objeto chamado "resultados"
resultados <- lm(log(price)~., data=imoveis)

# Vamos visualizar os resultados com a funcao "summary"
summary(resultados)

qnorm(0.975)
qf(0.95, 19, 521)

# Ao visualizarmos os resultados percebemos que temos varias
# variaveis que explicam o comportamento do preco do imovel.
# O R2 ajustado eh de 0,89 ou seja, as variaveis explicativas
# conseguem explicar 89% das variacoes ocorridas nos precos
# dos imoveis.

# As variaveis com "*" quer dizer que explicam o comportamento 
# do preco com 95% de confianca;
# As variaveis com "**" quer dizer que explicam o comportamento 
# do preco com 99% de confianca;
# As variaveis com "***" quer dizer que explicam o comportamento 
# do preco com 99,9% de confianca;
# Veja que a estatistica "t value" destas variaveis eh sempre 
# maior que "2"

# Este eh apenas um modelo preliminar, temos ainda que testar as 
# algumas hipoteses com respeito aos problemas econometricos e 
# selecionar o melhor modelo por regressao stepwise.

#################################################################
