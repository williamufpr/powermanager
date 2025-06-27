#################################################################
#              Unidade 1: Analise de regressao                  #
#           Secao 1.2: Correlacao versus regressao              #
#################################################################

#################################################################
#              Correlacao (Matriz de correlacao)                #
#################################################################

# Usaremos o dataset "imoveiscwbav" que contem uma amostra de 541
# imoveis com preco dos imoveis e as caracteristicas de cada um
# destes imoveis.

# Vamos carregar o dataset
load("imoveiscwbav.RData")

# Vamos ver a base de dados
View(imoveiscwbav)

# Para um exemplo de correlacao entre variaveis, vamos estimar a
# matriz de correlacao das variaveis "price", "parea" e "tarea"
cor(imoveiscwbav[,c("price","parea","tarea")], use="complete")

# price = preco do imovel;
# parea = area privativa do imovel;
# tarea = area total do imovel;

# Percebemos que a correlacao entre "price" e "parea" eh de 0,69
# ou seja, quando a variavel "price" cresce um "1" (uma unidade)
# a variavel "parea" cresce 0.69, ou seja, "parea" cresce 69%
# aproximadamente quando o preço dobra.
# Entre "price" e "tarea" essa mesma relacao eh de aprox. 75%.
# Entre "parea" e "tarea" essa mesma relação eh de aprox. 81%.

# O resultado segue uma logica, de que o preco cresce quando crescem
# as areas privativa e total do imovel; ou seja, quanto maior o 
# imovel(em area privativa e total) maior o preco.

# Perceba que nao existe aqui uma relacao causal, mas sim uma
# associacao linear entre as variaveis.

# Na regressao, que veremos na proxima secao, existe uma relacao 
# explicativa entre as variaveis, para alem de uma associacao linear
# aos pares.
####################################################################