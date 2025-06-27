#################################################################
#               Unidade 1: Analise de regressao                 #
#     Secao 1.6: A predicao com modelos lineares por MQO        # 
#################################################################

# Vamos instalar os pacotes necessarios
# install.packages("performance")

# Vamos carregar os pacotes necessarios
library(performance)

# Lembre-se de que este exemplo eh uma sequencia do exercicio
# que estamos fazendo desde a secao de estudos passada (1.5)
# portanto para dar continuidade a este exercicio voce deve
# primeiro executar todos os comandos que estao em:
# EST2_Rotina_Secao_1_5.R

# Primeiramente vamos observar os indicadores de performace
# do nosso modelo

# Lembre-se que:
# Menores valores de AIC, BIC, Sigma e RMSE sao desejaveis
# Maiores valores de R2 e R2(adj) sao melhores
# Sigma eh o Erro Padrao dos Residuos
# RMSE eh a raiz quadrada dos erros medios (menor eh melhor)

# Vamos ver esses indicadores no nosso ultimo modelo com a
# heterocedasticidade minimizada
model_performance(resultados1)

# Alguns indicadores nao mudam no modelo com ou sem 
# heterocedasticidade minimizada, por isso o comando acima 
# nao mostra tudo. Vamos ver no nosso modelo antes da 
# correcao da heterocedasticidade
model_performance(resultados)

# Os indicadores AIC, AICc, BIC RMSE e Sigma servem para 
# comparar este modelo com outros modelos. O R2 e R2(adj.)
# sao indicadores que podemos interpretar isoladamente
# O R2 eh 0.898 o que significa que o nosso modelo esta
# explicando quase 90% do preco do imovel, algo parecido
# com o R2 ajustado que considera o tamanho da amostra e 
# o numero de variveis incluidas no modelo. Portanto, o
# nosso modelo explica muito bem (excelente) o preco do
# imovel.

# Se desejarmos ver os residuos do modelo, podemos fazer:
resultados$residuals

# Se desejarmos inclui-los no dataset e ve-los fazemos:
imoveis<- within(imoveis, 
                 {residuos <- residuals(resultados) })
View(imoveis)
# Jah testamos a normalidade dos residuos na secao de
# estudos anterior, sabemos que sao normalmente distriuidos
# mas vamos repetir:
# As hipoteses do teste:
# H0: A amostra eh normalmente distribuida
# Ha: A amostra nao eh normalmente distribuida

# O teste:
shapiro.test(resultados$residuals)
# P-value eh maior que 0.05, logo os residuos sao normalmente
# distribuidos

# Se desejarmos ver os valores preditos (Y chapeu) fazemos:
resultados$fitted.values

# Se desejarmos inclui-los no dataset e ve-los fazemos:
imoveis<- within(imoveis, 
                 {preditos <- fitted.values(resultados)})
View(imoveis)
# lembre-se que estao em logaritmo neperiano

# Vamos estimar os intervalos de confianca com 95% de 
# confianca com erros padroes dos parametros corrigidos de
# heterocedasticidade
confint <- confint (resultados1, level = 0.95)
confint
# 2,5% sao os parametros de valor minimo e 97,5% sao os 
# parametros de valor maximo

# Vamos salvar os valores aos quais desejamos fazer a predicao
# em variaveis
age = 14 # anos
parea = 120 # (metros quadrados de area privativa)
tarea = 180 # (metros quadrados de area total)
bath = 2 # (banheiros)
ensuit = 1 # (tem uma suite)
garag = 1  # (tem uma garagem)
park = 2 # (distancia em km de um parque)
balc = 1 # (variavel binaria, sigifica que tem sacada)
elev = 1 # (variavel binaria, tem elevador)
fitg = 1 # (variavel binaria, tem academia no condominio)
party = 1 # (variavel binaria, tem area de festas no condom.)
categ = 1 # (variavel binaria, eh um apartamento; zero= casa)

# Vamos fazer as predicoes a partir de um dataframe das
# variaveis que acabamos de salvar
predito <- predict(object = resultados,
                   data.frame(age=age, parea=parea, 
                              tarea=tarea, bath=bath,
                              ensuit=ensuit, garag=garag, 
                              park=park, balc=balc,
                              elev=elev, fitg=fitg, 
                              party=party, categ=categ))

# O preco medio do imovel estimado eh:
predmedfinal <- exp(predito)
predmedfinal
# R$712.814,70
# Aplicamos o antilog porque (lembre-se) o preço do imovel 
# no modelo esta em logaritmo neperiano).

# Vamos estimar o preco minimo do intervalo de confianca
# (95% de confianca)
Lestimate=confint[1,1]+age*confint[2,1]+parea*confint[3,1]+
  tarea*confint[4,1]+bath*confint[5,1]+ensuit*confint[6,1]+
  garag*confint[7,1]+park*confint[8,1]+balc*confint[9,1]+
  elev*confint[10,1]+fitg*confint[11,1]+party*confint[12,1]+
  categ*confint[13,1]  
Lestimate
exp(Lestimate)
# R$335.334,30

# Vamos estimar o preco maximo do intervalo de confianca
# (95% de confianca)
Uestimate=confint[1,2]+age*confint[2,2]+parea*confint[3,2]+
  tarea*confint[4,2]+bath*confint[5,2]+ensuit*confint[6,2]+
  garag*confint[7,2]+park*confint[8,2]+balc*confint[9,2]+
  elev*confint[10,2]+fitg*confint[11,2]+party*confint[12,2]+
  categ*confint[13,2]  
Uestimate
exp(Uestimate)
# R$1.515.219,00

# Entao o preco medio estimado eh R$712.814,70, e pode
# variar entre R$335.334,30 e R$1.515.219,00

# O intervalo eh muito amplo; vamos fazer a estimativa para
# um intervalo de confianca de 80% de confianca:
confint80 <- confint (resultados1, level = 0.80)
confint80

predito80 <- predict(object = resultados,
                   data.frame(age=age, parea=parea, 
                              tarea=tarea, bath=bath,
                              ensuit=ensuit, garag=garag, 
                              park=park, balc=balc,
                              elev=elev, fitg=fitg, 
                              party=party, categ=categ))

# O preco medio do imovel estimado eh:
predmedfinal80 <- exp(predito80)
predmedfinal80
# R$712.814,70
# eh o mesmo valor porque aqui nao usamos o intervalo de 
# confianca

# Vamos estimar o preco minimo do intervalo de confianca
# (80% de confianca)
Lestimate80=confint80[1,1]+age*confint80[2,1]+
  parea*confint80[3,1]+tarea*confint80[4,1]+
  bath*confint80[5,1]+ensuit*confint80[6,1]+
  garag*confint80[7,1]+park*confint80[8,1]+
  balc*confint80[9,1]+
  elev*confint[10,1]+fitg*confint[11,1]+party*confint[12,1]+
  categ*confint[13,1]  
Lestimate80
exp(Lestimate80)
# R$408.031,60

# Vamos estimar o preco maximo do intervalo de confianca
# (80% de confianca)
Uestimate80=confint80[1,2]+age*confint80[2,2]+
  parea*confint80[3,2]+tarea*confint80[4,2]+
  bath*confint80[5,2]+ensuit*confint80[6,2]+
  garag*confint80[7,2]+park*confint80[8,2]+
  balc*confint80[9,2]+elev*confint80[10,2]+
  fitg*confint80[11,2]+party*confint80[12,2]+
  categ*confint80[13,2]  
Uestimate80
exp(Uestimate80)
# R$1.166.521,00

# Entao o preco medio estimado eh R$712.814,70, e pode
# variar entre R$408.031,60 e R$1.166.521,00

# Continua um intervalo grande entre os precos minimo e 
# maximo, isto se deve aos imoveis muito diferentes que
# temos na amostra. O range de precos eh muito grande e
# isso se reflete nos resultados das estimativas e na 
# predicao.

# Para prever o preco com mais acuracia devemos ter uma
# amostra mais aderente ao imovel que desejamos prever
# o preco. 
# Seria o caso de filtrarmos a amostra com imoveis mais
# parecidos com aquele que queremos prever o preço. 
# Isso pode ser feito filtrando os elementos da amostra
# com 1 desvio padrao para cima e para baixo nas
# variaveis explicativas nao binarias.
# Depois disso refazemos todo o caminho (testes, etc.) 
# ateh aqui.
# Fica o desafio para voce fazer esse exercicio e vera 
# que as estimativas de maximo e minimo ficam bem mais
# justas.

# Por outro lado, se pensarmos que o valor estimado 
# R$712.814,70 eh o valor medio, ja que o metodo MQO eh
# a estimativa pela media; entao podemos estimar o 
# intervalo de confianca para a media, como vimos 
# na disciplina EST1 (anteriormente).

# Vamos fazer esta estimativa
n <- nrow(imoveis)
m <- predmedfinal
s <- sd(imoveis$price)
dam <- s/sqrt(n)
CIlwr <- m + (qnorm(0.025))*dam
CIupr <- m - (qnorm(0.025))*dam 

CIlwr
# R$670.822,50
CIupr
# R$754.806,80

# Entao agora temos que o preco medio eh R$712.814,70 e
# pode variar entre R$670.822,50 e R$754.806,80

# Outra forma de fazer que pode ser interessante, mas o 
# mais correto eh filtrar uma amostra homogenea, como 
# exposto acima.

#############################################################