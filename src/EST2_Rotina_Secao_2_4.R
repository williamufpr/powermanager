##############################################################
#    Unidade 2: Modelos de regularizacao ou penalidade       #
#               Secao 2.4: Regressao ElasticNet              # 
##############################################################

# Instalando os pacotes necessarios
# install.packages("plyr")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("repr")
# install.packages("glmnet")
# install.packages("caret")

# Carregando os pacotes necessarios
library(plyr)
library(readr)
library(dplyr)
library(ggplot2)
library(repr)
library(glmnet)
library(caret)

# Vamos utilizar a base de dados "wage" que eh um dataset
# que contem uma amostra com dados sobre a renda de
# familias
# Carregando o dataset
load("C:/iaa/wage.RData")

# Vamos guardar este dataset em um objeto chamaddo "dat"
dat <- wage

# Vamos ver o dataset inteiro
View(dat)

# Vamos visualizar parte do dataset
glimpse(dat)

# Vamos ver como esta nossa memoria
gc()

# Vamos jogar a semente para gerar numeros aleatorios
# Aqui no exemplo a semente eh "302", mas poderia
# ser qualquer outro numero, se todos usarem a mesma
# semente os resultados serao iguais.
# Essa semente de numeros aleatorios serve para
# particionar o dataset aleatoriamente
set.seed(302)  

# Vamos criar um indice para particionar o dataset em
# 80% para treinamento
index = sample(1:nrow(dat),0.8*nrow(dat))

# Vamos criar a base de dados de treinamento
train = dat[index,]  

# Vamos criar a base de dados de teste
test = dat[-index,] 

# Vamos checar as dimensoes das bases de treinamento e 
# teste
dim(train)
dim(test)
# A base de treinamento possui 1435 linhas (familias) e 
# 15 colunas (variaveis)
# A base de teste possui 359 linhas (familias) e 15
# colunas (variaveis)

# Vamos padronizar as variaveis
# Vamos criar um objeto com as variaveis para padronizar
# As variaveis binarias nao sao padronizadas
cols = c('husage', 'husearns', 'huseduc', 'hushrs', 
         'earns', 'age', 'educ', 'hrwage')

# Padronizando a base de treinamento e teste
pre_proc_val <- preProcess(train[,cols], 
                           method = c("center", "scale"))
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

# Vamos ver o sumario estatistico das variaveis 
# padronizadas de cada dataset
summary(train)
summary(test)

#############################################################
#                     REGRESSAO ELASTICNET                  #
#############################################################

# Vamos criar um objeto com as variaveis que usaremos no
# modelo
cols_reg = c('husage', 'husearns', 'huseduc', 'hushrs', 
             'earns', 'age', 'educ', 'hrwage','husblck',
             'hushisp', 'kidge6', 'black', 'hispanic',
             'union', 'kidlt6')

# Vamos gerar variaveis dummies para organizar os datasets
# em objetos tipo matriz
# Estamos interessados em estimar o salario-hora (hrwage)
dummies <- dummyVars(hrwage~husage+husearns+huseduc+hushrs+
                       earns+age+educ+husblck+hushisp+
                       kidge6+black+hispanic+union+kidlt6, 
                     data = dat[,cols_reg])
train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])
print(dim(train_dummies)); print(dim(test_dummies))

# Vamos guardar a matriz de dados de treinamento das 
# variaveis explicativas para o modelo em um objeto 
# chamado "x"
x = as.matrix(train_dummies)

# Vamos guardar o vetor de dados de treinamento da 
# variavel dependente para o modelo em um objeto
# chamado "y_train"
y_train = train$hrwage

# Vamos guardar a matriz de dados de teste das variaveis
# explicativas para o modelo em um objeto chamado
# "x_test"
x_test = as.matrix(test_dummies)

# Vamos guardar o vetor de dados de teste da variavel
# dependente para o modelo em um objeto chamado "y_test"
y_test = test$hrwage

# A regressao ElasticNet combina aspectos das regressoes
# Ridge e Lasso. Ela penaliza o modelo usando ambas 
# funcoes de penalidade, l2-norm and l1-norm. 
# O modelo pode ser customizado pelo pacote "caret" 
# que encontra os valores otimos dos parametros
# automaticamente.

# Vamos configurar o treinamento do modelo por 
# cross validation, com 10 folders, 5 repeticoes
# e busca aleatoria dos componentes das amostras
# de treinamento, o "verboseIter" eh soh para 
# mostrar o processamento.
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Nos nao temos o parametro "alpha", porque a regressao
# ElasticNet vai encontra-lo automaticamente, cujo valor
# estara entre 0 e 1 (para Ridge ==> alpha = 0; e
# para Lasso ==> alpha = 1).
# O parametro "lambda" tambem eh escolhido por
# cross-validation

# Vamos treinar o modelo
elastic_reg <- train(hrwage ~ husage+husearns+huseduc+
                       hushrs+earns+age+educ+husblck+
                       hushisp+kidge6+black+hispanic+
                       union+kidlt6,
                     data = train,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = train_cont)

# O melhor parametro alpha escolhido eh:
elastic_reg$bestTune

# E os parametros sao:
elastic_reg[["finalModel"]][["beta"]]

# Vamos fazer as predicoes e avaliar a performance do
# modelo

# Vamos fazer as predicoes no modelo de treinamento:
predictions_train <- predict(elastic_reg, x)

# Vamos calcular o R^2 dos valores verdadeiros e 
# preditos conforme a seguinte funcao:
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # As metricas de performace do modelo:
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# As metricas de performance na base de treinamento
# sao:
eval_results(y_train, predictions_train, train) 

# Vamos fazer as predicoes na base de teste
predictions_test <- predict(elastic_reg, x_test)

# As metricas de performance na base de teste sao:
eval_results(y_test, predictions_test, test)

# Vamos agora fazer a predicao para o nosso exemplo,
# mesmos parametros utilizados para a regressao 
# Ridge e Lasso

# Vamos fazer uma predicao para:
# husage = 40 anos (idade do marido)
husage = (40-pre_proc_val[["mean"]][["husage"]])/
  pre_proc_val[["std"]][["husage"]]

# husearns = 551 (rendimento do marido em US$)
husearns = (551-pre_proc_val[["mean"]][["husearns"]])/
  pre_proc_val[["std"]][["husearns"]]

# huseduc = 13 (anos de estudo do marido)
huseduc = (13-pre_proc_val[["mean"]][["huseduc"]])/
  pre_proc_val[["std"]][["huseduc"]]

# husblck = 0 (o marido nao eh preto)
husblck = 0

# hushisp = 0 (o marido nao eh hispanico)
hushisp = 0

# hushrs = 40 (o marido trabalha 40 horas semanais)
hushrs = (40-pre_proc_val[["mean"]][["hushrs"]])/
  pre_proc_val[["std"]][["hushrs"]]

# kidge6 = 0 (nao tem filhos maiores de 6 anos)
kidge6 = 0

# earns = 355.5 (rendimento da esposa em US$)
earns = (355.5-pre_proc_val[["mean"]][["earns"]])/
  pre_proc_val[["std"]][["earns"]]

# age = 37 anos (idade da esposa) 
age = (37-pre_proc_val[["mean"]][["age"]])/
  pre_proc_val[["std"]][["age"]]

# black = 0 (esposa nao eh preta)
black = 0

# educ = 13 (esposa possui 13 anos de estudo)
educ = (13-pre_proc_val[["mean"]][["educ"]])/
  pre_proc_val[["std"]][["educ"]]

# hispanic = 0 (esposa nao eh hispanica)
hispanic = 0

# union = 0 (o casal nao possui uniao registrada)
union = 0

# kidlt6 = 0 (nao possui filhos com menos de 6 anos)
kidlt6 = 0

# Vamos construir uma matriz de dados para a predicao
our_pred = as.matrix(data.frame(husage=husage, 
                                husearns=husearns,
                                huseduc=huseduc,
                                husblck=husblck,
                                hushisp=hushisp,
                                hushrs=hushrs,
                                kidge6=kidge6,
                                earns=earns,
                                age=age,
                                black=black,
                                educ=educ,
                                hispanic=hispanic,
                                union=union,
                                kidlt6=kidlt6))

# Vamos fazer a predicao com base nos parametros que
# selecionamos
predict_our_elastic <- predict(elastic_reg,our_pred)
predict_our_elastic

# Novamente, o resultado eh padronizado, nos temos que
# reverte-lo para o nivel dos valores originais do
# dataset, vamos fazer isso:
wage_pred_elastic=(predict_our_elastic*
                     pre_proc_val[["std"]][["hrwage"]])+
  pre_proc_val[["mean"]][["hrwage"]]
wage_pred_elastic
# Entao o salario-hora medio da esposa predito com base
# nas caracteristicas informadas eh US$9.28

# Vamos criar o intervalo de confianca para o nosso
# exemplo
n <- nrow(train)
m <- wage_pred_elastic
s <- pre_proc_val[["std"]][["hrwage"]]
dam <- s/sqrt(n)
CIlwr_elastic <- m + (qnorm(0.025))*dam
CIupr_elastic <- m - (qnorm(0.025))*dam 

# Os valores minimo e maximo sao:
CIlwr_elastic
CIupr_elastic
# Entao, o salario-hora medio da esposa eh de US$9.28 
# e pode variar entre US$8.99 e US$9.57

#########################################################