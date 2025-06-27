##############################################################
#    Unidade 2: Modelos de regularizacao ou penalidade       #
#                  Secao 2.3: Regressao Lasso                # 
##############################################################

# Instalando os pacotes necessarios
#install.packages("plyr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("repr")
#install.packages("glmnet")

# Carregando os pacotes necessarios
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

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
cols = c('husage', 'husearns', 'huseduc', 'hushrs', 'earns',
         'age', 'educ', 'hrwage')

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
#                     REGRESSAO LASSO                       #
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

# A regressao Lasso reduz os coeficientes nao significativos
# a "zero"

# A regressao Lasso, ou Operador de Encolhimento Absoluto
# Minimo e de Selecao, tambem eh uma variacao da regressao
# linear.
# Para a regressao Lasso, a funcao perda eh alterada para
# minimizar a complexidade do modelo, restringindo a soma
# dos valores absolutos dos coeficientes do modelo
# (tambem chamada de restricao "l1-norm").
# A restricao "l1-norm" forca alguns alguns valores dos 
# pesos a "zero" para permitir que outros coeficientes  
# tenham valores mais distantes de "zero".

# A funcao perda eh:
# Funcao perda = MQO+lambda*soma(valores absolutos dos
# coeficientes)

# Vamos criar o intervalo para o tunning do melhor lambda
lambdas <- 10^seq(2, -3, by = -.1)

# Vamos atribuir alpha = 1 para implementar a regressao
# lasso
lasso_lamb <- cv.glmnet(x, y_train, alpha = 1, 
                        lambda = lambdas, 
                        standardize = TRUE, nfolds = 5)

# Vamos guardar o lambda "otimo" em um objeto chamado
# best_lambda_lasso
best_lambda_lasso <- lasso_lamb$lambda.min 
best_lambda_lasso

# Vamos estimar o modelo Lasso 
lasso_model <- glmnet(x, y_train, alpha = 1, 
                      lambda = best_lambda_lasso, 
                      standardize = TRUE)

# Vamos visualizar os coeficientes estimados
lasso_model[["beta"]]
# Perceba que alguns coeficientes estao zerados pois 
# nao sao significativos

# Vamos fazer as predicoes na base de treinamento e
# avaliar a regressao Lasso 
predictions_train <- predict(lasso_model, 
                             s = best_lambda_lasso,
                             newx = x)

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

# As metricas da base de treinamento sao:
eval_results(y_train, predictions_train, train)

# Vamos fazer as predicoes na base de teste
predictions_test <- predict(lasso_model, 
                            s = best_lambda_lasso, 
                            newx = x_test)

# As metricas da base de teste sao:
eval_results(y_test, predictions_test, test)
# Perceba que os erros nao sao muito diferentes, assim 
# como os valores de R2.

# Vamos fazer uma predicao para o nosso exemplo

# Vamos fazer a predicao com base nos mesmos parametros
# como fizemos anteriormente na regressao Ridge

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

# Vamos para a predicao
predict_our_lasso <- predict(lasso_model, 
                             s = best_lambda_lasso, 
                             newx = our_pred)
predict_our_lasso

# Novamente, o resultado esta padronizado, nos temos de
# converte-lo para valor compativel com o dataset original
wage_pred_lasso=(predict_our_lasso*
                   pre_proc_val[["std"]][["hrwage"]])+
  pre_proc_val[["mean"]][["hrwage"]]
wage_pred_lasso
# Portanto, o salario-hora da esposa eh US$10.36

# Vamos criar o intervalo de confianca para o nosso
# exemplo
n <- nrow(train)
m <- wage_pred_lasso
s <- pre_proc_val[["std"]][["hrwage"]]
dam <- s/sqrt(n)
CIlwr_lasso <- m + (qnorm(0.025))*dam
CIupr_lasso <- m - (qnorm(0.025))*dam 

# O intervalo de confianca eh:
CIlwr_lasso
CIupr_lasso
# Entao, o salario medio eh de US$10.36 e pode variar
# entre US$10.07 e US$10.65

#############################################################