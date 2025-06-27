##############################################################
#    Unidade 2: Modelos de regularizacao ou penalidade       #
#                  Secao 2.2: Regressao Ridge                # 
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
#                     REGRESSAO RIDGE                       #
#############################################################

# A regressao Ridge "encolhe os valores dos coeficientes"

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

# A regressao Ridge eh uma extensão da regressao linear em  
# que a funcao perda eh alterada para minimizar a 
# complexidade do modelo.
# Esta mudanca eh feita ao adicionarmos um parametro de 
# penalty igual ao quadrado dos valores dos coeficientes. 

# A principal diferenca entre a regressao linear e a 
# penalizada eh que a regressao penalizada usa um 
# hiperparametro "lambda".
# A funcao glmnet() executa o modelo muitas vezes para 
# valores diferentes de lambda. 
# Podemos automatizar esta tarefa para encontrar o melhor 
# valor de lambda usando a funcao "cv.glmnet()".

# A funcao de perda generica eh:
# Funcao perda = MQO+lambda*soma(quadrados dos valores
# dos coeficientes).
# O lambda eh o parametro de penalty encontrado.

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

# Vamos calcular o valor otimo de lambda; 
# alpha = "0", eh para regressao Ridge
# Vamos testar os lambdas de 10^-3 ate 10^2, a cada 0.1
lambdas <- 10^seq(2, -3, by = -.1)
# Calculando o lambda:
ridge_lamb <- cv.glmnet(x, y_train, alpha = 0, 
                        lambda = lambdas)
# Vamos ver qual o lambda otimo 
best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

# Para contar o tempo de processamento do modelo usamos:
start <- Sys.time()
# Estimando o modelo Ridge
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, 
                   family = 'gaussian', 
                   lambda = best_lambda_ridge)
end <- Sys.time()
difftime(end, start, units="secs")

# Vamos ver o resultado (valores) da estimativa 
# (coeficientes)
ridge_reg[["beta"]]

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

# Predicao e avaliacao nos dados de treinamento:
predictions_train <- predict(ridge_reg, 
                             s = best_lambda_ridge,
                             newx = x)

# As metricas da base de treinamento sao:
eval_results(y_train, predictions_train, train)

# Predicao e avaliacao nos dados de teste:
predictions_test <- predict(ridge_reg, 
                            s = best_lambda_ridge, 
                            newx = x_test)

# As metricas da base de teste sao:
eval_results(y_test, predictions_test, test)

# Se compararmos as metricas de treinamento e teste
# vemos que o tamanho dos erros de treinamento e teste
# e os R^2 sao muito parecidos, o que descarta a hipotese
# de overfitting e underfitting. Alem disso o poder 
# explicativo do modelo eh elevado, maior que 85%
# (treinamento e teste) o que eh excelente

# Vamos fazer uma predicao para o nosso exemplo

# Como os valores do dataset sao padronizados, nos temos 
# de padronizar tambem os dados que vamos fazer a predicao 
# Note que as variaveis dummies nao devem ser padronizadas

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

# Fazendo a predicao:
predict_our_ridge <- predict(ridge_reg, 
                             s = best_lambda_ridge, 
                             newx = our_pred)
# O resultado da predicao eh:
predict_our_ridge

# O resultado eh um valor padronizado, vamos converte-lo
# para o valor nominal, consistente com o dataset original
wage_pred_ridge=(predict_our_ridge*
                   pre_proc_val[["std"]][["hrwage"]])+
  pre_proc_val[["mean"]][["hrwage"]]

# O resultado eh:
wage_pred_ridge
# Este eh o valor predito do salario por hora (US$), 
# segundo as caracteristicas que atribuimos

# O intervalo de confianca para o nosso exemplo eh:
n <- nrow(train) # tamanho da amostra
m <- wage_pred_ridge # valor medio predito
s <- pre_proc_val[["std"]][["hrwage"]] # desvio padrao
dam <- s/sqrt(n) # distribuicao da amostragem da media
CIlwr_ridge <- m + (qnorm(0.025))*dam # intervalo inferior
CIupr_ridge <- m - (qnorm(0.025))*dam # intervalo superior

# Os valores sao:
CIlwr_ridge
CIupr_ridge

# Entao, segundo as caracteristicas que atribuimos o
# salario-hora da esposa eh em media US$10.32 e pode
# variar entre US$10.03 e US$10.61

###########################################################