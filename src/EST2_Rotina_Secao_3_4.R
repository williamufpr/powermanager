##############################################################
#          Unidade 3: Modelos de series temporais            #
#                   Secao 3.4: Modelo ARIMA                  # 
##############################################################

# Criando um objeto com os pacotes necessarios
pacotes <- c("readr","readxl","plotly","tidyverse",
             "gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata",
             "fpp3","lubridate","urca", "dygraphs", 
             "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable",
             "tsutils","GetBCBData","quantmod","dgof",
             "seasonal", "seastests", "ggplot2")

# Instalando e carregando os pacotes necessarios
if(sum(as.numeric(!pacotes %in% installed.packages()))!=0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# Vamos utilizar a base de dados de populacao (mulheres)
# no Brasil 1990-2019, do IBGE
# Carregando a base de dados:
load("populacao.RData")

# Vamos criar um objeto chamado "popul" do tipo "ts"
# que eh uma serie temporal
popul=ts(populacao[2], start = c(1990), end=c(2019), 
          frequency = 1)

# Vamos plotar a nossa serie
options(scipen=999)
plot(popul)

# Vamos fazer o teste de sazonalidade para dados 
# nao anuais
seastests::combined_test(popul)
# Nao executou:
# Este eh um teste que nao funciona para dados anuais
# que eh o nosso caso, provavelmente nao existe 
# sazonalidade anual, mas voce pode utiliza-lo
# se for fazer uma analise com dados mensais, diarios,
# etc.

# Vamos executar o modelo ETS 
popul.ets <- ets(popul)
summary(popul.ets)

# O modelo ETS nao identificou sazonalidade, porque 
# escolheu uma funcao aditiva nos erros (A), 
# tendencia aditiva (A) e sem sazonalidade
# (N); entao devemos acreditar que nao existe 
# sazonalidade.

# Vamos dividir o nosso dataset em treinamento e teste
# Para o treinamento:
treinopop=window(popul, start=c(1990), end=c(2015))

# Vamos plotar a serie de treinamento
plot(treinopop)

# O tamanho da series eh:
length(treinopop)

# Para a base de teste:
testepop=window(popul, start=c(2016), end=c(2019))

# Vamos plotar a serie de teste:
plot(testepop)

# O tamanho da serie eh:
length(testepop)

# Vamos analisar a serie de treinamento
ggtsdisplay(treinopop)

# Vamos ver a acf da base de treino
acf(treinopop)
# A serie eh uma senoidal, entao pode ser um 
# modelo AR ou ARMA

# Vamos ver a pacf da base de treino
pacf(treinopop)
# A serie possui apenas 1 observacao fora da area de
# significancia, pode ser AR(1)

# Quantas diferencas sao necessarias para tornar a serie
# estacionaria? Vamos usar a funcao:
ndiffs(treinopop)
# Resposta = 2 (a serie precisa de 2 diferencas para se 
# tornar estacionaria). 

# Vamos utilizar a funcao AUTOARIMA que encontra o
# melhor modelo por experimentacao:
modelpop=auto.arima(treinopop,trace = T, 
                    seasonal = FALSE)
modelpop
# A funcao "auto.arima" nos mostra que o melhor modelo
# eh AR(2) I(2) MA(0) 

# Vamos fazer o teste de Dickey-Fuller
# As hipoteses sao:
# H0: a serie nao eh estacionaria
# Ha: a serie eh estacionaria

# Vamos fazer o ajuste do teste com tendencia, porque
# nos vimos que a serie possui tendencia (no modelo ETS),
# e vamos colocar 1 lag (por conta da PACF).

testeDFpop=ur.df(treinopop, type = 'none', lags = 1, 
                 selectlags='BIC')
testeDFpop
summary(testeDFpop)

# Resultado: z.lag.1 possui p-value 0.349, que eh > 0.05 
# entao nos nao rejeitamos H0, a serie nao eh 
# estacionaria. A serie necessita mesmo ser diferenciada.

# Vamos fazer nossa predicao 
forecpop<-forecast::forecast(treinopop,h=4)

# Vamos ver os valores preditos (Point Forecast)
forecpop
# Os valores ja sao fornecidos com intervalos de 
# confianca de 80% e 95%

# Vamos plotar a serie
autoplot(forecpop) +
  theme_bw()

ggplotly(
  autoplot(treinopop)+
    autolayer(testepop,serie="Valores Reais")+
    autolayer(forecpop$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# Vamos ver a acuracia do modelo na base de treino 
# e teste
forecast::accuracy(forecpop,testepop)
# O modelo "ARIMA" eh bom para essa predicao apesar
# de alguma diferenca entre os erros nas bases de 
# treino e teste

# Agora nos temos um modelo definido, entao nos 
# precisamos verificar se o modelo representa bem a
# estrutura da serie de tempo. Isso significa que
# nos devemos checar se os residuos sao nao 
# autocorrelacionados e normalmente distribuidos.

# 1. Teste de autocorrelacao dos residuos
# Ljung-Box test
# As hipoteses do teste sao:
# H0: independencia da serie de tempo (os residuos
#     sao nao correlacionados no tempo -eles sao iid-
#     o modelo nao apresenta falhas no ajuste)
# HA: dependencia na serie de tempo (os residuos sao 
#     correlacionados (nao sao iid) - o modelo possui
#     um ajuste ruim e nao captura alguma estrutura
#     dos dados, o que indica erro sistematico

# Vamos fazer o teste:
autoplot(modelpop$residuals)
checkresiduals(modelpop)

# O p-value eh 0.2896, como o p-value > 0.05, nao 
# rejeitamos H0 de que a serie eh iid e os residuos  
# sao independentes, nao existem falhas de 
# ajustamento no modelo.

# 2. Teste de normalidade nos residuos
# Kolmogorov-Smirnov test
# H0: os residuos sao normalmente distribuidos.
# HA: os residuos sao nao normalmente distribuidos.

ks.test(modelpop$residuals, "pnorm", 
        mean(modelpop$residuals), 
        sd(modelpop$residuals))

# como p-value > 0.05 nos nao rejeitamos H0, os
# residuos sao normalmente distribuidos. 

# 3. Teste de estacionariedade da variancia 
# (significa testar se existe um efeito ARCH OU GARCH)
# As hipoteses do teste sao:
# H0: Nao existe efeito ARCH/GARCH
# HA: Existe efeito ARCH/GARCH

# Vamos fazer o teste:
FinTS::ArchTest(modelpop$residuals)

# Como o p-value > 0.05 nos nao rejeitamos H0, a serie 
# temporal nao contem efeito ARCH/GARCH

# Se existisse efeito Arch/Garch nos necessitariamos 
# utilizar outro modelo tal como modelos ARCH ou GARCH
# (topico extra para voce estudar).

##########################################################
