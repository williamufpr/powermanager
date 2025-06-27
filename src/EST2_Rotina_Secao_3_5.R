##############################################################
#          Unidade 3: Modelos de series temporais            #
#                  Secao 3.5: Modelo SARIMA                  # 
##############################################################

# Este modelo possui parametros sazonais "P, D e Q" . 
# SARIMA(p,d,q)(P,D,Q)

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

# Vamos utilizar uma base de dados do indice de volume de
# vendas de "Sao Paulo-SP"
# Vamos carregar o dataset
load("C:/iaa/varejo.RData")

# Vamos criar um objeto chamado "varejo" do tipo "ts" -
# serie de tempo - de janeiro de 2000 a dezembro de 2018
# frequency = 12 significa que a serie eh mensal
varejo=ts(varejo[2], start = c(2000,1), 
          end = c(2018,12), 
          frequency = 12)

# Vamos plotar a serie
plot(varejo)
# Outra forma de visualizar
autoplot(varejo) +
  theme_bw()

# Vamos dividir a serie em treino e teste
treinovarejo=window(varejo, start=c(2000,1), 
                    end=c(2017,12))

# O tamanho da serie de treino eh:
length(treinovarejo)
testevarejo=window(varejo,start=c(2018,1), 
                   end=c(2018,12))

# O tamanho da serie de teste eh:
length(testevarejo)

# Vamos plotar as duas series juntas para checagem
autoplot(varejo) +
  autolayer(treinovarejo, series="Treino") +
  autolayer(testevarejo, series="Teste") +
  scale_color_viridis_d() +
  theme_bw()

# Vamos analisar a serie de treino
ggtsdisplay(treinovarejo)
# Eh possivel que exista sazonalidade
# Vemos os picos na 12a. observacao tanto na FAC quanto
# na FACP

# Vamos checar a serie pelo modelo ETS
varejo.ets <- ets(treinovarejo)
summary(varejo.ets)

# O modelo ETS nos apresenta que existe sazonalidade 
# multiplicativa, erro multiplicativo e tendencia 
# aditiva com drift

# Vamos fazer o teste de sazonalidade
combined_test(treinovarejo)

# O p-value eh aproximadamente "0" (que eh, < 0.5)
# entao a serie possui sazonalidade, nos podemos
# usar o modelo SARIMA, caso contrario deveriamos
# utilizar o modelo ARIMA

# Vamos fazer o teste de estacionariedade - raiz 
# unitaria
# Teste de Dickey-Fuller
# Ho: a serie nao eh estacionaria
# HA: a serie eh estacionaria
treinovarejoadf=ur.df(treinovarejo, selectlags = 'BIC',
                      type = 'none')
summary(treinovarejoadf)

# O "z.lag.1" possui p-value > 0.05, entao a serie eh 
# nao estacionaria.

# Vamos ver quantas diferenciacoes sao necessarias 
# para tornar a serie estacionaria
ndiffs(treinovarejo)
# Uma diferenciacao eh suficiente

# Vamos aplicar a diferenciacao 
diftreinovarejo=diff(treinovarejo)
ggtsdisplay(diftreinovarejo)
# Agora parece que a serie eh estacionaria, grafico
# com as observacoes em volta do zero.

# Vamos repedir o teste de Dickey-Fuller
treinovarejoadfdif=ur.df(diftreinovarejo, 
                         type='none',
                         selectlags = 'BIC')
summary(treinovarejoadfdif)
# Agora o "z.lag.1" possui p-value < 0.05, a serie
# eh estacionaria.

# A funcao "auto.arima" vai detectar a sazonalidade  
# e trata-la como um modelo SARIMA, fazendo as 
# diferenciacoes, etc.
arimavarejo=auto.arima(treinovarejo, trace=T)
# O melhor modelo eh um ARIMA (1,1,3) e o componente
# sazonal eh (0,1,1), com sazonalidade na 12a. 
# observacao

# Validacao e diagnostico
# Vamos checar os residuos
checkresiduals(arimavarejo)

# 1. Teste Ljung-Box ==> p-value = 0.8838 > 0.05, 
# entao nos nao rejeitamos que os residuos nao sao
# correlacionados (eles sao iid - o modelo nao possui
# falhas de ajustamento)

# 2. teste de normalidade nos residuos
ks.test(arimavarejo$residuals, "pnorm", 
        mean(arimavarejo$residuals),
        sd(arimavarejo$residuals))
# p-valor = 0.09386 > 0.05 - entao nos nao rejeitamos
# que os residuos sao normalmente distribuidos.

# Vamos checar a estacionariedade da variancia, que eh
# verificar se existe efeito ARCH/GARCH
ArchTest(arimavarejo$residuals)

# p-valor = 0.1069 > 0.05, entao nos nao rejeitamos H0, 
# que eh a nao existencia de efeito ARCH/GARCH
# (heterocedasticidade)

# Vamos fazer a predicao para 12 meses adiante
prevvarejo=forecast::forecast(treinovarejo, h=12)

# Os valores preditos sao:
prevvarejo

# Vamos ver no grafico
autoplot(prevvarejo) +
  theme_bw()

# Vamos verificar a acuracia do modelo
forecast::accuracy(prevvarejo, testevarejo)
# Os erros sao muito proximos, o modelo eh bom

# Vamos plotar a serie do modelo ajustado com as 
# predicoes
ggplotly(
  autoplot(treinovarejo)+
    autolayer(testevarejo,serie="Valores Reais")+
    autolayer(prevvarejo$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

#######################################################