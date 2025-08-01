##############################################################
#          Unidade 3: Modelos de series temporais            #
#                    Secao 3.3: Modelo ETS                   # 
##############################################################

# Criando um objeto "pacotes" com os pacotes necessarios
pacotes <- c("readxl","plotly","tidyverse","gridExtra",
             "forecast","TTR","smooth","tidyverse", 
             "tsibble", "fable","tsibbledata", "fpp3",
             "urca", "tigerstats")

# Instalando e carregando os pacotes necessarios
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# O Modelo ETS eh:
# E (Error) T (Trend) S (Season)
# O erro pode ser: aditivo (A) ou multiplicativo (M)
# A tendencia pode ser: nenhuma (N), aditiva (A),
#                       multiplicativa (M) ou 
#                       dumped (A ou M)
# A sazonalidade pode ser: nenhuma (N), aditiva (A)
#                          ou multiplicativa (M)

# Dumped = Amortecida ou suavizada

# Vamos utilizar a base de dados de consumo de energia
# eletrica na regiao sudeste (dados disponiveis no 
# IPEADATA)

# Vamos carregar o dataset e guardar em um objeto
# chamado "energia"
energia <- read_excel("energia.xlsx")

# Vamos utilizar apenas a coluna 2, que contem os dados 
# de consumo de energia
energia=energia[2]

# Construindo um objeto do tipo ts (time series)
energia=ts(energia,start=c(1979,1), end=c(2021,9), 
           frequency=12)
energia

# Vamos ver o tamanho da amostra
length(energia)

# Vamos dividir o dataset, uma parte para treinamento do
# modelo e outra para teste
energiatreino=window(energia,start=c(1979,1), 
                     end=c(2019,12))
plot(energiatreino)
length(energiatreino)

energiateste=window(energia,start=c(2020,1), 
                    end=c(2021,9))
plot(energiateste)
length(energiateste)

# Vamos estimar o modelo ETS e guardar em um objeto
# chamado "energiatreino.ets"
energiatreino.ets <- ets(energiatreino)

# Vamos ver os resultados
summary(energiatreino.ets)
# O resultado nos mostra que o "erro eh multiplicativo",
# a "tendencia eh aditiva" e a "sazonalidade eh 
# multiplicativa"
# Ao final vamos analisar as estatisticas de erro.

# Vamos fazer uma previsao para os 21 periodos de tempo
# do dataset e depois comparar com os dados reais
energia.ets.forecasts <- forecast.ets(energiatreino.ets, 
                                      h = 21)

# Vamos ver os resultados
summary(energia.ets.forecasts)
# Os resultados jah trazem os intervalos de confianca
# de 80% e 95% de comfianca

# Vamos plotar um grafico com as previsoes
plot(energia.ets.forecasts)

# Agora vamos plotar um grafico com os dados reais e da
# previsao
ggplotly(
  autoplot(energiatreino)+
    autolayer(energiateste,serie="Real values")+
    autolayer(energia.ets.forecasts$mean, 
              serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
# Perceba que diferencas entre os dados reais e da 
# previsao estao relacionados com a pandemia e portanto
# sao choques exogenos que dificultam a acuracia do
# modelo

# Vamos ver as estatisticas de acuracia no dataset de
# teste
forecast::accuracy(energia.ets.forecasts$mean,
                   energiateste)

# Vamos plotar os residuos da estimativa
plot(energiatreino.ets$residuals)

# Vamos plotar a funcao de autocorrelacao da estimativa
acf(energiatreino.ets$residuals)
# A ACF nos fornece o numero de lags (defasagem) do
# modelo. Temos uma defasagem porque uma observacao esta 
# fora da area de significancia (2 erros padroes), como a
# segunda observacao esta dentro da area, desprezamos as
# demais para adiante.

# Vamos fazer a validacao do modelo
# Vamos analisar os residuos (erros) das predicoes
# As condicoes sao:
# a) os residuos nao podem ser correlacionados; se eles
#    sao correlacionados eles contem informacoes nos 
#    que deveriam estar no modelo;
# b) os residuos devem ter media zero, se nao, entao as
#    predicoes sao viesadas

# Vamos aplicar o teste de Ljung-Box com um lag (conforme 
# identificamos anteriormente)
Box.test(energiatreino.ets$residuals, lag=1,
         type=c("Ljung-Box"))

# As hipoteses do teste Ljung-box s�o:
# H0: Os residuos sao iid - independentes e identicamente 
#     distribuidos (o modelo nao possui erros de 
#     especificacao)
# HA: Os residuos nao sao iid - independentes e 
#     identicamente distribuidos (o modelo possui erros de
#     especificacao)

# A estatistica de teste eh uma distribuicao chi-squared
# Obtendo os valores criticos (da tabela) para 95% de 
# confianca
qchisq(0.95,1)

# Construindo o grafico para destacar os 5% do lado 
# superior da distribuicao
pchisqGC(c(3.841459),region="above",df=1, graph = TRUE)
# Como a estatistica de Ljung-Box eh menor que o valor 
# tabelado nao rejeitamos H0.

# Nos nao queremos rejeitar H0 (nos desejamos um 
# p-value>0.05), como o p-value eh 0.4795 nao rejeitamos
# H0. O modelo nao possui erros de especificacao

############################################################