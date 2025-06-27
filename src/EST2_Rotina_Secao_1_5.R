#################################################################
#               Unidade 1: Analise de regressao                 #
#       Secao 1.5: Testes para o modelo linear por MQO e        # 
#                         a regressao stepwise                  #
#################################################################
# Vamos instalar os pacotes necessarios
# install.packages("carData")
# install.packages("car")
# install.packages("RcmdrMisc")
# install.packages("zoo")
# install.packages("lmtest")
# install.packages("nortest")
# install.packages("lmtest")
# install.packages("sandwich")

# Vamos carregar os pacotes necessarios
library(carData)
library(car)
library(RcmdrMisc)
library(zoo)
library(lmtest)
library(nortest)
library(lmtest)
library(sandwich)

# Usaremos o dataset "imoveiscwbav" que contem uma amostra de 541
# imoveis com preco dos imoveis e as caracteristicas de cada um
# destes imoveis.

# Vamos carregar o dataset
load("imoveiscwbav.RData")

# Vamos ver a base de dados
View(imoveiscwbav)

# Vamos guardar o dataset em um outro objeto chamado "imoveis"
imoveis <- imoveiscwbav

# Vamos evitar a notacao cientifica nos resultados:
options(scipen = 999)

# Vamos agora estimar o modelo linear por MQO com o preco em
# logaritmo neperiano que no "R" eh "log"; o "." quer dizer que 
# estamos regredindo o ln do preco contra todas as demais 
# variaveis, ou seja, todas as demais variaveis do dataset sao
# variaveis explicativas (Xs) no modelo. Estamos guardando
# os resultados da estimativa em um objeto chamado "resultados"
resultados <- lm(log(price)~., data=imoveis)

# Vamos visualizar os resultados com a funcao "summary()"
summary(resultados)

###############################################################
#                       OUTLIERS                              #
###############################################################

# Vamos fazer o teste de Bonferroni sobre os resultados: 
outlierTest(resultados)

# O resultado eh:
#rstudent unadjusted p-value      Bonferroni p
#393  5.02293      0.00000070067   0.00037906

# Existe um outlier na linhas 393
# Neste caso, se vc julgar prudente, vc deve deletar a linha
# indicada na tabela de dados. Vamos editar o conjunto de 
# dados (eliminacao da linha 393):
row.names.imoveis<-c(393)
imoveis <- imoveis[!(row.names(imoveis) %in% 
                       row.names.imoveis),]

# Depois deve-se refazer a estimativa dos resultados e o 
# teste de outliers, se necessario deve-se extrair novas
# observacoes e recalcular o modelo e o teste de outlier
resultados <- lm(log(price)~., data=imoveis)
summary (resultados)

# Refazendo o teste de outliers
outlierTest(resultados)

# O resultado eh:
#rstudent unadjusted p-value Bonferroni p
#364 -4.005752        0.000070857     0.038263

# Ainda temos um outlier nas linhas 364, vamos retira-lo
row.names.imoveis<-c(364)
imoveis <- imoveis[!(row.names(imoveis) %in% 
                       row.names.imoveis),]

# Vamos reestimar o modelo novamente 
resultados <- lm (log(price)~., data=imoveis)
summary (resultados)

# Refazendo o teste de outliers
outlierTest(resultados)

# O resultado eh:
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#   rstudent unadjusted p-value Bonferroni p
# 362 -3.632233         0.00030895      0.16652

# O teste apresentou que nao temos mais outliers


################################################################
#                   Regressao STEPWISE                         #
#             Escolha das variaveis e do modelo                #
################################################################

# Vamos estimar a regressao stepwise sobre os nossos resultados
# com a opcao "backward" e o criterio AIC
stepwise(resultados, direction= 'backward', criterion ='AIC')

# A regressao setpwise indicou que devem permanecer no modelo
# as seguintes variaveis dependentes: age, parea, tarea, bath,
# ensuit, garag, plaz, park, trans, balc, elev, fitg, party,
# categ.

# Vamos reestimar o modelo somente com essas variaveis

resultados <- lm(log(price)~age+parea+tarea+bath+ensuit+
                   garag+plaz+park+trans+balc+elev+fitg+
                   party+categ, data=imoveis)
summary (resultados)

# Agora temos quase todas as variaveis estatisticamente 
# significativas, excecao para as variaveis "trans" e "plaz",
# que sao significativas apenas a 90% de confianca, mas ainda
# devemos fazer outros testes/ajustes e essas variaveis podem
# se tornar significativas a 95% de confianca.
# Se necessario faremos a exclusao mais adiante.


##############################################################
#                       Multicolinearidade                   #
##############################################################

# Vamos verificar a existencia de multicolineariedade, que eh 
# identificar a alta correlacao entre as variaveis. Vamos 
# fazer isto com o Fator de Inflacao da variancia (VIF), que 
# eh uma medida da correlacao multipla das variaveis e sua
# influencia no R2, R2 ajustado e variancia.

# Vamos calcular o VIF 
car::vif(resultados)

# Aqui a regra nao eh unica, muitos pesquisadores usam valores de
# corte do VIF diferentes, alguns usam valores acima de 4, outros
# acima de 5, outros acima de 10; recomenda-se o uso de valores 
# acima de 5, que eh o que a maioria dos pesquisadores utiliza.

# Portanto, como nao existe nenhuma variavel com VIF acima de 5, 
# entao nao existe nenhuma variavel para retirar do modelo.


###############################################################
#                       NORMALIDADE                           #
###############################################################

# Vamos testar a normalidade dos residuos da regressao.
# Vamos utilizar o teste de Shapiro-Wilk para amostras de ate
# 5000 observacoes

# As hipoteses do teste:
# H0: A amostra eh normalmente distribuida
# Ha: A amostra nao eh normalmente distribuida

# O teste:
shapiro.test(resultados$residuals)

# O resultado:
# Como o p-value > 0.05 entao nao rejeitamos H0, ou seja, a 
# amostra eh normalmente distribuida.

# Vamos utilizar o teste de normalidade de Kolmogorov-Smirnov
# para amostras de qualquer tamanho:
lillie.test(resultados$residuals)

# O resultado:
# Como o p-value > 0.05 entao nao rejeitamos H0, ou seja, a 
# amostra eh normalmente distribuida.

# Caso a amostra nao fosse normalmente distribuida, deveriamos
# optar por um modelo de regressao nao parametrico ou 
# semiparametrico. Pode-se ainda tentar normalizar o modelo
# com transformacao Box-Cox.


#################################################################
#                  ESPECIFICACAO DO MODELO                      #
#################################################################

# Precisamos fazer o teste RESET de especificacao do modelo.
# As hipoteses do teste sao: 
# H0 = O modelo esta corretamente especificado; 
# HA = O modelo NAO esta corretamente especificado;

# Atencao, as variaveis fatores (factor) ficam fora do teste
resettest(log(price)~age+parea+tarea+bath+ensuit+
            garag+plaz+park+trans, power=2:3, 
          type=c("fitted", "regressor", "princomp"), 
          data=imoveis)

# O resultado do teste eh:
# RESET test
# data:  log(price) ~ age + parea + tarea + bath + ensuit + garag 
#                     + plaz + park + trans
# RESET = 0.080544, df1 = 2, df2 = 527, p-value = 0.9226


# O valor tabelado para compararmos com o valor do teste eh:
qf(.95,df1=2,df2=527)

# Resposta:
# F tabelado = 3,012826

# Como o F calculado (0,08) eh menor que o F tabelado (3,01) 
# nao existe erro de especificacao do modelo.
# Regra de bolso: Toda vez que p-value > 0.05 nao existe erro
# de especificacao do modelo, não rejeitamos H0.

# Caso houvesse algum erro de especificacao do modelo teriamos 
# de incluir (testar) as potencias 2 e 3 nas variaveis, alem 
# outras formas de transformacao das variaveis, uma a uma e
# testar cada modelo pelo teste "reset".


################################################################
#                   AUTOCORRELACAO DOS RESIDUOS                #
################################################################

# Verificando a autocorrelacao dos residuos - empregavel somente
# para dados de series temporais, vamos fazer aqui soh para 
# exemplificar, caso voce precise fazer quando a sua amostra 
# for de dados de series temporais

# TABELA DE ANALISE DAS HIPOTESES
# HIPOTESE NULA            DECISAO       SE
# nao existe autocorr(+)   rejeitar      0 < d < dl
# nao existe autocorr +    sem decisao   dl <= d <= du
# nao existe autocorr -    rejeitar      4 - dl < d < 4
# nao existe autocorr -    sem decisao   4 - du <= d <= 4 - dl
# nenhuma autocorr(+)ou(-) nao rejeitar  du < d < 4-du ***

# As hipoteses do teste:
# H0: Nao existe autocorrelacao nos residuos
# Ha: Existe autocorrelacao nos residuos ou o teste eh nao
#     conclusivo

# O teste:
dwtest(log(price)~age+parea+tarea+bath+ensuit+
         garag+plaz+park+trans+balc+elev+fitg+
         party+categ, alternative="greater", data=imoveis)

# O resultado:
# Durbin-Watson test
#data:  log(price) ~ age + parea + tarea + bath + ensuit + garag +
#                    plaz + park + trans + balc + elev + fitg + 
#                    party + categ
#DW = 1.9661, p-value = 0.3191
# alternative hypothesis: true autocorrelation is greater than 0

#Parametros do teste:
# k = 14,  n = 539 
# dl = 1,528, du = 1,824 (tabela no livro do Gujarati)

# 4 - dl = 2,472
# 4 - du = 2,176

# O valor de 1.9661 esta entre 1,824 e 2,176, ultima linha da
# tabela de analise, portanto nao existe autocorrelacao nos
# residuos.
# Toda vez que a estatistica DW estiver muito proxima de 2
# quer dizer que nao existe autocorrelacao nos residuos.

# Regra de bolso = Toda vez que o p-value > 0.05 nao existe
#                  autocorrelacao nos residuos

# Caso exista autocorrelacao positiva ou negativa, utilizar o 
# metodo da primeira diferenca exposto na secao 12.9 "metodo 
# da primeira diferenca" do livro do Gujarati e Porter.
# Alternativamente, pode-se utilizar estimadores sandwich 
# HAC.


###############################################################
#                    HETEROCEDASTICIDADE                      #
###############################################################

# Verificando a heterocedasticidade = variancia nao constante
# (amostra homogenea?)

# Para grandes amostras utilizar o teste de Breusch-Pagan, 
# caso contrario utilizar o teste de Goldfeld-Quandt

# Vamos fazer primeiro o teste de Goldfeld-Quandt soh para
# exemplificar o uso da funcao de teste (nao deve ser usada
# para grandes amostras)
# As hipoteses do teste:
# H0: A amostra eh homocedastica
# Ha: A amostra eh heterocedastica

# Vamos ao teste:
gqtest(resultados, order.by = ~age+parea+tarea+bath+ensuit+
         garag+plaz+park+trans+balc+elev+fitg+
         party+categ, data = imoveis, fraction = 5)

# O valor tabelado para compararmos com o valor do teste eh:
qf(.95,df1=252,df2=252)

# Como o valor da estatistica de teste (1.206) eh menor que 
# a estatistica tabelada (1.23075) não rejeitamos H0, ou seja,
# se fosse uma amostra pequena, poderiamos concluir que a 
# amostra eh homogenea (homocedastica).
# Regra de bolso: como p-value > 0.05 a amostra eh 
# homocedastica 

# Agora vamos utilizar um teste adequado para grandes amostras
# que eh o nosso caso. Vamos utilizar o teste de Breusch-Pagan. 

# As hipoteses do teste:
# H0: O modelo eh homocedastico (variancia constante) 
# HA: O modelo eh Heterocedatico (variancia nao-constante)

# O teste:
bptest(log(price)~age+parea+tarea+bath+ensuit+
         garag+plaz+park+trans+balc+elev+fitg+
         party+categ, studentize=FALSE, data=imoveis)

# O resultado:
# Breusch-Pagan test
#  data:  lnprice~age+parea+tarea+bath+ensuit+garag+plaz+
#                 park+trans+balc+elev+fitg+party+categ
# BP = 59.988, df = 14, p-value = 0.0000001179

# Obtendo o valor critico (tabelado) da estatistica 
# qui-quadrado:
qchisq(0.95, df=14)

# O resultado eh confrontado em um teste qui-quadrado com
# k-1 graus de liberdade. Como o resultado do teste BP 
# (59.988) eh maior que o tabelado (23,68478) rejeita-se a
# hipotese de que o modelo eh homocedastico.
# Regra de bolso: se p-value < 0.05 existe 
# heterocedasticidade, temos um problema importante e
# precisamos corrigi-lo.

# Vamos refazer a regressao novamente para lembrar do modelo:
resultados <- lm(log(price)~age+parea+tarea+bath+ensuit+
                   garag+plaz+park+trans+balc+elev+fitg+
                   party+categ, data=imoveis)
summary(resultados)

# Vamos fazer a correcao da heterocedasticidade por
# estimadores sandwich, isto eh feito pela seguinte funcao:
coeftest(resultados, vcov=vcovHC(resultados, type="HC"))

# Analisando os resultados vemos que as variaveis "plaz" e 
# "trans", nao sao significativas a 95% de confianca, 
# portanto vamos retira-las do modelo, e reestimar o modelo
# e o teste de heterocedasticidade.

# Reestimando o modelo sem as variaveis "plaz" e "trans":
resultados <- lm(log(price)~age+parea+tarea+bath+ensuit+
                   garag+park+balc+elev+fitg+party+categ,
                 data=imoveis)
summary(resultados)

# Refazendo o teste de heterocedasticidade:
bptest(log(price)~age+parea+tarea+bath+ensuit+
         garag+park+balc+elev+fitg+party+categ,
       studentize=FALSE, data=imoveis)

# O resultado:
# Breusch-Pagan test
# data:  lnprice ~ age + parea + tarea + bath + ensuit + 
#                  garag + park + balc + elev + fitg +
#                  party + categ
# BP = 55.195, df = 12, p-value = 0.000000167

# Obtendo o valor critico (tabelado) da estatistica 
# qui-quadrado:
qchisq(0.95, df=12)

# Percebemos que melhorou o problema da heterocedasticidade,
# o valor da estatistica BP esta menor, portanto essas duas
# variaveis extraidas eram fontes de heterocedasticidade.
# Porem, ainda persiste o problema de heterocedasticidade
# pois p-value < 0.05

# Vamos novamente corrigir a heterocedasticidade por 
# regressao linear robusta (sandwich) e vamos guardar os 
# valores estimados no objeto "resultados1"
resultados1 <- coeftest(resultados, vcov=vcovHC(resultados,
                                                type="HC"))
# Visualizando os resultados
resultados1

# Agora todas as variaveis sao significativas a 95% de 
# confianca, o que nos permite fazer inferencia com maior
# confiabilidade.


############################################################
#                        ANOVA                             #
############################################################

# Primeiro vamos estimar a ANOVA do modelo sem ajuste para
# heterocedasticidade, para o caso se estivermos trabalhando
# em um modelo sem heterocedasticidade
car::Anova(resultados)

# Agora vamos estimar a ANOVA do modelo com ajuste para 
# heterocedasticidade 
car::Anova(resultados,white.adjust=TRUE)

# A ANOVA nos fornece a analise da variancia de cada 
# variavel do modelo 

#############################################################
# USAREMOS TUDO O QUE FIZEMOS ATE AQUI NA PROXIMA SECAO DE  #
# ESTUDOS. ENTAO EH IMPORTANTE QUE VOCE TENHA "TODOS" OS    #
# COMANDOS EXECUTADOS NESTA ROTINA PARA QUE POSSA EXECUTAR  #
# OS COMANDOS DA PROXIMA SECAO DE ESTUDOS E REPRODUZIR OS   #
# RESULTADOS.                                               #
#############################################################