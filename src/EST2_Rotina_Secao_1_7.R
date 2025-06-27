#################################################################
#               Unidade 1: Analise de regressao                 #
#     Secao 1.7: Transformação Box-Cox e outras alternativas    # 
#################################################################

# Instalando os pacotes necessarios
# install.packages("AID")

# Carregando os pacotes necessarios
library(AID)

# Vamos usar o dataset nativo do "R" chamado "textile"
data("textile")

###############################################################
#           Transformacao Box-Cox para uma variavel           #
###############################################################

# Vamos evitar a notacao cientifica dos resultados
options(scipen = 999)

# Vamos testar primeiro se a variavel "textile" eh normalmente
# distribuida
shapiro.test(textile$textile)

# Como p-value < 0,05 os dados nao sao normalmente distrbuidos

# Vamos estimar o parametro lambda baseado no teste de 
# Shapiro-Wilk e vamos guardar no objeto "out"
out <- boxcoxnc(textile[,1], method = "sw")

# O resultado nos revela que o lambda estimado eh -0.06 e que
# com esse lambda a distribuicao se torna normal

# Se quisermos ver o lambda estimado:
out$lambda.hat 

# Se quisermos ver o p-value da estimativa:
out$p.value
# Se p-value > 0.05 a transformacao foi bem sucedida

# Se quisermos ver os valores transformados:
out$tf.data

# Se quisermos ver a media e o intervalo de confianca dos
# dados apos a transformacao (retornados ao range de 
# valores de antes da transformacao):  
confInt(out) 

# Se quisermos ver graficamente a comparacao do "antes" e 
# depois da transformacao: 
out2 <- boxcoxnc(textile[,1], method = "sw")

# Uma alternativa eh ver qual o lambda utilizando o teste
# de Shapiro-Francia, vamos guardar o resultado em "out3"
out3 <- boxcoxnc(textile[,1], method = "sf")
out3$lambda.hat
# O resultado eh o mesmo de antes

# Se quisermos ver o p-value por Shapiro-Francia
out3$p.value

# Se quisermos ver os valores transformados
out3$tf.data

# Se quisermos ver a media e o intervalo de confianca
confInt(out3)


##############################################################
#        Transformacao Box-Cox para modelos lineares         #
##############################################################

# Vamos tilizar o dataset nativo do "R" chamado "trees"
# vamos salva-lo no objeto chamado "trees" no formato matriz
trees=as.matrix(trees)

# Vamos estimar o modelo Box-Cox, nossa variavel dependente
# eh "Volume" e as variaveis explicativas sao grossura(Girth)
# e a altura(height)
outlm <- boxcoxlm(x = trees[,1:2], y = trees[,3])

# A transformação foi bem sucedida, o p-value > 0.05 e o 
# lambda eh 0.31

# Se quisermos ver o lambda:
outlm$lambda.hat 

# Se quisermos ver o p-value:
outlm$p.value 

# Se quisermos ver os residuos da regressao com dados 
# transformados:
outlm$tf.residuals

# Nestes casos o teste de normalidade eh feito sobre os
# residuos
# Vamos fazer o teste:
shapiro.test(outlm$tf.residuals)
# Como o p-value > 0.05 a regressao eh normalmente 
# distribuida

# Se quisermos ver os dados transformados da variavel 
# dependente (Y)
outlm$tf.data 

# O detalhe eh que o resultado da predicao deve ser 
# retornado aos dados normais de acordo com a expressao 
# de transformacao do lambda:
# Ybox-cox = ((Y^lambda)-1)/lambda
# ou seja:
# Y = (Ybox-cox . lambda - 1)^1/lambda
# Isso deve ser calculado, inclusive para os resultados 
# nos intervalos de confianca

#########################################################