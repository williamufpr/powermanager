##############################################################
#          Unidade 3: Modelos de series temporais            #
#        Secao 3.2: Decomposição de series temporais         # 
##############################################################

# Criando um objeto "pacotes" com os pacotes necessarios
pacotes <- c("readxl","plotly","tidyverse","gridExtra",
             "forecast","TTR","smooth","tidyverse", 
             "tsibble", "fable","tsibbledata", "fpp3",
             "urca")

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

# Vamos aplicar a decomposicao de series temporais para a 
# serie "PIB Mensal BR"

# Vamos carregar o dataset
load("pib.RData")

# Vamos criar um objeto da classe "ts" (time series)
pib_ts <- ts(data = pib[, 2],
             start = c(2004, 1),
             end = c(2021, 9),
             frequency = 12)

# Vamos plotar a serie de tempo
pib %>%
  ggplot() +
  geom_line(aes(x = Data, y = PIB, group = TRUE, 
                color = "PIB"), size = 0.8) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black",
                                    fill = NA),
        legend.position = "none")

# Vamos decompor o PIB pelo modelo aditivo e guardar em um 
# objeto chamado "decpib"
decpib <- decompose(x = pib_ts,
                    type = "additive")

# Vamos visualizar numericamente cada componente
decpib$trend
decpib$seasonal
decpib$random

# Vamos transformar o objeto "decpib" em um dataframe
decpib_df<-data.frame(tempo = pib$Data,
                      serie = unlist(decpib$x),
                      tendencia = unlist(decpib$trend),
                      sazonalidade = unlist(decpib$seasonal),
                      dessazonalizada = pib_ts -
                        decpib$seasonal,
                      erro = unlist(decpib$random)) %>%
  rename(tempo = 1,
         serie = 2,
         tendencia = 3,
         sazonalidade = 4,
         dessazonalizada = 5,
         erro = 6)

# Vamos plotar os componentes da decomposicao todos juntos
# em um grafico
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, 
                color = "Serie values"), size = 1.2) +
  geom_line(aes(x = tempo, y = tendencia, 
                color = "Trend"), size = 1) +
  geom_line(aes(x = tempo, y = sazonalidade, 
                color = "Seasonality"), size = 1.2) +
  geom_line(aes(x = tempo, y = erro, 
                color = "Error"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legend:",
       x = NULL,
       y = NULL) +
  scale_color_manual(values=c("#440154FF", "#3CBB75FF",
                            "#39568CFF", "#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4),
        panel.background = element_rect(fill = "white", 
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", 
                                    fill = NA),
        legend.position = "bottom")

# Vamos plotar os objetos dos componentes em separado em 
# um unico grafico
# Primeiro Vamos gerar um grafico da serie original
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, 
                color = "Serie values")) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Serie values",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#39568CFF")) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   size = 7),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", 
                                    fill = NA),
        legend.position = "none") -> decomp_serie

# Para sazonalidade:
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = sazonalidade, 
                color = "Seasonality")) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seasonality",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#3CBB75FF")) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   size = 7),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black",
                                    fill = NA),
        legend.position = "none") -> decomp_sazonalidade

# Para tendencia:
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = tendencia, 
                color = "Trend")) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Trend",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   size = 7),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", 
                                    fill = NA),
        legend.position = "none") -> decomp_tendencia

# Para o erro:
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = erro, color = "Error")) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Error",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#440154FF")) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   size = 7),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", 
                                    fill = NA),
        legend.position = "none") -> decomp_erro

# Agora vamos juntar as partes da decomposicao em um 
# unico grafico
grid.arrange(decomp_serie,
             decomp_sazonalidade,
             decomp_tendencia,
             decomp_erro,
             ncol = 1)

# Vamos plotar a serie desazonalizada junto com a serie
# original
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, 
                color = "Serie values"), size = 1.2) +
  geom_line(aes(x = tempo, y = dessazonalizada, 
                color = "Unseasoned"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legenda:",
       x = NULL,
       y = NULL) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black",
                                    fill = NA),
        legend.position = "bottom")


# Agora vamos decompor o PIB pelo modelo multiplicativo
decpib = decompose(pib_ts, type = "multiplicative")
plot(decpib)


# Vamos ver os valores numericos da decomposicao
decpib$trend
decpib$seasonal
decpib$random

# Podemos gerar os mesmos graficos como fizemos para
# o outro metodo

##########################################################