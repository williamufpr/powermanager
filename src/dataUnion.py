import pandas as pd
from functools import reduce 
import os 

caminho_data = os.path.join(os.path.dirname(__file__), '..', 'data')

# Lista com os arquivos
arquivos = [
    ('PowerMin.csv','potencia_minima'),
    ('PowerMax.csv', 'potencia_maxima'),
    ('PowerAvg.csv', 'potencia_media'),
    ('TempMin.csv', 'temperatura_minima'),
    ('TempMax.csv', 'temperatura_maxima')
    # adicione os demais arquivos aqui
]

# Lista para armazenar os DataFrames
dataframes = []

for arquivo, nome_metrica in arquivos:
    
    caminho_arquivo = os.path.join(caminho_data, arquivo)
    df = pd.read_csv(caminho_arquivo)
    
    # Renomeia as colunas (exceto a primeira que é datetime)
    df.columns = [df.columns[0]] + [f"{nome_metrica}_{col}" for col in df.columns[1:]]
    
    # Converte a coluna de tempo para datetime, se necessário
    df[df.columns[0]] = pd.to_datetime(df[df.columns[0]])
    
    dataframes.append(df)

# Agora faz merge sequencial com base na coluna de datetime
df_merged = reduce(lambda left, right: pd.merge(left, right, on=left.columns[0], how='outer'), dataframes)

# Exporta para um novo CSV unificado
df_merged.to_csv('dados_unificados.csv', index=False)
