import pandas as pd
import re

# Lê o arquivo unificado
df = pd.read_csv('../data/dados_unificados.csv')

# Função de renomeação personalizada
def renomear_coluna(col):
    if col == 'datetime':
        return col

    # Remove descrições extras
    col = re.sub(r' - Power Consumption.*', '', col)
    col = re.sub(r' - Peak Inlet.*', '', col)

    # Aplica os prefixos customizados
    col = col.replace('potencia_minima_', 'potmin_')
    col = col.replace('potencia_maxima_', 'potmax_')
    col = col.replace('potencia_media_', 'potavg_')
    col = col.replace('temperatura_maxima_', 'tempmax_')
    col = col.replace('temperatura_minima_', 'tempmin_')

    return col

# Aplica a função a todas as colunas
df.columns = [renomear_coluna(col) for col in df.columns]

# Exibe as 10 primeiras colunas após renomear
print(df.columns[-15:])
