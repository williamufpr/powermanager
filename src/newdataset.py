import pandas as pd
import re
import os

# Caminhos dos arquivos
caminho_base = os.path.join(os.path.dirname(__file__), '..', 'data')
arquivo_entrada = os.path.join(caminho_base, 'dados_unificados.csv')
arquivo_saida = os.path.join(caminho_base, 'dataset.csv')

# Lê o arquivo original SEM alterar
df_original = pd.read_csv(arquivo_entrada)

# Função para renomear colunas conforme regras fornecidas
def renomear_coluna(col):
    if col == 'datetime':
        return col
    col = re.sub(r' - Power Consumption.*', '', col)
    col = re.sub(r' - Peak Inlet.*', '', col)
    col = col.replace('potencia_minima_', 'potmin_')
    col = col.replace('potencia_maxima_', 'potmax_')
    col = col.replace('potencia_media_', 'potavg_')
    col = col.replace('temperatura_maxima_', 'tempmax_')
    col = col.replace('temperatura_minima_', 'tempmin_')
    return col

# Cria uma cópia do DataFrame com colunas renomeadas
df_renomeado = df_original.copy()
df_renomeado.columns = [renomear_coluna(col) for col in df_renomeado.columns]

# Salva a cópia renomeada como dataset.csv
df_renomeado.to_csv(arquivo_saida, index=False)

print("Arquivo 'dataset.csv' criado com colunas renomeadas. O arquivo original foi mantido intacto.")
