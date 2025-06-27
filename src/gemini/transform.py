import pandas as pd
import numpy as np
from gemini_structure import verificar_estrutura_df

def transformar_dados_para_agrupamento(df_original):
    """
    Realiza a engenharia de atributos no DataFrame de séries temporais,
    transformando-o em um DataFrame onde cada linha é um equipamento
    e as colunas são as características resumidas de seu consumo,
    considerando múltiplas métricas por equipamento.

    Args:
        df_original (pd.DataFrame): O DataFrame original com séries temporais
                                    (DataHora como índice, colunas como metrica_equipamentoID).

    Returns:
        pd.DataFrame: Um novo DataFrame pronto para agrupamento, com Equipamento_ID como índice.
                      Retorna None se o DataFrame de entrada estiver vazio.
    """
    if df_original.empty:
        print("Erro: DataFrame de entrada vazio para transformação.")
        return None

    print("\n--- Iniciando Engenharia de Atributos para Agrupamento ---")

    # Dicionário para armazenar as features, onde a chave é o Equipamento_ID
    # e o valor é outro dicionário de features para aquele equipamento.
    equipamentos_features = {}

    # 1. Mapear cada coluna para seu Equipamento_ID e Tipo de Métrica
    col_info = [] # Lista de tuplas: (Equipamento_ID, Métrica_Tipo, Nome_Coluna_Original)
    equipamento_ids_unicos = set()

    for col_name in df_original.columns:
        # A lógica para extrair métrica e ID do equipamento é mais complexa agora.
        # Vamos tentar separar a métrica do ID, assumindo que o ID é o que vem
        # APÓS a primeira parte que é a métrica (potmin, potmax, tempmin, etc.)
        # e que o ID pode conter hífens ou outros underscores.

        # Exemplo: 'potmin_SMV340200' -> métrica='potmin', id='SMV340200'
        # Exemplo: 'potmin_idrac-9DQGPV3' -> métrica='potmin', id='idrac-9DQGPV3'
        
        # Encontra a posição do primeiro underscore. Se não houver, trata como um ID único.
        first_underscore_idx = col_name.find('_')

        if first_underscore_idx != -1:
            metrica_tipo = col_name[:first_underscore_idx] # Ex: 'potmin'
            equipamento_id = col_name[first_underscore_idx+1:] # Ex: 'SMV340200' ou 'idrac-9DQGPV3'
        else:
            # Caso a coluna não siga o padrão esperado (ex: se for apenas 'equipamento1')
            print(f"Atenção: Coluna '{col_name}' não segue o padrão 'metrica_equipamentoID'. Será tratada como métrica e ID únicos.")
            metrica_tipo = col_name
            equipamento_id = col_name + '_unidentified'

        col_info.append((equipamento_id, metrica_tipo, col_name))
        equipamento_ids_unicos.add(equipamento_id)

    print(f"\nIdentificados {len(equipamento_ids_unicos)} equipamentos únicos.")
    # print(f"IDs de equipamentos únicos: {sorted(list(equipamento_ids_unicos))[:5]}...") # Mostra os primeiros 5 para depuração

    # Inicializar a estrutura de features para cada equipamento
    for eq_id in equipamento_ids_unicos:
        equipamentos_features[eq_id] = {}

    # 2. Calcular estatísticas para cada combinação de Métrica e Equipamento
    for eq_id, metrica_tipo, col_name_original in col_info:
        series = df_original[col_name_original]

        # Calcula as estatísticas e as armazena no dicionário do equipamento
        # O nome da feature será no formato 'estatistica_metrica' (ex: 'media_potmin')
        equipamentos_features[eq_id][f'media_{metrica_tipo}'] = series.mean()
        equipamentos_features[eq_id][f'mediana_{metrica_tipo}'] = series.median()
        equipamentos_features[eq_id][f'std_{metrica_tipo}'] = series.std()
        equipamentos_features[eq_id][f'min_{metrica_tipo}'] = series.min()
        equipamentos_features[eq_id][f'max_{metrica_tipo}'] = series.max()
        equipamentos_features[eq_id][f'amplitude_{metrica_tipo}'] = series.max() - series.min()
        equipamentos_features[eq_id][f'q25_{metrica_tipo}'] = series.quantile(0.25)
        equipamentos_features[eq_id][f'q75_{metrica_tipo}'] = series.quantile(0.75)
        # Adicione outras estatísticas se considerar relevante!

    # Converte o dicionário de features em um DataFrame
    df_features = pd.DataFrame.from_dict(equipamentos_features, orient='index')
    df_features.index.name = 'Equipamento_ID'

    print("\n--- DataFrame de Atributos (Features) dos Equipamentos Criado ---")
    print(df_features.head())
    print(f"\nShape do DataFrame de Features: {df_features.shape}")
    print("\nEstatísticas Descritivas das Novas Features:")
    print(df_features.describe())

    # Tratamento de NaNs que podem surgir na criação de features (ex: std de coluna constante)
    if df_features.isnull().sum().sum() > 0:
        print("\n--- Verificação e Preenchimento de NaNs no DataFrame de Features ---")
        print(df_features.isnull().sum()[df_features.isnull().sum() > 0]) # Mostra apenas colunas com NaNs
        print("Preenchendo NaNs no DataFrame de Features com 0 (considerar outra estratégia se aplicável).")
        df_features.fillna(0, inplace=True) # Preenche NaNs com 0
        print("\nNaNs após preenchimento:")
        print(df_features.isnull().sum()[df_features.isnull().sum() > 0])


    return df_features


if __name__ == "__main__":

    # Exemplo de uso:
    nome_arquivo = '../../data/dataset.csv'  # Substitua pelo caminho do seu arquivo CSV
    df_carregado = verificar_estrutura_df(nome_arquivo)

    if df_carregado is not None:
        print("\nEstrutura do DataFrame verificada com sucesso. Pronto para a próxima etapa.")
    # Exemplo de uso:
    df_features_para_agrupamento = transformar_dados_para_agrupamento(df_carregado)
    df_carregado.describe() 
    