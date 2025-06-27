import pandas as pd

def translate_equipment(caminho_arquivo):
    """
    Lê um arquivo CSV com métricas por equipamento ao longo do tempo
    e retorna um DataFrame com uma linha por equipamento, contendo
    estatísticas agregadas (média e desvio padrão) por métrica.

    Parâmetros:
    -----------
    caminho_arquivo : str
        Caminho completo para o arquivo .csv de entrada.

    Retorno:
    --------
    df_equip : pd.DataFrame
        DataFrame com uma linha por equipamento e colunas agregadas por métrica.
    """

    # Lê o arquivo
    df = pd.read_csv(caminho_arquivo)

    # Converte coluna de tempo (boa prática, mesmo que não seja usada aqui)
    df['Date and time'] = pd.to_datetime(df['Date and time'])

    # Remove a coluna de tempo
    df_metrica = df.drop(columns=['Date and time'])

    # Agregação por equipamento
    equipamentos = {}
    for col in df_metrica.columns:
        try:
            metrica, nome_eq = col.split('_', 1)
        except ValueError:
            continue  # ignora colunas fora do padrão esperado

        if nome_eq not in equipamentos:
            equipamentos[nome_eq] = {}

        equipamentos[nome_eq][f'{metrica}_mean'] = df_metrica[col].mean()
        equipamentos[nome_eq][f'{metrica}_std'] = df_metrica[col].std()

    # Monta o DataFrame
    df_equip = pd.DataFrame.from_dict(equipamentos, orient='index')
    df_equip.index.name = 'equipamento'
    df_equip.reset_index(inplace=True)

    return df_equip
