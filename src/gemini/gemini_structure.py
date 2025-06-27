import pandas as pd
import numpy as np

def verificar_estrutura_df(caminho_arquivo, coluna_data_hora='Date and time'): # Nome da coluna ajustado
    """
    Carrega um arquivo CSV, verifica sua estrutura e exibe informações iniciais.

    Args:
        caminho_arquivo (str): Caminho para o arquivo CSV.
        coluna_data_hora (str, optional): Nome da coluna de data/hora.
                                        Defaults to 'Date and time'.

    Returns:
        pd.DataFrame: O DataFrame carregado e com a coluna de data/hora como índice.
                      Retorna None se o arquivo não for encontrado ou houver erro.
    """
    try:
        # Tenta carregar o CSV e parsear a coluna de data/hora
        df = pd.read_csv(caminho_arquivo, parse_dates=[coluna_data_hora])
        df = df.set_index(coluna_data_hora)
        print(f"DataFrame '{caminho_arquivo}' carregado com sucesso!")
    except FileNotFoundError:
        print(f"Erro: O arquivo '{caminho_arquivo}' não foi encontrado. Verifique o nome e o caminho.")
        return None
    except KeyError:
        print(f"Erro: A coluna de data/hora '{coluna_data_hora}' não foi encontrada no arquivo '{caminho_arquivo}'.")
        print(f"Colunas disponíveis: {df.columns.tolist()}")
        return None
    except Exception as e:
        print(f"Ocorreu um erro ao carregar ou processar o arquivo: {e}")
        return None

    print("\n--- Informações Iniciais do DataFrame ---")
    print("\nPrimeiras 5 linhas:")
    print(df.head())
    print("\nÚltimas 5 linhas:")
    print(df.tail())
    print("\nInformações sobre Tipos de Dados e Valores Não-Nulos:")
    print(df.info())

    print("\n--- Estatísticas Descritivas das Colunas Numéricas ---")
    df_numeric = df.select_dtypes(include=[np.number])
    print(df_numeric.describe())

    print("\n--- Verificação de Valores Ausentes por Coluna ---")
    print(df.isnull().sum())

    # Tratamento simples de NaNs (imputação com a média) para as colunas numéricas
    print("\n--- Tratando Valores Ausentes (Imputação com Média para Colunas Numéricas) ---")
    for col in df_numeric.columns:
        if df_numeric[col].isnull().sum() > 0:
            mean_val = df_numeric[col].mean()
            df_numeric[col].fillna(mean_val, inplace=True)
            # Como df_numeric é uma cópia, precisamos garantir que as mudanças
            # reflitam no df original ou retornemos o df_numeric se ele for o principal.
            # Para manter a consistência com o uso posterior, vamos atualizar o df original
            df[col] = df_numeric[col]

    print("\n--- Verificação de Valores Ausentes Após Tratamento ---")
    print(df.isnull().sum())

    return df

if __name__ == "__main__":

    # Exemplo de uso:
    nome_arquivo = '../../data/dataset.csv'  # Substitua pelo caminho do seu arquivo CSV
    df_carregado = verificar_estrutura_df(nome_arquivo)

    if df_carregado is not None:
        print("\nEstrutura do DataFrame verificada com sucesso. Pronto para a próxima etapa.") 
