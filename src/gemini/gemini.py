import sys
import os 

from gemini_structure import verificar_estrutura_df
from transform import transformar_dados_para_agrupamento 
from gemini_visual import visualizar_features 




print("Python Environment Information")
      
print(f"Python version: {sys.version}\n")
    
arquivo = "../../data/dataset.csv"  # Substitua pelo caminho do seu arquivo CSV
dataframe1 = verificar_estrutura_df(arquivo,'Date and time')

if dataframe1 is not None:
    print("\nEstrutura do DataFrame verificada com sucesso. Pronto para a próxima etapa.")
    df_features = transformar_dados_para_agrupamento(dataframe1) 
    
    
    if df_features is not None:
        print("\nAtributos dos equipamentos criados com sucesso.")
        visualizar_features(df_features)
else:
    print("Erro ao carregar o DataFrame. Verifique o arquivo e tente novamente.")