# Define as métricas desejadas
metricas = ['potmin', 'potmax', 'potavg'] 

power_df = power_df.rename(columns={"Date and time": "Datetime"})
power_df = power_df.set_index('Datetime')
power_df.index = power_df.to_datetime(power_df.index)


power_df_transp = []

# Processa cada métrica
for metrica in metricas:
    colunas = [col for col in power_df.columns if col.startswith(metrica)]
    
    df_melt = power_df[["Datetime"] + colunas].melt(
        id_vars="Datetime",
        var_name="variavel",
        value_name="valor"
    )
    
    # Extrai o nome do equipamento (remove prefixo da métrica)
    df_melt["equipamento"] = df_melt["variavel"].str.replace(f"{metrica}_", "", regex=False)
    df_melt["metrica"] = metrica
    
    power_df_transp.append(df_melt[["Datetime", "equipamento", "metrica", "valor"]])

# Concatena tudo em um único dataframe longo
power_df_long = pd.concat(power_df_transp, ignore_index=True)


# Visualiza as primeiras linhas
print("Primeiras linhas do DataFrame longo :\n")
print(power_df_long.head(15))
power_df_long.describe()
