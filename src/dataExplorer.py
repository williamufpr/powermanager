import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from translate import translate_equipment

# Se você ainda não carregou:
df_equip = translate_equipment('../data/dataset.csv')

# Verifica informações básicas
print("\n📊 Informações do dataset:")
print(df_equip.info())
print("\n🔢 Estatísticas descritivas:")
print(df_equip.describe())


# -----------------------------
# # 1. Histograma para cada feature
# # -----------------------------
# df_equip.drop(columns=['equipamento']).hist(bins=20, figsize=(16, 10))
# plt.suptitle('Distribuição das Métricas Agregadas por Equipamento')
# plt.tight_layout()
# plt.show()

# # -----------------------------
# # 2. Boxplot para detectar outliers
# # -----------------------------
# plt.figure(figsize=(16, 8))
# sns.boxplot(data=df_equip.drop(columns=['equipamento']))
# plt.xticks(rotation=90)
# plt.title('Boxplot das Métricas Agregadas')
# plt.tight_layout()
# plt.show()

# # -----------------------------
# # 3. Matriz de correlação
# # -----------------------------
# plt.figure(figsize=(12, 10))
# sns.heatmap(df_equip.drop(columns=['equipamento']).corr(), annot=True, fmt=".2f", cmap='coolwarm')
# plt.title('Matriz de Correlação entre Métricas')
# plt.tight_layout()
# plt.show()
