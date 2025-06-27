import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Configurar o estilo dos gráficos
sns.set_style("whitegrid")

def visualizar_features(df_features, num_hist_box_plots=8, num_pair_plots=5):
    """
    Gera visualizações (histogramas, box plots e matriz de scatter plots)
    para as features do DataFrame.

    Args:
        df_features (pd.DataFrame): O DataFrame de features dos equipamentos.
        num_hist_box_plots (int): Número máximo de features para gerar hist/box plots.
        num_pair_plots (int): Número máximo de features para gerar o pair plot.
    """
    if df_features.empty:
        print("Erro: DataFrame de features vazio para visualização.")
        return

    print("\n--- Gerando Visualizações das Novas Features ---")

    # Seleciona um subconjunto de features para histogramas e box plots
    # Priorizando médias e desvios padrões, se existirem
    features_to_plot_hb = [col for col in df_features.columns if 'media' in col or 'std' in col]
    if not features_to_plot_hb: # Se não encontrar media/std, pega as primeiras colunas
        features_to_plot_hb = df_features.columns.tolist()

    selected_features_for_hb = features_to_plot_hb[:min(num_hist_box_plots, len(features_to_plot_hb))]

    if selected_features_for_hb:
        plt.figure(figsize=(15, 4 * len(selected_features_for_hb)))
        for i, feature in enumerate(selected_features_for_hb):
            # Histograma
            plt.subplot(len(selected_features_for_hb), 2, 2*i + 1)
            sns.histplot(df_features[feature], kde=True)
            plt.title(f'Histograma da Feature: {feature}', fontsize=12)
            plt.xlabel('Valor da Feature', fontsize=10)
            plt.ylabel('Contagem de Equipamentos', fontsize=10)

            # Box Plot
            plt.subplot(len(selected_features_for_hb), 2, 2*i + 2) # Corrigi para usar selected_features_for_hb aqui
            sns.boxplot(y=df_features[feature])
            plt.title(f'Box Plot da Feature: {feature}', fontsize=12)
            plt.ylabel('Valor da Feature', fontsize=10)
        plt.tight_layout()
        plt.show()
    else:
        print("Nenhuma feature selecionada para Histograma/Box Plot. Verifique as colunas do DataFrame.")

    # Matriz de Scatter Plot (Pair Plot)
    if len(df_features.columns) > 1: # Pair plot requer pelo menos 2 colunas
        # Limita o número de features para o pair plot para evitar gráficos muito grandes
        selected_features_for_pair = df_features.columns[:min(num_pair_plots, len(df_features.columns))]
        if len(selected_features_for_pair) > 1:
            print(f"\nGerando Matriz de Scatter Plots para as {len(selected_features_for_pair)} primeiras features...")
            sns.pairplot(df_features[selected_features_for_pair])
            plt.suptitle('Matriz de Scatter Plots das Principais Features', y=1.02, fontsize=16)
            plt.show()
        else:
            print("Número insuficiente de features para gerar Matriz de Scatter Plot após seleção.")
    else:
        print("Número insuficiente de features para gerar Matriz de Scatter Plot.")

    print("\n--- Visualizações Concluídas ---")