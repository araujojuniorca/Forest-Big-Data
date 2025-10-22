# ==============================================================================
'                                K-MEANS                                      '
# ==============================================================================
# 11. Kmeans
library(sparklyr)
library(dplyr)

#_______________________________________________________________________________
  # 1. Preparando os dados
  # 1.1 Indicando as variaveis que serao consideradas para agrupamento
varNum <- c("ROTACAO", 
            "ANOROTACAO", 
            "IDADEMESES", 
            "ALTDOM", 
            "ALTMED", 
            "DAPMED", 
            "AB", 
            "COVASHA", 
            "ARVORESHA", 
            "FUSTESHA",
            "PERDASHA", 
            "VTCCHA")

  # 1.2 Normalizacao dos dados
dadosNorm <- ft_min_max_scaler(x = ft_vector_assembler(dadosMedicoes, 
                                                      input_cols = varNum,
                                                      output_col = "features"),
                               input_col = "features",
                               output_col = "var_normalizadas",
                               min = 0,   
                               max = 1)

#_______________________________________________________________________________
  # 2. Executando o algoritmo
  # 2.1 Rodar o KMeans
modeloNorm <- ml_kmeans(dadosNorm, features_col = "var_normalizadas", k = 3)

  # 2.2 Estimando o agrupamento para cada registro e avaliando o resultado
dadosKnorm <- ml_predict(modeloNorm, dadosNorm)

  # 2.3 avaliando o resultado
  'S(i) = b(i) – a(i) / max {a(i),b(i)}
    S(i) é o coeficiente de silhueta do ponto i;
    a(i) é a distancia média entre “i” e todos os outros pontos de dados no 
  cluster ao qual ele pertence;
    b(i) é a distancia média de “i” a todos os clusters que ele não pertence.
    Entre -1 e 1, 
      +1: clusters são densos e bem separados
       0: clusters estão sobrepostos
      -1: clusterização está incorreta.'
ml_clustering_evaluator(dadosKnorm,
                        features_col = "var_normalizadas",
                        prediction_col = "prediction",
                        metric_name = "silhouette"
)

#_______________________________________________________________________________
  # 3. Mostrando os resultados
  # 3.1 Visualizar algumas linhas
dadosKnorm %>% 
  select(all_of(varNum), prediction) %>% 
    head() %>% 
      collect()

  # 3.2 Trazer para R para plotar no gráfico
dfKnorm <- dadosKnorm %>% collect()

  # 3.3 Gráfico com dois atributos (ex: ALTDOM vs ALTMED)
ggplot(dfKnorm, 
       aes(x = PERDASHA, 
           y = VTCCHA, 
           color = as.factor(prediction))) +
  geom_point(size = 2) +
  labs(color = "Cluster",
       title = "Agrupamentos obtidos por K-means (normalizado)",
       x = "Perdas (N/ha)",
       y = "Volume (m³/ha)") +
  theme_minimal() +
  theme(
        axis.title.x = element_text(size = 12, face = "bold"),  # aumenta fonte e negrito do x
        axis.title.y = element_text(size = 12, face = "bold"),  # label do y
        axis.text.x = element_text(size = 12),  # valores do eixo x
        axis.text.y = element_text(size = 12),  # valores do eixo y
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#_______________________________________________________________________________
  # 4. Testando valores para a quantidade de grupos (k)
  # 4.1 Teste valor K
library(purrr)
k_values <- 2:10  # Testar k de 2 até 10
avaliacoes <- map_df(k_values, 
                     function(k) {
                        modelo <- ml_kmeans(dadosNorm, 
                                            features_col = "var_normalizadas", 
                                            k = k)
                        pred <- ml_predict(modelo, dadosNorm)
                        sil <- ml_clustering_evaluator(pred,
                                                       features_col = "var_normalizadas",
                                                       prediction_col = "prediction",
                                                       metric_name = "silhouette")
                        tibble(k = k, silhouette = sil)})

  # 4.2 Mostrando na tela
print(avaliacoes)

  # 4.3 Plotando no gráfico
ggplot(avaliacoes, 
       aes(x = k, 
           y = silhouette)) +
  geom_line(color = "blue") +
  geom_point(size = 3, 
             color = "red") +
  labs(
    title = "Silhouette Score para diferentes valores de k",
    x = "Número de clusters (k)",
    y = "Silhouette Score") +
  theme_minimal() +
  theme(
      axis.title.x = element_text(size = 12, face = "bold"),  # aumenta fonte e negrito do x
      axis.title.y = element_text(size = 12, face = "bold"),  # label do y
      axis.text.x = element_text(size = 12),  # valores do eixo x
      axis.text.y = element_text(size = 12),  # valores do eixo y
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.01)  )

#_______________________________________________________________________________
# 5 Removendo objetos desnecessarios
remove(modeloNorm, dadosKnorm, dfKnorm, k_values, avaliacoes)
