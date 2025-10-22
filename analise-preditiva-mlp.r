# ==============================================================================
'                              RNA PARA CLASSIFICACAO                          '
# ==============================================================================
library(ggplot2)
#_______________________________________________________________________________
  # 1. Preparando os dados
  # 1.1 Obtendo os dados, filtrando e criando variavel de saida
dadosRNA <- dadosF7anos %>%
  mutate(VTCCHA_class = case_when(VTCCHA < 100 ~ 0,
                                  VTCCHA < 200 ~ 1,
                                  VTCCHA < 300 ~ 2,
                                  VTCCHA < 400 ~ 3,
                                  TRUE ~ 4)) %>%
    na.omit() %>% # remove registros com NA (nulos/vazios)
      mutate(VTCCHA_class = as.integer(VTCCHA_class)) #define a variavel de saida como int

  # 1.2 Definindo variáveis numericas
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
            "PERDASHA")

  # 1.3 Normalizando variáveis numéricas
dadosNorm <- ft_min_max_scaler(x = ft_vector_assembler(dadosRNA, 
                                                       input_cols = varNum,
                                                       output_col = "features"),
                               input_col = "features",
                               output_col = "var_normalizadas",
                               min = 0,
                               max = 1)

  # 1.4 Divisao dos dados para treinamento e validacao
set.seed(123)
particoes <- dadosNorm %>% 
  sdf_random_split(train = 0.7, 
                   valid=0.3)
train <- particoes$train
valid <- particoes$valid

  # 1.5 Conferir a quantidade de dados em cada grupo
glimpse(train)
sdf_dim(train)
sdf_dim(valid)


#_______________________________________________________________________________
  # 2. Configurando e treinando a RNA

  # 2.1 Definindo arquitetura: número de neurônios por camada
  ' Entrada: length(varNum) = 11 neuronios
    Ocultas: 8 e 3 neurônios
    Saída: Número de classes = 5 neurônios
  '
camadas <- c(length(varNum), 10, 10, 5)

# 2.2 Treinando o modelo MLP para classificação
mlp_model <- ml_multilayer_perceptron_classifier(x = train,
                                                 features_col = "var_normalizadas",
                                                 label_col = "VTCCHA_class",
                                                 layers = camadas,
                                                 block_size = 128,
                                                 seed = 123,
                                                 max_iter = 100)

#_______________________________________________________________________________
  # 3. Avaliando os resultados
  # 3.1 Aplicando o modelo para dados de treinamento e validacao
pred_Tre <- ml_transform(mlp_model, train)
pred_Val <- ml_transform(mlp_model, valid)

  # 3.2 Lista de conjuntos de dados e métricas
datasets <- list(Treino = pred_Tre, 
                 Validacao = pred_Val)
metricas <- c("accuracy", 
              "f1", 
              "weightedPrecision", 
              "weightedRecall")

# 3.3 Inicializa tibble para armazenar resultados e executa o loop
resultados <- tibble(dataset = character(),
                     metrica = character(),
                     valor = numeric())

for (dataset_name in names(datasets)) {
  df <- datasets[[dataset_name]]
  for (metrica in metricas) {
    valor <- ml_multiclass_classification_evaluator(
      df,
      label_col = "VTCCHA_class",
      prediction_col = "prediction",
      metric_name = metrica)
    resultados <- resultados %>%
      add_row(
        dataset = dataset_name,
        metrica = metrica,
        valor = valor)
  }
}
resRNA <- resultados

  # 3.4 Mostrar resultados
library(knitr)
kable(resRNA, 
      caption = "Métricas para avaliação da RNA",
      align = "c")


  # 3.5 Trazer apenas colunas necessárias para plotar no gráfico
df_pred <- pred_Val %>%
  select(VTCCHA_class, prediction) %>%
    collect()

conf_matrix_rna <- df_pred %>%
  count(VTCCHA_class, prediction)

  # 3.6 Mostra a matriz de confusão no gráfico
plotRNA <- ggplot(conf_matrix_rna, 
                  aes(x = factor(VTCCHA_class), 
                      y = factor(prediction), 
                      fill = n)) +
            geom_tile(color = "gray", 
                      linewidth = 0.5) +
            geom_text(aes(label = n), 
                      color = "black", 
                      size = 4) +
            scale_fill_gradient(low = "white", 
                                high = "steelblue") +
            scale_x_discrete(limits = c("0","1","2","3","4")) +
            scale_y_discrete(limits = c("0","1","2","3","4")) +
            labs(
              title = "Matriz de confusão para RNA",
              x = "Classe Observada",
              y = "Classe Predita",
              fill = "Contagem") +
            theme_minimal() +
            theme(
              axis.title.x = element_text(size = 12, face = "bold"),  # aumenta fonte e negrito do x
              axis.title.y = element_text(size = 12, face = "bold"),  # label do y
              axis.text.x = element_text(size = 12),  # valores do eixo x
              axis.text.y = element_text(size = 12),  # valores do eixo y
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.01)
            )
plotRNA

remove(camadas, mlp_model, pred_Tre, pred_Val, datasets, metricas, resultados, df, valor)
remove(dataset_name,  metrica, conf_matrix, df_pred)
