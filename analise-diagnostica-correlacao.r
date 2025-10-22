# ---------------------------------------------------------------------------- #
'                     CORRELAÇÃO                                               '
# ---------------------------------------------------------------------------- #
# Pacotes necessários
#install.packages("ggcorrplot")

library(kableExtra)
library(ggcorrplot)
#_______________________________________________________________________________
  # 6. Criando correlograma para analisar os dados brutos

  # 6.1 Indica as variáveis numéricas para calcular a correlação
variaveis <- c("ROTACAO", 
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

  # 6.2 Calcula a matriz de correlação usando ml_corr do Sparklyr
corr_matrix_db <- ml_corr(dadosBrutos, columns = variaveis)

  # 6.2.1 Visualiza na tela do RStudio
corr_matrix_db

  # 6.3 Formata para apresentar os resultados
corr_matrix_db <- as.matrix(corr_matrix_db)
rownames(corr_matrix_db) <- variaveis

  # 6.4 Plota em uma imagem
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_matrix_db,
           lab = TRUE, 
           type = "lower",
           title = "Matriz de Correlação para dados brutos")

  # 6.5 Utiliza o pacote kableExtra para melhorar a apresentação
kable(corr_matrix_db, 
      caption = "Matriz de correlação para dados brutos", 
      align = "c") %>%
        kable_styling(full_width = FALSE,
                      position = "center",
                      bootstrap_options = c("striped", "hover"))

#_______________________________________________________________________________
  # 7. Criando correlograma para analisar os dados agrupados por talhão e medição
  # 7.1 Indica as variáveis numéricas para calcular a correlação
  variaveis <- c("ROTACAO", 
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

  # 7.2 Calcula a matriz de correlação usando ml_corr do Sparklyr
corr_matrix_da <- ml_corr(dadosMedicoes, columns = variaveis)

  # 7.3 Formata para apresentar os resultados
corr_matrix_da <- as.matrix(corr_matrix_da)
rownames(corr_matrix_da) <- variaveis

  # 7.4 Plota em uma imagem
ggcorrplot(corr_matrix_da, 
           lab = TRUE, 
           type = "lower",
           title = "Matriz de Correlação para dados agrupados")

  # 7.5 Utiliza o pacote kableExtra para melhorar a apresentação
kable(corr_matrix_da, 
      caption = "Matriz de correlação para dados agrupados", 
      align = "c") %>%
        kable_styling(full_width = FALSE, 
                      position = "center", 
                      bootstrap_options = c("striped", "hover")) 
