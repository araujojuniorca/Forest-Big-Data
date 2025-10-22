# ---------------------------------------------------------------------------- #
'                     ANÁLISE DESCRITIVA - QUADROS                             '
# ---------------------------------------------------------------------------- #
# Pacotes necessários:
# install.packages("knitr")
# install.packages("kableExtra")

library(knitr)
library(kableExtra)
#_______________________________________________________________________________
  # 1 Análise dos dados brutos
  # 1.1 Sumário de estatísticas descritivas para todo o dataset
  'A função sdf_describe vai computar um sumário de estatísticas para cada coluna'
  'Verifique o código que está no arquivo install-connect-read.r'
sdf_describe(dadosBrutos)

  # 1.2 Seleciona variáveis de interesse, calcula as estatísticas e salva 
  #   em um dataframe usando a função collect()
var <- c("IDADEMESES", 
         "ALTDOM", 
         "ALTMED", 
         "DAPMED", 
         "AB", 
         "COVASHA", 
         "VTCCHA")

  # 1.3 Calcula as estatísticas descritivas
resDesc <- sdf_describe(dadosBrutos, cols = var) %>% collect()

  # 1.4 Visualiza os resultados na tela principal
resDesc

#_______________________________________________________________________________
  # 2 Para uma visualização mais organizada

  # 2.1 Converte texto para número e arredonda
cols_num <- setdiff(names(resDesc), "summary")
resDesc[cols_num] <- lapply(resDesc[cols_num], 
                            function(x) round(as.numeric(x), 2))

  # 2.2 Renomeia cada coluna
cols_nomes <- c("Est.", 
                "Idade (meses)", 
               "Alt. Dom. (m)", 
                "Alt. méd. (m)", 
               "Dap Méd. (cm)", 
                "Área Basal (m²)",
               "Covas/ha", 
                "Volume/ha")
names(resDesc)[1:8] <- cols_nomes

  # 2.3 Utiliza o pacote knitr para organizar a visualização
kable(resDesc, caption = "Estatística descritiva para dados brutos", align = "c")

  # 2.4 Utiliza o pacote kableExtra para melhorar ainda mais e salvar como HTML
kable(resDesc, 
      caption = "Estatística descritiva para dados brutos", 
      align = "c") %>%
        kable_styling(full_width = FALSE, 
                      position = "center", 
                      bootstrap_options = c("striped", "hover"))

#_______________________________________________________________________________
  # 3. Filtrando dados
  # 3.1 Dados com idade acima de 18 meses e volume acima de 0
dadosF <- dadosBrutos %>%
            filter(IDADEMESES > 18, VTCCHA > 0)

  # 3.2 Conferindo o total de observações
sdf_dim(dadosF)

  # 3.3 Resumo estatístico para dados filtrados
resDescF <- sdf_describe(dadosF, cols = var) %>% 
              collect()

  # 3.4 Converte texto para número e arredonda
cols_num <- setdiff(names(resDescF), "summary")
resDescF[cols_num] <- lapply(resDescF[cols_num], 
                             function(x) round(as.numeric(x), 2))

  # 3.5 Renomeia cada coluna
names(resDescF)[1:8] <- cols_nomes

  # 3.6 Apresenta o quadro resumo
kable(resDescF, 
      caption = "Estatística descritiva para dados filtrados", 
      align = "c") %>%
        kable_styling(full_width = FALSE, 
                      position = "center", 
                      bootstrap_options = c("striped", "hover"))

#_______________________________________________________________________________
  # 4. Filtrando dados - 7 anos
  # 4.1 Dados com idade em anos igual a 7 e volume acima de 0
dadosF7anos <- dadosBrutos %>%
  filter(VTCCHA > 0, IDADEANOS == 7)

  # 4.2 Conferindo o total de observações
sdf_dim(dadosF7anos)

  # 4.3 Resumo estatístico para dados filtrados
resDescF7 <- sdf_describe(dadosF7anos, cols = var) %>% 
               collect()

  # 4.4 Converte texto para número e arredonda
cols_num <- setdiff(names(resDescF7), "summary")
resDescF7[cols_num] <- lapply(resDescF7[cols_num], 
                              function(x) round(as.numeric(x), 2))

  # 4.5 Renomeia cada coluna
names(resDescF7)[1:8] <- cols_nomes

  # 4.6 Apresenta o quadro resumo
kable(resDescF7, 
      caption = "Estatística descritiva para dados anos 7 anos", 
      align = "c") %>%
        kable_styling(full_width = FALSE, 
                      position = "center", 
                      bootstrap_options = c("striped", "hover"))

#_______________________________________________________________________________
  # 5 Análise dos dados agrupados
  'Não há uma função nativa relacionada ao sdf_describe'

  # 5.1 Função para agregar os dados
summarise_all_fnc <- function(tbl, group_cols, cols) {
  tbl %>%
    group_by(across(all_of(group_cols))) %>% 
    summarise(across(all_of(cols), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      min  = ~min(.x, na.rm = TRUE),
      max  = ~max(.x, na.rm = TRUE)
    ))) %>%
      collect()}

  # 5.2 Visualizar os nomes das colunas e define o agrupamento
dadosF7anos %>% glimpse()
grupo <- c("PROPRIETARIO",
           "REGIAO", 
           "REGIMEMANEJO")

  # 5.3 Calcula as estatisticas por agrupamento
resDescEst <- summarise_all_fnc(dadosF7anos,
                                group_cols = grupo,  # agrupa por duas colunas
                                cols = var)

  # 5.4 Converte texto para número e arredonda
cols_num <- setdiff(names(resDescEst), grupo)
resDescEst[cols_num] <- lapply(resDescEst[cols_num], 
                               function(x) round(as.numeric(x), 2))

  # 5.5 Apresenta o quadro resumo
kable(resDescEst, 
      caption = "Estatística descritiva para dados agrupados", 
      align = "c") %>%
        kable_styling(full_width = FALSE, 
                      position = "center", 
                      bootstrap_options = c("striped", "hover"))

#_______________________________________________________________________________
  # 6. Removendo objetos desnecessarios
remove(resDesc, resDescF, dadosMPlot,resDescEst, resDescF7, dadosF, dadosBPlot)
remove(corr_matrix_da, corr_matrix_db, var, cols_nomes, cols_num, grupo, variaveis)

