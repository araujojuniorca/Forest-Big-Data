# Limpar memória do RStudio
rm(list = ls())

# ---------------------------------------------------------------------------- #
'                     INSTALACAO DO PACOTE SPARKLYR                            '
# ---------------------------------------------------------------------------- #
# 1. Instala o pacote sparklyr a partir do CRAN (apenas se necessário)
#install.packages("sparklyr")

# 2. Carrega os pacotes necessários
library(sparklyr)
library(dplyr)

# ---------------------------------------------------------------------------- #
'                     CONEXAO COM O SPARK                                      '
# ---------------------------------------------------------------------------- #

# É necessário configurar o WSL e instalar o Hadoop
# Em seguida, inicie o HDFS no WSL

# 1. Configura a conexao
config <- spark_config()
config$spark.hadoop.fs.defaultFS <- "hdfs://localhost:9000"

# 2. Cria uma conexao com o Spark
sc <- spark_connect(master = "local[*]", 
                    config = config, 
                    app_name = "BigData",
                    enable_hive_support = FALSE)

# 3. Verificar se está OK (TRUE)
connection_is_open(sc)

# 4. Desconectar o Spark (após realizar as análises)
spark_disconnect(sc)


# ---------------------------------------------------------------------------- #
'                     LER ARQUIVO CSV E ORGANIZAR DADOS                        '
# ---------------------------------------------------------------------------- #
#_______________________________________________________________________________
# 1. Lendo os dados de inventario que estao no HDFS

# 1.1 Cria uma tabela temporaria no Spark de nome Inv
dadosInv <- spark_read_csv(
  sc,
  name = "Inv",
  path = "hdfs:///data/raw/inventario/dados_IFC_SZ.csv",
  header = TRUE,
  infer_schema = TRUE,
  delimiter = ";"
)

# 1.2 Visualizando os dados na tela
'glimpse é uma funcao do pacote dplyr para visualizar conteudo de um tbl_spark
(objeto do Spark que representa uma tabela remota)'
dadosInv %>% glimpse()

#_______________________________________________________________________________
# 2. Corrigindo a base de dados

# 2.1 Corrigindo o tipo da variavel V_VTCC_m3f
dadosInv <- dadosInv %>%
  mutate(V_VTCC_m3f = as.numeric(V_VTCC_m3f))

# 2.2 Visualizando novamente os dados na tela
dadosInv %>% glimpse()

# 2.3 Verifica a quantidade de dados
sdf_dim(dadosInv)

#_______________________________________________________________________________
# 3. Convertendo para um formato otimizado

# 3.1 Converter para formato Parquet
'O formato Parquet é binário, comprimido e colunar 
 Torna a leitura e consultas muito mais rápidas.
 Pode salvar no HDFS ou localmente'
spark_write_parquet(
  dadosInv,
  path = "hdfs:///data/raw/inventario/parquet/",
  mode = "overwrite"
)

# 3.2 Faz a leitura dos dados comprimidos
dadosPqt <- spark_read_parquet(
  sc, 
  name = "invpqt",
  path = "hdfs:///data/raw/inventario/parquet/"
)

# 3.3 Visualizando novamente os dados na tela
dadosPqt %>% glimpse()

# 3.4 Verifica a quantidade de dados
sdf_dim(dadosPqt)

#_______________________________________________________________________________
# 4. Particionando os dados
' Particionar melhora o desempenho em queries filtradas
  Assim, o Spark só lê as partições relevantes quando você 
  filtra por Região ou Regime de Manejo'

# 4.1 Particionar os dados (por Região e Regime de Manejo)
spark_write_parquet(
  dadosPqt,
  path = "hdfs:///data/raw/inventario/particionado/",
  partition_by = c("REGIAO", "REGIMEMANEJO"),
  mode = "overwrite"
)

# 4.2 Leia o Parquet particionado
dadosPart <- spark_read_parquet(
  sc,
  name = "invpart",
  path = "hdfs:///data/raw/inventario/particionado/"
)

# 4.3 Visualizando novamente os dados na tela
glimpse(dadosPart)
dadosPart %>% glimpse()

# 4.4 Verifica a quantidade de dados
sdf_dim(dadosPart)

# 4.5 Altera o nome do objeto
dadosBrutos <- dadosPart



#_______________________________________________________________________________
# 5. Consolidando os dados por talhao, rotacao e ano de medicao

# 5.1 Identificar tipos de colunas
schema_info <- sdf_schema(dadosBrutos)

# 5.2 Colunas numéricas
num_cols <- names(schema_info)[sapply(schema_info, function(x) x$type %in% 
                                        c("DoubleType", "IntegerType", "FloatType", "LongType"))]

# 5.3 Colunas categóricas (não numéricas)
cat_cols <- setdiff(colnames(dadosBrutos), num_cols)

# 5.4 Remover colunas específicas do agrupamento
cat_cols <- setdiff(cat_cols, c("DATAATUALIZACAO", "DATAMEDICAO", "PARCELA","AREAPARCELAM2"))

# 5.5 Calcular médias por grupo
dadosMedicoes <- dadosBrutos %>%
  group_by(across(all_of(cat_cols))) %>%
  summarise(across(all_of(num_cols), ~mean(.x, na.rm = TRUE))) #%>%
  #collect()

glimpse(dadosMedicoes)

# 5.6 Verifica a quantidade de dados
sdf_dim(dadosMedicoes)


#_______________________________________________________________________________
# 6. Removendo objetos desnecessarios

remove(cat_cols, num_cols, schema_info)
remove(dadosInv, dadosPqt, dadosPart)
