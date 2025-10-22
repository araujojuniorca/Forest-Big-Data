# ---------------------------------------------------------------------------- #
'                     ANÁLISE DESCRITIVA - GRÁFICOS                             '
# ---------------------------------------------------------------------------- #
#_______________________________________________________________________________
  # 6. Criando histograma para analisar os dados brutos
  ' A ideia central é consolidar os dados usando o Spark e trazer apenas os 
    resultados para o R e então usar o ggplot'

  # 6.1 Trazer os dados brutos para o R
dadosBPlot <- dadosBrutos %>% collect()

  # 6.2 Plotar um histograma por regime de manejo
library(ggplot2)
ggplot(dadosBPlot, 
       aes(x = VTCCHA, 
           fill = factor(REGIMEMANEJO))) +
  scale_fill_brewer(palette = "Accent") +
  geom_histogram(binwidth = 10, 
                 color = "gray", 
                 alpha = 0.7) +
  facet_wrap(~REGIMEMANEJO, 
             scales = "free_y") + 
  labs(
      title = "Histograma de volume/ha para cada regime de manejo (parcelas)",
      x = "Volume (m²/ha)",
      y = "Frequência",
      fill = "Regime de Manejo") +
  theme_minimal() +
  theme(
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.01))

  # 6.3 Filtrar dados brutos e coletar para o R
dadosBPlot <- dadosBPlot %>%
  filter(!is.na(VTCCHA), !is.na(IDADEANOS), IDADEANOS >= 3, IDADEANOS<= 8) %>%
    collect() 

# 6.4 Plotar um histograma por regime de manejo e idade em anos
ggplot(dadosBPlot, 
       aes(x = VTCCHA,
           fill = factor(REGIMEMANEJO))) +
  scale_fill_brewer(palette = "Accent") +
  geom_histogram(binwidth = 10, 
                 color = "gray", 
                 alpha = 0.7) +
  facet_wrap(~INTIDADEANOS+REGIMEMANEJO, 
             scales = "free_y") + 
  labs(
       title = "Histograma de volume/ha para cada regime de manejo e idade (parcelas)",
       x = "Volume (m²/ha)",
       y = "Frequência",
       fill = "Regime de Manejo") +
  theme_minimal() +
  theme(
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12, face = "bold"),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.01))

  # 6.5 Remover objeto não utilizado
remove(dadosBPlot)

#_______________________________________________________________________________
  # 7. Criando histograma para analisar os dados consolidados por talhão e medição
  # 7.1 Preparando os dados
dadosMedicoes <- dadosMedicoes %>%
  mutate(IDADEANOS = floor(IDADEMESES / 12))

  # 7.2 Coletar dados relevantes para o R
dadosMPlot <- dadosMedicoes %>%
  filter(!is.na(VTCCHA), !is.na(IDADEANOS), IDADEANOS >= 3, IDADEANOS<= 8) %>%
    collect() 

  # 7.3 Plotar um histograma por regime de manejo
ggplot(dadosMPlot, 
       aes(x = VTCCHA,fill = factor(REGIMEMANEJO))) +
  geom_histogram(binwidth = 10, 
                 color = "gray", 
                 alpha = 1) +
  facet_wrap(~IDADEANOS+REGIMEMANEJO, 
             scales = "free_y") +  # um histograma por idade
  scale_fill_brewer(palette = "Accent") +
  labs(
       title = "Histograma de volume/ha para cada regime de manejo e idade (talhões)",
       x = "Volume (m²/ha)",
       y = "Frequência",
       fill = "Regime de Manejo") +
  theme_minimal() +
  theme(
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.01))

  # 7.3 Plotar um histograma por idade
ggplot(dadosMPlot, 
       aes(x = VTCCHA, fill = factor(IDADEANOS))) +
  geom_histogram(binwidth = 10, 
                 color = "gray", 
                 alpha = 1) +
  facet_wrap(~IDADEANOS, 
             scales = "free_y") +  # um histograma por idade
  scale_fill_brewer(palette = "Accent") +
  labs(
       title = "Histograma de volume/ha para cada idade (talhões)",
       x = "Volume (m²/ha)",
       y = "Frequência",
       fill = "Idade (anos)") +
  theme_minimal() +
  theme(
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.01))

  # 7.4 Remover objeto não utilizado
remove(dadosMPlot)

