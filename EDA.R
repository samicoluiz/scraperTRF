# Normalizando os dois dataframes
dfTRF1_analise <- jurisprudencia_recente %>%
  select(Tipo, Classe, "Número", Data, Origem, "Órgão julgador",  "Relator(a)", Ementa, "Decisão", Sucessos)

dfTRF4_analise <- dfTRF4_AC %>%
  select(tipo, classe, nproc, data, origem, orgjulg, relator, ementa, decisao, sucessos)

dfTRF4_analise$classe <- as.factor(dfTRF4_analise$classe)
dfTRF4_analise$data <- as.Date(dfTRF4_analise$data, format = "%d/%m/%Y")

df_TRF <- bind_rows(dfTRF1_analise, dfTRF4_analise) %>% 
  mutate(origem = str_replace(df_TRF$origem, "TRF - PRIMEIRA REGIÃO", "TRF1"))

#### PRIMEIRO PLOT ####
# Razão de sucesso por tribunal
casos_por_tribunal <- df_TRF %>% group_by(origem) %>% count()

sucessos_por_tribunal <- df_TRF %>% group_by(origem) %>%
  summarise(razao_sucessos = sum(sucessos, na.rm = TRUE)/n()) %>% 
  inner_join(casos_por_tribunal, by = 'origem') %>% 
  rename('obs_por_tribunal' = 'n')


plot_sucessos_por_tribunal <- ggplot(data = sucessos_por_tribunal, aes(x=origem, y=razao_sucessos, fill=origem)) +
  geom_col(width=.33, show.legend = F) +
  scale_fill_brewer(palette="Set1") +
  xlab('Origem') +
  ylab('Razão de Sucessos') +
  geom_text(aes(label = round(razao_sucessos, 3)), vjust = 1.75, colour = "white") +
  ggtitle('Razão de Sucessos por Tribunal', subtitle='Qtd. de sucessos pelo total de casos por tribunal')

plot_sucessos_por_tribunal + theme_minimal()

#### SEGUNDO PLOT ####
# Grafico da quantidade de sucessos por orgao julgador por tribunal
casos_por_orgjulg <- df_TRF %>%
  group_by(orgjulg, origem) %>% 
  count()

sucessos_por_orgjulg <- df_TRF %>% 
  group_by(orgjulg, origem) %>% 
  summarise(razao_sucessos_orgjulg = sum(sucessos, na.rm = T)/n()) %>% 
  inner_join(casos_por_orgjulg, by=c('orgjulg', 'origem'), keep=F)

# Resolvendo para o caso em que o orgjulg tem exatamento o mesmo nome para ambos os tribunais
sucessos_por_orgjulg$orgjulg_comb <- with(sucessos_por_orgjulg, interaction(orgjulg, origem))

# Separando a exibição por tribunal de origem
orgjulg_ordem <- sucessos_por_orgjulg$orgjulg_comb[order(sucessos_por_orgjulg$origem, sucessos_por_orgjulg$razao_sucessos_orgjulg)]
sucessos_por_orgjulg$orgjulg <- factor(sucessos_por_orgjulg$orgjulg_comb, levels = orgjulg_ordem)

# Criando o plot
plot_sucessos_por_orgjulg <- ggplot(data = sucessos_por_orgjulg, aes(x=razao_sucessos_orgjulg, y=orgjulg, fill=origem)) +
  geom_col(width=.6, show.legend=T) +
  scale_y_discrete(labels=abbreviate) +
  scale_fill_brewer(palette="Set1") +
  geom_text(aes(label = round(razao_sucessos_orgjulg, 3)),size=3 , hjust = 1.25, vjust=.33,colour = "white") +
  xlab('Razão de Sucessos') +
  ylab('Órgão Julgador') +
  ggtitle('Sucessos por Órgão Julgador', subtitle = 'Qtd. de sucessos pelo total de sucessos por órgão julgador.')
  
plot_sucessos_por_orgjulg + 
  theme_minimal() +
  theme(
    legend.position = c(.8, 0.1)
  )

#### TERCEIRO PLOT ####
# nuvem de palavras para os termos mais comuns nas emendas



# nuvem de palavra ou coisa que o valha para os termos mais comuns nas emendas por sucesso/fracasso
## ver https://r-graphics.org/recipe-bar-graph-dot-plot o exemplo da figura 3.31