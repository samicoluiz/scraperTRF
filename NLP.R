

# Obtendo apenas os casos únicos dos últimos 2 anos
jurisprudencia_recente <- unique(jurisprudencia) %>% filter(Data >= '2021-01-01')

dl <- udpipe_download_model(language = "portuguese")
udmodel_pt <- udpipe_load_model(dl$file_model)


# Identificando padrão sintático
# Limpando os textos de quebras de linha mal colocadas.
jurisprudencia_recente$Decisão <- lapply(jurisprudencia_recente$Decisão, str_replace("","(?<![.?!])\n"))

sucesso <- function(decisao) {
  tryCatch(
    {
      y <- udpipe(tolower(decisao), 'portuguese')
      root_id <- as.numeric(subset(y, dep_rel == "root", select = token_id))
      z <- subset(y, head_token_id == root_id & (upos == "ADV" | dep_rel %in% c("obj", "amod")) | token_id == root_id, select = token)
      return(z[[1]])
    },
    error = function(cond) {NA}
  )
  
}

# Algumas decisões tem mais de um root, isso gera um erro e instruimos o algoritmo a
# preencher essas entradas como NA na coluna de sucessos.

jurisprudencia_recente$Sucessos_bruto <- lapply(jurisprudencia_recente$Decisão, sucesso) %>% 
  lapply(paste0, collapse=" ") %>% 
  unlist()

# Quando a coluna foi criada, os valores foram transformados em factor, e os NA viraram "NA"
# A linha abaixo retorna o valor esperado
jurisprudencia_recente$Sucessos_bruto[jurisprudencia_recente$Sucessos_bruto == "NA"] <- NA

# Criando a coluna com os valores lógicos
jurisprudencia_recente$Sucessos <- ifelse(grepl("não|negou", jurisprudencia_recente$Sucessos_bruto), FALSE, TRUE)
jurisprudencia_recente$Sucessos <- ifelse(is.na(jurisprudencia_recente$Sucessos_bruto), NA, jurisprudencia_recente$Sucessos)

sucessos <- summary(jurisprudencia_recente$Sucessos)['TRUE']
fracassos <- summary(jurisprudencia_recente$Sucessos)['FALSE']

print(sucessos)
print(fracassos)

# Criando o dataframe para realizar as análises
dano_moral_previdenciario <- select(jurisprudencia_recente, Tipo, Número, Classe, "Órgão julgador", Data, Ementa, Decisão, Sucessos) %>% 
  filter(Classe == "APELAÇÃO CIVEL (AC)") %>% 
  group_by(dano_moral_previdenciario$'Órgão julgador') %>%
  summarize(exitos = sum(Sucessos, na.rm=T), ocorr=n()) %>% 
  mutate(prop_exito = exitos/sum(exitos)) %>% 
  mutate(rate_exito = exitos/ocorr) %>%
  arrange(desc(rate_exito)) %>% 
  names(dano_moral_previdenciario)[1] <- 'org_julgador'

# Melhorar os plots e gerar outras visualizações
grafico <- ggplot(data = dano_moral_previdenciario, aes(reorder(org_julgador, rate_exito), rate_exito)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(labels = abbreviate) +
  xlab(NULL) +
  ylab("Taxa de êxito")



