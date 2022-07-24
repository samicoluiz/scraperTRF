library("udpipe")
library("lattice")
#dl <- udpipe_download_model(language = "portuguese")
udmodel_pt <- udpipe_load_model(dl$file_model)


# Identificando padrão sintático
# Limpando os textos de quebras de linha mal colocadas.
jurisprudencia_recente$Decisão <- lapply(jurisprudencia_recente$Decisão, str_replace_all("(?<![.?!])\n", ""))

sucesso <- function(decisao) {
  tryCatch(
    {
      y <- udpipe(tolower(decisao), 'portuguese')
      root_id <- as.numeric(subset(y, dep_rel == "root", select = token_id))
      print(root_id)
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
