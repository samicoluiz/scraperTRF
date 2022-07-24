library("udpipe")
library("lattice")
dl <- udpipe_download_model(language = "portuguese")
udmodel_pt <- udpipe_load_model(dl$file_model)

x <- udpipe_annotate(udmodel_pt, x = df$Ementa[3])
x <- as.tibble(x)
str(x)
table(x$upos)

stats <- subset(x, upos %in% c("VERB"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(stats$key ~ stats$freq)

df$Ementa[3]

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("VERB", "ADV"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = stats, col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


# Identificando padrão sintático
y <- udpipe(df$Decisão[6], 'portuguese')
root_id <- as.numeric(subset(y, dep_rel == "root", select = token_id))
root_id
z <- subset(y, head_token_id == root_id & upos == "ADV" | dep_rel %in% c("obj", "amod") | token_id == root_id , select = token)
z


sucesso <- function(decisao) {
  y <- udpipe(decisao, 'portuguese')
  root_id <- as.numeric(subset(y, dep_rel == "root", select = token_id))
  print(root_id)
  z <- subset(y, head_token_id == root_id & (upos == "ADV" | dep_rel %in% c("obj", "amod")) | token_id == root_id, select = token)
  return(z)
}

averiguar <- sucesso(df$Decisão[6])
print(averiguar)

lapply(jurisprudencia_recente$Decisão, sucesso)

#jurisprudencia_recente$sucesso_bruto <- jurisprudencia_recente %>% lapply(Decisão, sucesso)
### ALGUMAS DECISÕES TEM MAIS DE UM ROOT, IDEIA -> SELECIONAR O ULTIMO ROOT A OCORRER. ###

