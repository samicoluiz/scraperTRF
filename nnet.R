library(tensorflow)
library(keras)
library(tfdatasets)
# INSTALAÇÃO E CHECAGEM DA ABILITAÇÃO DO TENSORFLOW PARA USO COM GPU LOCAL
# install_tensorflow(gpu = TRUE)
# tf$config$list_physical_devices("GPU")

textos <- dfTRF4 %>% select(c(orgjulg, relator, decisao, sucessos)) %>%
  drop_na() %>%
  mutate(sucessosencode = as.numeric(sucessos)) %>% 
  select(-sucessos) %>% 
  select(sucessosencode, everything())
textos$id <- 1:nrow(textos)

samples <- textos$decisao

texto_train <- textos %>% sample_frac(.8)
texto_test <- anti_join(textos, texto_train, by='id')
x_train <- texto_train %>% select(c(-1, -5))
y_train <- texto_train$sucessosencode
x_test <- texto_test %>% select(c(-1, -5))
y_test <- texto_test$sucessosencode

tokenizer <-  text_tokenizer(num_words = 1000) %>%
  fit_text_tokenizer(texto_train$decisao)
sequences <- texts_to_sequences(tokenizer, texto_train$decisao)
one_hot_results <- texts_to_matrix(tokenizer, texto_train$decisao, mode="binary")
word_index <- tokenizer$word_index

cat('Foram encontrados', length(word_index), 'tokens únicos.\n')

data <- pad_sequences(sequences, maxlen=100)

# WORD EMBED
glove_dir = 'glove_s50.txt'
lines <- readLines('glove_s50.txt')
embeddings_index <- new.env(hash=TRUE, parent = emptyenv())
for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}

cat("Found", length(embeddings_index), "word vectors.\n")
