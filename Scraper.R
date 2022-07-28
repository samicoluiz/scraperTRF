library('RSelenium')
library("rvest")
library('tidyverse')

# Função para organizar as colunas das tabelas
tratar <- function(df) {
  index <- seq_len(nrow(df)) %% 2
  valores <- df[index==0,] %>% rename(X2 = X1)
  chaves <- df[index==1,]
  df_tratada <- tibble(chaves, valores) %>% pivot_wider(names_from = X1, values_from = X2)
  return(df_tratada)
}

# Criando o webdriver
rs_driver_object <- rsDriver(
  browser = 'firefox',
  verbose = F,
  port = 4444L
)

remDr <- rs_driver_object$client
remDr$open()
remDr$navigate('https://www2.cjf.jus.br/jurisprudencia/trf1/index.xhtml')

# Resolvendo o CAPTCHA
Sys.sleep(5)
captcha <- remDr$findElement(using='xpath', '//iframe[@title="reCAPTCHA"]')
remDr$switchToFrame(captcha)
captcha$click()
remDr$switchToFrame(NULL)

# Fazendo a pesquisa do tema
Sys.sleep(5)
#busca <- readline(prompt='Digite o termo de busca: ')
searchField <- remDr$findElement(using = 'id', 'formulario:textoLivre')
searchField$clickElement()
searchField$sendKeysToElement(list("direito moral previdenciário", key="enter"))
Sys.sleep(5)

# Localizando as tabelas no html da página
tabelas <- remDr$findElements(using = 'xpath', '//table[@class="table_resultado"]')

# Criando a lista agregadora
df_main <- tibble()

# Iniciando o loop pelas páginas de pesquisa (caso haja mais de uma)
condicao = TRUE

# Enquanto o botão de 'next' estiver ativo, o loop vai recuperar as tabelas em formato html
# transformar as tabelas em um tabela compatível com o R, separar as tabelas de interesse,
# organizar as linhas e colunas com a função tratar(), e apensar o resultado de cada página
# na lista agregadora
while (condicao == TRUE) {
  tabelas_prep <- lapply(tabelas, function(x) {x$getPageSource()})
  tabelas_html <- lapply(tabelas_prep %>% unlist(), read_html)
  dfs <- lapply(tabelas_html, html_table)
  index3 <- seq_len(length(dfs[[1]][10:length(dfs[[1]])])) %% 2
  dfs <- dfs[[1]][10:length(dfs[[1]])] 
  dfs <- dfs[index3==1]
  dfs <- lapply(dfs, tratar)
  dfs <- bind_rows(dfs)
  df_main <- bind_rows(df_main, dfs)
  
  Sys.sleep(2) # intervalo para o carregamento de cada página
  
  tryCatch(
    {
      botao_next <- remDr$findElement(using = 'xpath', '//a[@class = "ui-paginator-next ui-state-default ui-corner-all"]')
      botao_next$clickElement()
    },
    error = function(e) {
      print('Script finalizado!')
      condicao <<- FALSE
    }
  )
  
  if (condicao == FALSE) {
    break
  }
}

# Descomentar caso queira gerar um csv
# write_csv(df_main,'df_main.csv')

# Encerrando o browser e o servidor
remDr$close()
remDr$quit()
system("taskkill /im java.exe /f")

####################################################
################  FIM DO SCRAPER  ##################
####################################################

# Formatando as colunas de data
df <- mutate(df_main, Data = as.Date(Data, format='%d/%m/%Y'))
df$`Data da publicação` <- gsub('\\s+', '', df$`Data da publicação`) %>% 
  as.Date(format='%d/%m/%Y')
head(df)

# Formatando os nomes das colunas 14 e 17 (continham marcação \n e \t)
colnames(df)[14] <- "Relator para Acórdão"
colnames(df)[17] <- "Referência legislativa"
summary(df)
# Formatando a coluna de número do processo
df$Número <- df$Número %>% strsplit(split = ' ')

df$'Número Formatação' <- lapply(df$Número, function(x) x[1]) %>%
  as.character()

df$Número <- lapply(df$Número, function(x) x[2]) %>%
  as.character()

# Transformando as colunas Classe e Órgão Julgador em factor
df$Classe <- as.factor(df$Classe)
df$`Órgão julgador` <- as.factor(df$`Órgão julgador`)

## Reorganizando as colunas
ordem_colunas <- c(1, 2, 19, 3:18)
df <- df[,ordem_colunas]
head(df)

# Selecionando as colunas de interesse
jurisprudencia <- select(df, -3, -10, -14:-19)
head(jurisprudencia)
write.csv(jurisprudencia, 'juris_data.csv')

####################################################
################  FIM DA LIMPEZA  ##################
####################################################
