library('RSelenium')
library("rvest")
library('tidyverse')

# Criando o webdriver
rs_driver_object <- rsDriver(
  browser = 'firefox',
  verbose = F,
  port = 4444L
)

remDr <- rs_driver_object$client
remDr$open()
remDr$navigate('https://jurisprudencia.trf4.jus.br/pesquisa/pesquisa.php?tipo=%201')

# Fazendo a pesquisa do tema
Sys.sleep(5)
#busca <- readline(prompt='Digite o termo de busca: ')
optEmenta <- remDr$findElement(using = 'id', 'optEmenta')
optEmenta$clickElement()
decMono <- remDr$findElement(using = 'id', 'chkDecMono')
decMono$clickElement()
searchField <- remDr$findElement(using = 'id', 'textoPesqLivre')
searchField$clickElement()
searchField$sendKeysToElement(list("dano moral previdenciário", key="enter"))
Sys.sleep(5)
parcial <- remDr$findElement(using = 'id', 'parcial')
parcial$clickElement()

# Localizando as tabelas no html da página
#
# Aqui usamos o método findElement e não findElements (note o s no final), este último
# usado no scraper do TRF1, que usava uma outra estrutura de tabelas html nos resultados
# do buscador
tabela <- remDr$findElement(using = 'xpath', '//table[@class="tab_resultado"]')
tabela_html <- tabela$getPageSource()

# Extraindo os dados brutos
page <- read_html(tabela_html %>% unlist())

# Criando a lista agregadora
df_main <- html_table(page)

# Data wrangling

##### Anotações para entender a tabela bruta extraida #####
#
# 1.  As colunas se repetem a cada 9 linhas do tibble df_main[[2]] e.g. o Relator do caso n
# está na linha x, o relator do caso n+1 está na linha x+9.
#
# 2.  O padrão de informação começa na linha 2
#
# 3.  A informação que nos é útil pode ser totalmente encontrada no tibble df_main[[2]]
#
# 3.1 As informações são: 
#   Número do Processo,
#   Tipo da decisão,
#   Classe da ação, 
#   Relator, 
#   Origem?,
#   Data,
#   Ementa,
#   Decisão

















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
