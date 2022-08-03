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
#
###########################################################

# Função para organizar as colunas das tabelas
tratarTRF4 <- function(df) {
  # Criando uma coluna cada para Relator, Ementa, e Decisão
  relator <- df[[2]] %>% select("X1", "X2") %>% filter(str_detect(X1, '\\bRelator[a-z]?\\b')) %>% 
    select(-X1)
  
  ementa <- df[[2]] %>% select("X1", "X2") %>% filter(str_detect(X1, '\\bEmenta\\b')) %>% 
    select(-X1)
  
  decisao <- df[[2]] %>% select("X1", "X2") %>% filter(str_detect(X1, '\\bDecisão\\b')) %>% 
    select(-X1)
  
  # Criando um tibble com as demais variáveis: Tipo, Classe, Nº do processo, Data e Órgão Julgador
  df_clean <- df[[2]] %>% select("X2", "X4", "X8", "X12", "X14") %>% filter(str_detect(X2, '\\bAcórdão\\b')) %>%
    add_column(relator = relator$X2, ementa = ementa$X2, decisao = decisao$X2)
  
  colnames(df_clean) <- c('tipo', 'classe', 'nproc', 'data', 'orgjulg', 'relator', 'ementa', 'decisao')
  
  
  # Limpando o texto das variáveis (ESSA ABORDAGEM ESTÁ BAGUNÇANDO OS TIPOS DAS VARIÁVEIS)
  # Código muito feio, buscar como reescrever isso de maneira mais elegante
  df_clean$classe <- df_clean$classe %>% lapply(str_replace_all, 'Classe: ', '') %>% unlist()
  df_clean$nproc <- df_clean$nproc %>% lapply(str_replace_all, 'Processo: ', '') %>% unlist()
  df_clean$data <- df_clean$data %>% lapply(str_replace_all, 'Data da Decisão: ', '') %>% unlist()
  df_clean$orgjulg <- df_clean$orgjulg %>% lapply(str_replace_all, 'Orgão Julgador: ', '') %>% unlist()
  print(df_clean)
  
  return(df_clean)
}  

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
# Aqui usamos o método findElement e não findElements (note o s omitido no final), este último
# usado no scraper do TRF1, que usava uma outra estrutura de tabelas html nos resultados
# do buscador
tabela <- remDr$findElement(using = 'xpath', '//table[@class="tab_resultado"]')

# Iniciando o loop para recorrer todas as páginas da busca
cond = TRUE
dfTRF4 <- tibble()

while (cond == TRUE) {
  tabela_html <- tabela$getPageSource()
  page <- read_html(tabela_html %>% unlist())
  tabela_limpa <- html_table(page) %>%
    tratarTRF4()
  dfTRF4 <- bind_rows(dfTRF4, tabela_limpa)
  
  Sys.sleep(.5)
  
  tryCatch(
    {
      botao_next <- remDr$findElement(using = 'id', 'sbmProximaPagina')
      botao_next$clickElement()
    },
    error = function(e) {
      
      print('Script finalizado!')
      cond <<- FALSE
    }
  )
  
  if (cond == FALSE) {
    break
  }
}

# Encerrando o browser e o servidor
remDr$close()
remDr$quit()
system("taskkill /im java.exe /f")

write.csv(dfTRF4, 'juris_data_trf4.csv')
