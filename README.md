# scraperTRF1
Esse código faz o scraping de jurisprudência através da ferramenta de busca no site dos TRFs. Após o scraping, agrega todas as informações em formato tabular.

## Uso
Insere-se um termo de busca e o algoritmo retornará _todas_ as ocorrêncais do termo retornadas pela ferramenta de busca do TRF1 agregadas em formato tabular.

### Arquivos adicionais
`df_main.csv` é um exemplo do resultado final do scraping sem tratamento adicional
`df.csv` é uma versão de `df_main.csv` em que as variáveis de data e número do processo passaram por correção de tipo e limpeza adicional.

Ps.: O projeto ainda está em forma bruta, concentrando todo o código no script `Scraper.R`, e evidentemente necessita de modularização, o que será feito conforme o projeto for refinado.
Ps.2: O site do TRF1 usa CAPTCHAs, por conta disso, no formato atual, o algoritmo funciona bem contanto que o CAPTCHA apresentado pelo site seja do tipo "marcação de caixa". Os CAPTCHAs de reconhecimento de imagem impedem o funcionamento normal do crawler.
