## Data IPCC - emission factor (https://www.ipcc-nggip.iges.or.jp/EFDB/find_ef.php?reset=)

rm(list = ls())

## library
{
  library(tidyverse)
  library(XML)
  library(stringi)
}


##path and read
{
  path  <- "C:\\Users\\Tailine\\Documentsinventory"

  setwd(path)

file.exists("ipcc.xls")
format_from_ext("ipcc.xls")
format_from_signature("ipcc.xls")

readr::read_lines("ipcc.xls", n_max = 10)

data <- readHTMLTable("ipcc.xls")


all_df <- do.call("rbind", tbls)
rownames(all_df)<-NULL

colnames(all_df) <-gsub("<!--[if gte mso 9]>", "", colnames(all_df))%>%
  str_replace_all(fixed(" "), "")%>%
  toupper()  %>%  ### Colocar em letra maiuscula
  stri_trans_general("Latin-ASCII")%>%## Remover acentos
  trimws(which = c("both"))%>%
  str_replace_all(fixed("/"), "")

#names(tbls) <- read_html(gsub("<!--[if gte mso 9]>", "", tbls, fixed=TRUE)) %>%
#  xml_find_all(".//name") %>%
#  xml_text()
#tbls<-lapply(tbls, function(x) {colnames(x) <- colnames(tbls[[1]]) ; return(x)})
}








##Para dados

my_pkg_data <- amostra(1000)
usethis::use_data(meu_pkg_data)

2.
usethis::use_data_raw()

usethis::use_data_raw("my_pkg_data")

## teste de memoria
lobstr::mem_used()
#> 60.34 MB
library(nycflights13)
lobstr::mem_used()
#> 62.24 MB
#>
#> tomanho do pacote
#> usethis::use_data(compress =) é “bzip2”, enquanto o padrão para save(compress =) é (efetivamente) “gzip” e “xz” é outra opção válida.
acompanhando o código para gerar seus dados, seria aconselhável atualizar a chamada use_data(compress =) correspondente abaixo de data-raw/ e gerar novamente o .rda de forma limpa
