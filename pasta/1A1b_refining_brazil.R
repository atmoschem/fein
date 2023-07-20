####################################################
#
# FUENTE DE DATOS:  https://sigel.aneel.gov.br/Down/
# URL: "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-processamento-de-petroleo-e-producao-de-derivados/processamento-petroleo-b.xls"
# download.file(
#        url = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-processamento-de-petroleo-e-producao-de-derivados/processamento-petroleo-b.xls",
#        destfile = "datos/1A1b/REFI_1A1b_barris.xls"
# )
# BACKUP: "datos/1A1b/REFI_1A1b_barris.xls"
#
####################################################

##################
#
# install library_github
#
# remotes::install_github("ibarraespinosa/emep")
# remotes::install_github("ibarraespinosa/fem")
# remotes::install_github("atmoschem/vein")
#
########################

if (Sys.info()[["sysname"]] == "Linux") library(colorout)
library(data.table)
library(emep)
library(vein)
library(fem)
data("emep")

class(emep)

format(object.size(emep), units = "Mb")


dir.create("inventory")
setorderv(emep,
       cols = c("NFR", "Sector", "Table", "Type", "Technology", "Fuel", "Pollutant"),
       order = 1
)


# writexl::write_xlsx(unique(emep), "inventory/emep.xlsx")

unique(emep$NFR)
nfr <- unique(emep[, c("NFR", "Sector", "Type", "Technology", "Fuel")])

unfr <- unique(emep$NFR)


# crear llave en emep
data(brazil)
setDT(brazil)

brazil[, unique(Sector)]
names(brazil)

# Refinarias ####

# Tabela 2.27 – Evolução da capacidade de refino, segundo refinarias – 2011-2020

dir.create("datos")


readxl::excel_sheets("datos/1A1b/REFI_1A1b_barris.xls")

df <- readxl::read_xls("datos/1A1b/REFI_1A1b_barris.xls", "DPCache_b")
df <- as.data.table(as.data.frame(df))

names(df)[4] <- "MATERIA_PRIMA"

# https://petrobras.com.br/pt/nossas-atividades/principais-operacoes/refinarias/
dfr <- data.table(REFINARIA = unique(df$REFINARIA))
dfr$endereco <- c(
       "Av. 9 de abril, 777 - Jardim das Indústrias, Cubatão - SP CEP: 11505-000", # RPBC
       "Rodovia BA 523, KM 4 – Mataripe, São Francisco do Conde - B, CEP: 49170-000", # RLAM
       "Rodovia Washington Luiz, km 113,7, Campos Elíseos – Duque de Caxias - RJ", # REDUC
       "Av. Refinaria Gabriel Passos, 690, Distrito Industrial Paulo Camilo Sul, Betim - MG, CEP: 32669-205", # REGAP
       "Avenida Getúlio Vargas, 11001 - Bairro Brigadeira, Canoas - RS - Brasil, CEP: 92420-221", # REFAP
       "Refinaria Lubrificantes e Derivados do Nordeste - Lubnor, Av. Leite Barbosa, s/nº - Mucuripe, Fortaleza - Ceará, CEP: 60180-420", # LUBNOR
       "Rodovia SP 332 - Km. 130, Bonfim - Paulínia – SP, CEP: 13147-900", # REPLAN
       "Rua Rio Quixito, 1, Vila Buriti - Distrito Industrial, Manaus - AM, CEP: 69072-070", # REMAN
       "Av. Alberto Soares Sampaio, 2122-A, Capuava, Mauá - SP", # RECAP
       "Rodovia do Xisto, BR 476, km 16, Araucária - PR, CEP: 83707-440", # REPAR
       "Rodovia Presidente Dutra, KM 143, S/N, Bairro Jardim Diamante - São José dos Campos – SP, CEP: 12223-900", # REVAP
       "Rodovia RN 221, KM 25,  Guamaré - RN, CEP: 59.598-000", # RPCC
       "Rodovia PE 60, Km 10, Ipojuca - PE", # RNEST
       "Av. Brasil, 3141 - Manguinhos, Rio de Janeiro - RJ, 20930-041", # MANGUINHOS
       "R. Eng. Heitor Amaro Barcelos, 551 - Parque Res. Coelho, Rio Grande - RS, 96202-900", # RIOGRANDENSE
       "Est Municipal Iva010, 250 - Jardim Alegre, Itupeva - SP, 13295-000", # UNIVEN
       "R. Oxigênio, 245 - Polo Petroquímico, Camaçari - BA, 42810-270" # DAXOIL
)
dfr$lat <- c(
       -23.87161,
       -12.712060737002062,
       -22.71764003907247,
       -19.97174079241169,
       -29.873688095958087,
       -3.716060428476888,
       -22.729250349248847,
       -3.146670030718162,
       -23.643231480041784,
       -25.572329904477897,
       -23.195051564368914,
       -5.132577270438568,
       -8.382798259318163,
       -22.8870404808916,
       -32.043112612729615,
       -23.142591980022562,
       -12.656539001728472
)


dfr$lon <- c(
       -46.43375,
       -38.571863408401015,
       -43.25713764711672,
       -44.09642824149662,
       -51.16233712039244,
       -38.47207884250638,
       -47.135850302841405,
       -59.952762772753914,
       -46.478104923306866,
       -49.366937530475724,
       -45.828571049181846,
       -36.38610320838552,
       -35.02593016198562,
       -43.23696586514691,
       -52.09109931003124,
       -47.02219076393593,
       -38.3103979650492
)


###
# dfr$latlon

dff <- melt(data = df, id.vars = c("ANO", "ESTADO", "REFINARIA", "MATERIA_PRIMA", "UNIDADE"))

dff <- merge(dff, dfr, by = "REFINARIA", all.x = T)

dff[variable != "TOTAL",
       sum(value, na.rm = T),
       by = .(REFINARIA, variable, ANO, lat, lon)
] -> dt

names(dt)[2] <- "mes"
names(dt)[6] <- "barris"
# 1 barrel of crude oil	= 6.193 gigajoules
# https://www150.statcan.gc.ca/n1/pub/57-601-x/00105/4173282-eng.htm
dt$GJ <- 6.193 * dt$barris
dt$GJ <- units::set_units(dt$GJ, "GJ")

emep[
       NFR == "1.A.1.b",
       # Technology == "Reciprocating Engines (compression injection)",
       unique(Fuel)
]


emep[
       NFR == "1.B.2.a.iv",
       # Technology == "Reciprocating Engines (compression injection)",
       unique(Fuel)
]


emep[NFR == "1.A.1.b" &
       Pollutant == "CO"]


emep[NFR == "1.A.1.b" &
       Technology == "Reciprocating Engines (compression injection)" &
       Fuel == "Gas Oil"] -> ef

ef$Value <- as.numeric(ef$Value)
ef[Pollutant == "BC"]$Value <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$Value / 100
ef[Pollutant == "BC"]$CI_lower <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_lower / 100
ef[Pollutant == "BC"]$CI_upper <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_upper / 100
ef[Pollutant == "BC"]$Unit <- "g/GJ"

ef$gGJ <- ifelse(ef$Unit == "mg/GJ", ef$Value / 1000,
       ifelse(ef$Unit == "ng I-TEQ/GJ", ef$Value * 1e-9,
              ifelse(ef$Unit %in% c("g/GJ", "% of PM2.5"), ef$Value,
                     ifelse(ef$Unit == "µg/GJ", ef$Value * 1e-6,
                            NA
                     )
              )
       )
)

ef$gGJ <- units::set_units(ef$gGJ, "g/GJ")


rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
       dt$gGJ <- ef$gGJ[i]
       dt$Pollutant <- ef$Pollutant[i]
       dt$g <- dt$GJ * dt$gGJ
       dt
})) -> emi_1


saveRDS(emi_1, "inventory/1A1b_refinaria.rds")

rm(emi_1)
rm(list = ls())
gc()