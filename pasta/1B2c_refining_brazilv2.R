####################################################
#
# FUENTE DE DATOS:  BRASIL
#
# URL: https://dados.gov.br/is/dataset/52d7c0c0-b086-4860-84af-ac55de1a9a86/resource/1526168f-0eaa-4107-8df6-f0f2ebb7fa81
#
# download.file(
#  url = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/ppgn-el/producao-petroleo-m3-1997-2021.csv",
#  destfile = "datos/1B2c/1B2c_processamento-petroleo-m3-1990-2021.csv"
# )

# URL: https://dados.gov.br/dataset/a-producao-de-petroleo-e-gas-natural-por-estado-e-localizacao
#
# download.file(
#  url = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/ppgn-el/producao-lgn-m3-1997-2021.csv",
#  destfile = "datos/1B2c/1B2c_producao-lgn-m3-1997-2021.csv"
# )

####################################################

if (Sys.info()[["sysname"]] == "Linux") library(colorout)

library(data.table)
library(emep)
library(vein)
library(fem)
data("emep")

class(emep)

format(object.size(emep), units = "Mb")

# factor de emision ####
unique(grep(pattern = "refining", x = emep$Sector, value = T))

unique(emep[NFR == "1.B.2.c" &
       Table == "Table_3-4"])


emep[
       NFR == "1.A.1.b",
       # Technology == "Reciprocating Engines (compression injection)",
       unique(Fuel)
]

emep[NFR == "1.A.1.b" &
       Pollutant == "CO"]


emep[NFR == "1.B.2.c" &
       Table == "Table_3-4"] -> ef

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




# Refinarias ####

# https://petrobras.com.br/pt/nossas-atividades/principais-operacoes/refinarias/
refi <- fread("datos/REFI_1A1b.csv", encoding = "UTF-8")
meses <- c(
       "JAN", "FEV", "MAR", "ABR", "MAI",
       "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"
)
refis <- melt(refi[
       ANO %in% 2000:2020,
       c(
              "ANO",
              "REFINARIA",
              "JAN", "FEV", "MAR", "ABR", "MAI",
              "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"
       )
],
measure.vars = meses
)

rf <- refis[, sum(value, na.rm = T), by = REFINARIA]
rf$PER_TOT <- rf$V1 / sum(rf$V1)
rf$endereco <- c(
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
rf$lat <- c(
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


rf$lon <- c(
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


# petroleo ####
# https://dados.gov.br/dataset/a-producao-de-petroleo-e-gas-natural-por-estado-e-localizacao
df <- fread("datos/1B2c/1B2c_processamento-petroleo-m3-1990-2021.csv", dec = ",")
names(df) <- c("year", "month", "big_region", "uf", "product", "local", "m3")
unique(df$product)

df[
       year == 2011,
       sum(m3)
]


# poder calorifico
# https://www.gov.br/anp/pt-br/centrais-de-conteudo/publicacoes/anuario-estatistico/arquivos-anuario-estatistico-2020/anuario-2020-fatores-conversao.pdf
# petroleo 10.190 kcal/kg
# densidade = 0.8476 t/m3
df$m3 <- units::set_units(df$m3, m^3)
df$t <- df$m3 * units::set_units(0.8476, t / m^3)
df$kg <- units::set_units(df$t, "kg")
df$kcal <- df$kg * units::set_units(10.190, kcal / kg)
df$GJ <- units::set_units(df$kcal, "GJ")

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
       df$gGJ <- ef$gGJ[i]
       df$Pollutant <- ef$Pollutant[i]
       df$g <- df$GJ * df$gGJ
       df$fuel <- "gas oil"
       df
})) -> emi_1

# gas natural ####
# https://dados.gov.br/dataset/a-producao-de-petroleo-e-gas-natural-por-estado-e-localizacao
df <- fread("datos/1B2c/1B2c_producao-lgn-m3-1997-2021.csv", dec = ",")
names(df) <- c("year", "month", "big_region", "uf", "product", "m3")
unique(df$product)

df[
       year == 2000 &
              month == "JAN",
       sum(m3)
]

# poder calorifico
# https://www.gov.br/anp/pt-br/centrais-de-conteudo/publicacoes/anuario-estatistico/arquivos-anuario-estatistico-2020/anuario-2020-fatores-conversao.pdf
# gas natural  8.800 kcal/kg
# densidade = 4.685 t/m3
df$m3 <- units::set_units(df$m3, m^3)
df$t <- df$m3 * units::set_units(0.00074, t / m^3)
df$kg <- units::set_units(df$t, kg)
df$kcal <- df$kg * units::set_units(8.8, kcal / kg)
df$GJ <- units::set_units(df$kcal, "GJ")

emep[
       NFR == "1.A.1.b",
       # Technology == "Reciprocating Engines (compression injection)",
       unique(Fuel)
]

emep[NFR == "1.A.1.b" &
       Pollutant == "CO"]


emep[NFR == "1.A.1.b" &
       Technology == "4-stroke lean burn gas engines" &
       Fuel == "Natural Gas"] -> ef

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
       df$gGJ <- ef$gGJ[i]
       df$Pollutant <- ef$Pollutant[i]
       df$g <- df$GJ * df$gGJ
       df$fuel <- "gas natural"
       df
})) -> emi_2

emi_1$local <- NULL
emi <- rbind(emi_1, emi_2)

emi[Pollutant == "CO" &
       year == 2011,
sum(g) / 1000000,
by = fuel
]

emi[year %in% 1997:2020,
       sum(g),
       by = .(year, month, Pollutant)
] -> emi_3


rf

rbindlist(pbapply::pblapply(1:nrow(emi_3), function(i) {
       rf$year <- emi_3$year[i]
       rf$month <- emi_3$month[i]
       rf$Pollutant <- emi_3$Pollutant[i]
       rf$emi_g <- rf$PER_TOT * emi_3$V1[i]
       rf
})) -> rf2

saveRDS(rf2, "inventory/1A1b_refinaria.rds") # agregadas por combustible

rm(rf2)