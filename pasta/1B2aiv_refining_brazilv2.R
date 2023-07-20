####################################################
#
# FUENTE DE DATOS:  aneel
#
# URL: https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/processamento-de-petroleo-e-producao-de-derivados
#
# download.file(
#  url = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/pppd/processamento-petroleo-m3-1990-2021.csv",
#  destfile = "datos/1B2aiv/1B2aiv_processamento-petroleo-m3-1990-2021.csv"
# )
#
####################################################

if (Sys.info()[["sysname"]] == "Linux") library(colorout)

library(data.table)
library(emep)
library(vein)
library(fem)
data("emep")

class(emep)
setDT(emep)

format(object.size(emep), units = "Mb")

# factor de emision ####
unique(grep(pattern = "refining", x = emep$Sector, value = T))

unique(emep[NFR == "1.B.2.a.iv", unique(Sector)])
unique(emep[NFR == "1.B.2.a.iv", unique(Technology)])

unique(emep[NFR == "1.B.2.a.iv", unique(Technology)])


# emep[NFR == "1.B.2.a.iv" &
# Technology == "Fluid coking units"]

ef <- emep[
  NFR == "1.B.2.a.iv" &
    Technology == "Catalytic Cracking unit regenerators\nPartial burn without CO boiler"
]

ef$Value <- as.numeric(ef$Value)
ef[Pollutant == "BC"]$Value <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$Value / 100
ef[Pollutant == "BC"]$CI_lower <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_lower / 100
ef[Pollutant == "BC"]$CI_upper <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_upper / 100
ef[Pollutant == "BC"]$Unit <- "kg/m3 fresh feed"

unique(ef$Unit)
ef$g_m3 <- ifelse(
  ef$Unit == "kg/m3 fresh feed", ef$Value * 1000,
  ifelse(
    ef$Unit == "g/m3 fresh feed", ef$Value, NA
  )
)

ef <- ef[!is.na(g_m3)]
ef$Unit <- "g/m3 fresh feed"
ef$g_m3 <- units::set_units(ef$g_m3, "g/m^3")


# Refinarias ####


refi <- fread("datos/1B2aiv/1B2aiv_processamento-petroleo-m3-1990-2021.csv",
  encoding = "UTF-8"
)
unique(refi$REFINARIA)
names(refi) <- c("year", "month", "uf", "REFINARIA", "materia_prima", "m3")

unique(refi$month)

refi[, month := fifelse(
  month == "JAN", 1,
  fifelse(
    month == "FEV", 2,
    fifelse(
      month == "MAR", 3,
      fifelse(
        month == "ABR", 4,
        fifelse(
          month == "MAI", 5,
          fifelse(
            month == "JUN", 6,
            fifelse(
              month == "JUL", 7,
              fifelse(
                month == "AGO", 8,
                fifelse(
                  month == "SET", 9,
                  fifelse(
                    month == "OUT", 10,
                    fifelse(
                      month == "NOV", 11,
                      fifelse(
                        month == "DEZ", 12, 0
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)]

unique(refi$month)
class(refi$month)

rf <- data.table(REFINARIA = c(
  "RPBC", "RLAM", "REDUC", "REGAP", "REFAP",
  "LUBNOR", "REPLAN", "REMAN", "RECAP", "REPAR",
  "REVAP", "RPCC", "RNEST", "MANGUINHOS", "RIOGRANDENSE",
  "UNIVEN", "DAX OIL"
))
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

names(refi)
df <- merge(refi,
  rf[, c("REFINARIA", "lat", "lon")],
  by = "REFINARIA",
  all.x = T
)

df$m3 <- units::set_units(df$m3, m^3)

df[year == 2011, sum(m3, na.rm = T), by = REFINARIA]

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  df$g_m3 <- ef$g_m3[i]
  df$Pollutant <- ef$Pollutant[i]
  df$g <- df$m3 * df$g_m3
  df$NFR <- ef$NFR[i]
  df$Sector <- ef$Sector[i]
  df$Technology <- ef$Technology[i]
  df$Fuel <- ef$Fuel[i]
  df$Abatement <- ef$Abatement[i]
  df
})) -> emi_1

emi_1[year == 2011, sum(g, na.rm = T), by = Pollutant]
# dir.create("inventory") # comentar

saveRDS(emi_1, "inventory/1B2aiv_refinaria.rds")

emi_1[year == 2011,
  units::set_units(sum(g, na.rm = T), Gg),
  by = .(Pollutant, REFINARIA)
] -> xx

writexl::write_xlsx(
  dcast(
    data = xx,
    formula = REFINARIA ~ Pollutant
  ),
  "inventory/refi2011.xlsx"
)
