# HACER!!!

####################################################
#
# FUENTE DE DATOS:  https://sigel.aneel.gov.br/Down/
# URL: "https://www.aneel.gov.br/dados/relatorios?p_p_id=dadosabertos_WAR_dadosabertosportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_resource_id=gerarGeracaoFonteCSV&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=1"
# download.file(
#  url = "https://www.aneel.gov.br/dados/relatorios?p_p_id=dadosabertos_WAR_dadosabertosportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_resource_id=gerarGeracaoFonteCSV&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=1",
#  destfile = "datos/1A1a/1A1a.csv"
# )
# BACKUP: "datos/1A1a.csv"
#
# url <- "https://sigel.aneel.gov.br/arcgis/rest/services/PORTAL/Mapa_Empreendimentos_Gera%C3%A7%C3%A3o/MapServer/4"
# linfo <- ana::layer_info(url)
# (geom_type <- linfo$geometryType)
# esri_features <- get_esri_features(url)
# aneel <- esri_to_sf_geom(esri_features, geom_type = geom_type)
# saveRDS(aneel, "datos/1A1a/aneel.rds")
# BACKUP: "datos/1A1a/aneel.rds"
#
####################################################

if (Sys.info()[["sysname"]] == "Linux") library(colorout)

library(data.table)
library(emep)
library(vein)
library(fem)
library(sf)
library(ana)
data(emep)
# https://sigel.aneel.gov.br/Down/
x <- st_read("datos/1A1a/aneel/output/zipfolder/Usinas_Termelétricas_UTE.shp")
unique(x$COMBUSTIVE)
dim(x)


df3 <- fread("datos/1A1a/1A1a.csv",
  encoding = "Latin-1"
)

df3[anoReferencia == 2019,
  sum(mdaEnergiaDespachadaGWh, na.rm = T),
  by = .(nomFonteGeracao, anoReferencia)
]
# EMEP
# [1] "Gas Oil"                                       "Gaseous Fuels"
# [3] "Natural gas"                                   "Hard Coal"
# [5] "Brown Coal"                                    "Gas oil"
# [7] "Heavy Fuel Oil"                                "Gaseous fuels"
# [9] "Biomass"                                       "Residual Oil"
# [11] "Brown Coal/Lignite"                            "Natural Gas"
# [13] "Coking Coal, Steam Coal & Sub-Bituminous Coal" "Wood and wood waste (clean wood waste)"

df3[, unique(nomFonteGeracao)] -> nm

nm

act <- df3[nomFonteGeracao %in% nm[c(3, 4, 5, 7, 9)]]

# BR -> EEA
# Oleo Diesel -> Gas Oil
# Gas Natural -> Natural Gas
# Carvao -> Hard Coal
# Biomassas -> Biomass
# Residuos Processos Industriais -> Residual Oil

# convert GWh em GJ
# 1Wh = 3600 J

df3$mdaEnergiaDespachadaGJ <- units::set_units(df3$mdaEnergiaDespachadaGWh * 3600, GJ)

# tiene que ser df3 con mdaEnergiaDescpahadaGWh

# esri sf ####
url <- "https://sigel.aneel.gov.br/arcgis/rest/services/PORTAL/Mapa_Empreendimentos_Gera%C3%A7%C3%A3o/MapServer/4"
linfo <- ana::layer_info(url)
(geom_type <- linfo$geometryType)
esri_features <- get_esri_features(url)
aneel <- esri_to_sf_geom(esri_features, geom_type = geom_type)
dim(aneel)
sum(aneel$POT_FISC_KW, na.rm = T)
sum(aneel$POT_KW) / 1000 / 1000

(uc <- unique(aneel$COMBUSTIVEL))

# relacionamos aneel con las categorias nomFonteGeracao
# calculamos% POT_KW de grupos
# Calculoemisione df3 y distribuyo espacialmente con aneel

aneel$nomFonteGeracao <- ifelse(
  aneel$COMBUSTIVEL %in% c(
    "Óleo Diesel",
    "Óleo Combustível",
    "Óleos Vegetais",
    "Etanol"
  ), "Oleo Diesel / Combustivel",
  ifelse(
    aneel$COMBUSTIVEL %in% c(
      "Gás Natural",
      "Gás de Alto Forno",
      "Gás de Refinaria",
      "Biogás"
    ), "Gas Natural",
    ifelse(
      aneel$COMBUSTIVEL %in% c(
        "Carvão Vegetal",
        "Carvão Mineral",
        "Carvão - RU"
      ), "Carvao",
      ifelse(
        aneel$COMBUSTIVEL %in% c(
          "Bagaço de Cana de Açucar",
          "Capim Elefante",
          "Casca de Arroz",
          "Resíduos Florestais",
          "Lenha",
          "Resíduos de Madeira",
          "Licor Negro"
        ), "Biomassas",
        ifelse(
          aneel$COMBUSTIVEL %in% c(
            "Calor de Processo",
            "Outros Energéticos de Petróleo"
          ), "Residuos Processos Industriais",
          "otro"
        )
      )
    )
  )
)
names(aneel)
unique(aneel$nomFonteGeracao)
aneel[aneel$nomFonteGeracao == "otro", ]
# calcular % POT_KW por nomeFonteGeracao
setDT(aneel)
unique(aneel$FASE_USINA)
aneel[, .N, by = FASE_USINA]

aneel <- aneel[FASE_USINA == "Operação"]

aneel[, PER_POT := POT_KW / sum(POT_KW), by = nomFonteGeracao]

sum(aneel$PER_POT)

# ahora vamos a estimar las emisiones con df3


# "Oleo Diesel / Combustivel" ####
emep$Value <- as.numeric(emep$Value)
emep[NFR == "1.A.1.a" & Pollutant == "CO"] -> dd

setorderv(dd, "Value")
dd
# Oleo Diesel -> Gas Oil
emep[NFR == "1.A.1.a" &
  Technology == "Large stationary CI reciprocating engines" &
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

df <- df3[nomFonteGeracao == "Oleo Diesel / Combustivel"]

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  df$gGJ <- ef$gGJ[i]
  df$Pollutant <- ef$Pollutant[i]
  df$g <- df$mdaEnergiaDespachadaGJ * df$gGJ
  df
})) -> emi_1

emi_1[anoReferencia == 2011,
  sum(g, na.rm = T),
  by = Pollutant
]
# espacializando data

aneel_1 <- aneel[nomFonteGeracao == "Oleo Diesel / Combustivel"]
names(aneel_1)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  for (j in 1:ncol(emi_1)) {
    aneel_1[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  aneel_1$emi_g <- aneel_1$PER_POT * aneel_1$g
  aneel_1
})) -> aneel_emi_1

saveRDS(aneel_emi_1, "inventory/1A1a_oleo_diesel.rds")

rm(aneel_emi_1)
gc()



# "Gas Natural ####
# Gas Natural -> Natural Gas
emep[NFR == "1.A.1.a" & Pollutant == "CO"] -> dd

unique(emep[NFR == "1.A.1.a" &
  Table == "Table_3-20" &
  Fuel == "Natural gas"]) -> ef

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

df <- df3[nomFonteGeracao == "Gas Natural"]

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  df$gGJ <- ef$gGJ[i]
  df$Pollutant <- ef$Pollutant[i]
  df$g <- df$mdaEnergiaDespachadaGJ * df$gGJ
  df
})) -> emi_1

# espacializando data

aneel_1 <- aneel[nomFonteGeracao == "Gas Natural"]
names(aneel_1)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  for (j in 1:ncol(emi_1)) {
    aneel_1[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  aneel_1$emi_g <- aneel_1$PER_POT * aneel_1$g
  aneel_1
})) -> aneel_emi_1

saveRDS(aneel_emi_1, "inventory/1A1a_gas_natural.rds")

rm(aneel_emi_1)
gc()


# Carvao -> Hard Coal ####
emep[NFR == "1.A.1.a" & Pollutant == "CO"] -> dd

setorderv(dd, "Value")
dd
unique(emep[NFR == "1.A.1.a" &
  Type == "Tier 2 emission factor" &
  Fuel == "Hard Coal"]) -> ef

ef$Value <- as.numeric(ef$Value)
ef[Pollutant == "BC"]$Value <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$Value / 100
ef[Pollutant == "BC"]$CI_lower <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_lower / 100
ef[Pollutant == "BC"]$CI_upper <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_upper / 100
ef[Pollutant == "BC"]$Unit <- "g/GJ"

ef$gGJ <- ifelse(ef$Unit == "mg/GJ", ef$Value / 1000,
  ifelse(ef$Unit %in% c("ng I-TEQ/GJ", "ng WHO-TEG/GJ"), ef$Value * 1e-9,
    ifelse(ef$Unit %in% c("g/GJ", "% of PM2.5"), ef$Value,
      ifelse(ef$Unit == "µg/GJ", ef$Value * 1e-6,
        NA
      )
    )
  )
)
ef$gGJ <- units::set_units(ef$gGJ, "g/GJ")

df <- df3[nomFonteGeracao == "Carvao"]

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  df$gGJ <- ef$gGJ[i]
  df$Pollutant <- ef$Pollutant[i]
  df$g <- df$mdaEnergiaDespachadaGJ * df$gGJ
  df
})) -> emi_1

# espacializando data

aneel_1 <- aneel[nomFonteGeracao == "Carvao"]
names(aneel_1)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  for (j in 1:ncol(emi_1)) {
    aneel_1[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  aneel_1$emi_g <- aneel_1$PER_POT * aneel_1$g
  aneel_1
})) -> aneel_emi_1

saveRDS(aneel_emi_1, "inventory/1A1a_carvao.rds")

rm(aneel_emi_1)
gc()


# Biomassas -> Biomass ####
emep[NFR == "1.A.1.a" & Pollutant == "CO"] -> dd

setorderv(dd, "Value")
dd

unique(emep[NFR == "1.A.1.a" &
  # Type == "Tier 1 emission factor" &
  Fuel == "Biomass"]) -> ef

ef$Value <- as.numeric(ef$Value)
ef[Pollutant == "BC"]$Value <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$Value / 100
ef[Pollutant == "BC"]$CI_lower <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_lower / 100
ef[Pollutant == "BC"]$CI_upper <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_upper / 100
ef[Pollutant == "BC"]$Unit <- "g/GJ"

ef$gGJ <- ifelse(ef$Unit == "mg/GJ", ef$Value / 1000,
  ifelse(ef$Unit %in% c("ng I-TEQ/GJ", "ng WHO-TEG/GJ"), ef$Value * 1e-9,
    ifelse(ef$Unit %in% c("g/GJ", "% of PM2.5"), ef$Value,
      ifelse(ef$Unit == "µg/GJ", ef$Value * 1e-6,
        NA
      )
    )
  )
)
ef$gGJ <- units::set_units(ef$gGJ, "g/GJ")

df <- df3[nomFonteGeracao == "Biomassas"]

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  df$gGJ <- ef$gGJ[i]
  df$Pollutant <- ef$Pollutant[i]
  df$g <- df$mdaEnergiaDespachadaGJ * df$gGJ
  df
})) -> emi_1

# espacializando data

aneel_1 <- aneel[nomFonteGeracao == "Biomassas"]
names(aneel_1)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  for (j in 1:ncol(emi_1)) {
    aneel_1[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  aneel_1$emi_g <- aneel_1$PER_POT * aneel_1$g
  aneel_1
})) -> aneel_emi_1

saveRDS(aneel_emi_1, "inventory/1A1a_biomassas.rds")

rm(aneel_emi_1)
gc()


# Residuos Processos Industriais -> Residual Oil
dd
unique(emep[NFR == "1.A.1.a" &
  # Type == "Tier 1 emission factor" &
  Fuel == "Residual Oil"]) -> ef

ef$Value <- as.numeric(ef$Value)
ef[Pollutant == "BC"]$Value <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$Value / 100
ef[Pollutant == "BC"]$CI_lower <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_lower / 100
ef[Pollutant == "BC"]$CI_upper <- ef[Pollutant == "PM2.5"]$Value * ef[Pollutant == "BC"]$CI_upper / 100
ef[Pollutant == "BC"]$Unit <- "g/GJ"

ef$gGJ <- ifelse(ef$Unit == "mg/GJ", ef$Value / 1000,
  ifelse(ef$Unit %in% c("ng I-TEQ/GJ", "ng WHO-TEG/GJ"), ef$Value * 1e-9,
    ifelse(ef$Unit %in% c("g/GJ", "% of PM2.5"), ef$Value,
      ifelse(ef$Unit == "µg/GJ", ef$Value * 1e-6,
        NA
      )
    )
  )
)
ef$gGJ <- units::set_units(ef$gGJ, "g/GJ")

df <- df3[nomFonteGeracao == "Residuos Processos Industriais"]

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  df$gGJ <- ef$gGJ[i]
  df$Pollutant <- ef$Pollutant[i]
  df$g <- df$mdaEnergiaDespachadaGJ * df$gGJ
  df
})) -> emi_1

# espacializando data

aneel_1 <- aneel[nomFonteGeracao == "Residuos Processos Industriais"]
names(aneel_1)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  for (j in 1:ncol(emi_1)) {
    aneel_1[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  aneel_1$emi_g <- aneel_1$PER_POT * aneel_1$g
  aneel_1
})) -> aneel_emi_1

saveRDS(aneel_emi_1, "inventory/1A1a_residuos_processos_industriais.rds")

rm(aneel_emi_1)
gc()

# dt[, set_units(sum(PM10),"Gg"), by = ANO][, plot(y = V1, x = ANO)]