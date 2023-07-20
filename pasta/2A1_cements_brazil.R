####################################################
#
# FUENTE DE DATOS:  http://snic.org.br/numeros-relatorio-anual.php
# URL: "http://snic.org.br/numeros-relatorio-anual.php"
# DATOS: "ANA/totai_anuais_fontes_fixas-1.xlsx"
#
####################################################

if (Sys.info()[["sysname"]] == "Linux") library(colorout)
library(data.table)
library(emep)
library(vein)
library(fem)
library(sf)
library(ana)
library(jsonlite)
data(emep)
setDT(emep)
library(forecast)


emep[grep("Cement", x = emep$Sector)]
(emep[grep("Cement", x = emep$Technology)] -> ef)

tec_2019 <- data.table(
  MJ_tclinker = c(4200, 3400, 3500, 3490, 3470, 3375, 3220),
  year = c(1990, 2014, 2018, 2020, 2030, 2040, 2050),
  source = rep("Relatorio Anual 2019", 7),
  url = rep("http://snic.org.br/numeros-relatorio-anual.php", 7)
)

perc_clinker <- data.table(
  per = c(0.8, 0.74, 0.68, 0.69, 0.68),
  year = c(1990, 2000, 2010, 2019, 2020)
)

x = perc_clinker$year
y = perc_clinker$per
f <- approx(x, y)
as.data.table(f) -> per_clin
per_clin$x <- as.integer(round(per_clin$x))
names(per_clin) <- c("year", "perc_clin")


per_clin <- per_clin[!duplicated(year)]


a <- "cemento/tabula-2014_2020.tsv"

df <- fread(a, h = T)

dtt <- rev(unlist(df[, -1]))

ts_dt <- ts(dtt, frequency = 12) 

mymodel <- auto.arima(dtt)

plot(mymodel)

myforecast <- forecast(ts_dt, level=c(95), h=24*12)

predict(myforecast) -> xx


dtf <- data.table(
  t_cemento = c(dtt, xx$mean),
  year = rep(2020:1990, each = 12),
  month = 12:1)

setorderv(dtf, cols = c("year", "month"), order = c(-1, 1))

class(dtf$year)
class(per_clin$year)

dtf <- merge(dtf, per_clin, by = "year", all.x = T)

dtf$tec <- dtf$t_cemento*dtf$perc_clin

# Cemento ####

(emep[grep("Cement", x = emep$Technology)] -> ef)

ef$Value <- as.numeric(ef$Value)

ef$gtec <- ifelse(ef$Unit == "mg/GJ", ef$Value/1000,
                 ifelse(ef$Unit == "ng I-TEQ/te clinker", ef$Value*1e-9, 
                        ifelse(ef$Unit %in% c("g/te clinker", "% of PM2.5"), ef$Value,
                               ifelse(ef$Unit == "µg/te clinker", ef$Value*1e-6,
                                      NA))))

ef$gtec <- units::set_units(ef$gtec, "g")

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  dtf$gtec <- ef$gtec[i]
  dtf$Pollutant <- ef$Pollutant[i]
  dtf$g <- dtf$tec*dtf$gtec
  dtf
})) -> emi_1 

# espacializando data con 

ll_cemento <- readxl::read_excel("ANA/totai_anuais_fontes_fixas-1.xlsx", "CIMENTO")
names(ll_cemento)[1] <- "industria"
ll_cemento <- ll_cemento[!is.na(ll_cemento$industria), c("industria", "Latitude", "Longitude")]

ll_cemento$PER_TOT <- 1/nrow(ll_cemento)

setDT(ll_cemento)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  
  for(j in 1:ncol(emi_1)) {
    ll_cemento[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  ll_cemento$emi_g <- ll_cemento$PER_TOT*ll_cemento$g
  ll_cemento
})) -> ll_cemento_emi_1

ll_cemento_emi_1$NFR <- unique(ef$NFR)
ll_cemento_emi_1$Sector <- unique(ef$Sector)
ll_cemento_emi_1$Type <- unique(ef$Type)
ll_cemento_emi_1$Technology <- unique(ef$Technology)
ll_cemento_emi_1$Fuel <- unique(ef$Fuel)
ll_cemento_emi_1$Abatement <- unique(ef$Abatement)


saveRDS(ll_cemento_emi_1, "inventory/1A2gviii_cemento.rds")

rm(ll_cemento_emi_1)
gc()

