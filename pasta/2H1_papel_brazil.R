
library(data.table)
library(emep)
library(vein)
library(fem)
library(sf)
library(ana)
library(jsonlite)
data(emep)
setDT(emep)
# library(forecast)

(unique(emep[NFR == "2.H.1"]) -> ef)


pulpa <- data.table(
  Mt = c(11,
         12,
         13, 
         13.3, 14.164, 13.922, 13.977, 15.127, 16.465, 17.370, 18.773, 19.527, 21.085, 19.691, 
         19.691),
  year = 2006:2020
)

pulpa_old <- data.table(
  Mt = rep(11, 16),
  year = 1990:2005
)


pulpa <- rbind(pulpa_old, pulpa)

setorderv(pulpa, cols = "year", order = -1)

dp <- data.table(
  Mt = rep(pulpa$Mt, each = 12)/12,
  year = rep(pulpa$year, each = 12),
  month = 1:12
)

dp$Mt <- units::set_units(dp$Mt, Mt)  
dp$Mg <- units::set_units(dp$Mt, Mg)  

# Cemento ####

(unique(emep[NFR == "2.H.1" &
               Technology == "Paper pulp (Kraft process)"]) -> ef)

ef$Value <- as.numeric(ef$Value)
ef[Pollutant == "BC"]$Value <- ef[Pollutant == "PM2.5"]$Value*ef[Pollutant == "BC"]$Value/100*1000
ef[Pollutant == "BC"]$CI_lower <- ef[Pollutant == "PM2.5"]$Value*ef[Pollutant == "BC"]$CI_lower/100
ef[Pollutant == "BC"]$CI_upper <- ef[Pollutant == "PM2.5"]$Value*ef[Pollutant == "BC"]$CI_upper/100
ef[Pollutant == "BC"]$Unit <- "g/Mg"

ef$gMg <- ifelse(
  ef$Unit == "kg/Mg air dried pulp", ef$Value*1000,
                 ifelse(
                   ef$Unit == "g/Mg", ef$Value,
                   NA))

ef$Unit <- "g/Mg"

ef$gMg <- units::set_units(ef$gMg, "g/Mg")

head(ef$gMg[1]*dp$Mg[1])

rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
  dp$gMg <- ef$gMg[i]
  dp$Pollutant <- ef$Pollutant[i]
  dp$g <- units::set_units(dp$Mg*dp$gMg, g)
  dp
})) -> emi_1 

# espacializando data con 

ll_pupl <- readxl::read_excel("ANA/totai_anuais_fontes_fixas-1.xlsx", "CELULOSE")
names(ll_pupl)[1] <- "industria"
ll_pupl <- ll_pupl[!is.na(ll_pupl$industria), c("industria", "Latitude", "Longitude")]

ll_pupl$PER_TOT <- 1/nrow(ll_pupl)

setDT(ll_pupl)

rbindlist(pbapply::pblapply(1:nrow(emi_1), function(i) {
  
  for(j in 1:ncol(emi_1)) {
    ll_pupl[[names(emi_1)[j]]] <- emi_1[[j]][i]
  }
  ll_pupl$emi_g <- ll_pupl$PER_TOT*ll_pupl$g
  ll_pupl
})) -> ll_pupl_emi_1


ll_pupl_emi_1$NFR <- unique(ef$NFR)
ll_pupl_emi_1$Sector <- unique(ef$Sector)
ll_pupl_emi_1$Type <- unique(ef$Type)
ll_pupl_emi_1$Technology <- unique(ef$Technology)
ll_pupl_emi_1$Fuel <- unique(ef$Fuel)
ll_pupl_emi_1$Abatement <- unique(ef$Abatement)


saveRDS(ll_pupl_emi_1, "inventory/2H1_pulpa.rds")

rm(ll_pupl_emi_1)
gc()

