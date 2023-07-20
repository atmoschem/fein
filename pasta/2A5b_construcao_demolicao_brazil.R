
####################################################
#
# FUENTE DE DATOS:  https://geo.anm.gov.br/arcgis/rest/services/SIGMINE/dados_anm/MapServer
# url <- "https://geo.anm.gov.br/arcgis/rest/services/SIGMINE/dados_anm/MapServer/0"
# linfo <- ana::layer_info(url)
# (geom_type <- linfo$geometryType)
# esri_features <- get_esri_features(url)
# mine <- esri_to_sf_geom(esri_features, geom_type = geom_type)
# saveRDS(mine, "datos/2A5a/minas.rds")
# BACKUP: "datos/2A5a/minas.rds"
#
####################################################
####################################################

if (Sys.info()[["sysname"]] == "Linux") library(colorout)
library(data.table)
library(emep)
library(vein)
library(fem)
library(sf)
data(emep)
setDT(emep)
unique(emep$NFR)

emep[NFR == "2.A.5.b", unique(Sector)]

emep[NFR == "2.A.5.b", unique(Type)]

# nao tenho dados espaciais

# 
# 
# emep[NFR == "2.A.5.b" &
#        Type == "Tier 1 emission factor"]
# 
# names(emep)
# emep[NFR == "2.A.5.a" &
#        Type == "Tier 2 emission factor", 
#      unique(Type)]
# 
# 
# mina <- readRDS("datos/2A5a/minas.rds")
# 
# unique(mina$FASE)
# 
# mina <- mina[mina$FASE == "LICENCIAMENTO", ]
# unique(mina$SUBS)
# mina$SUBS <- iconv(x = mina$SUBS, from = "UTF-8", to = "ASCII//TRANSLIT")
# 
# plot(mina[mina$SUBS == "MINERIO DE OURO", ]$geoms)

# Nao consegui


# 
# emep[Sector == "Glass production", unique(Type)]
# 
# emep[Sector == "Glass production" &
#        Type == "Tier 1 emission factor"]
# 
# emep[Sector == "Glass production" &
#        Type == "Tier 1 emission factor"] -> ef
# 
# dates <- seq.Date(as.Date("2006-01-01"), 
#                   as.Date("2021-12-31"), 
#                   by = "month")
# 
# numberOfDays <- function(date) {
#   m <- format(date, format="%m")
#   
#   while (format(date, format="%m") == m) {
#     date <- date + 1
#   }
#   return(as.integer(format(date - 1, format="%d")))
# }
# 
# vidro <- data.frame(dates = dates,
#                        days = unlist(lapply(dates, numberOfDays)))
# 
# 
# vidro <- as.data.frame(dias)
# 
# # https://www.google.com.br/maps/place/Cebrace/@-26.634989,-48.704182,17z/data=!4m12!1m6!3m5!1s0x94d92b0cbad3db97:0x2572c7ac416b3e51!2sCebrace!8m2!3d-26.6349894!4d-48.7041819!3m4!1s0x94d92b0cbad3db97:0x2572c7ac416b3e51!8m2!3d-26.6349894!4d-48.7041819
# cebrace_1 <- vidro
# cebrace_1$t_dia <- c(rep(600, 3), rep(900, 6+7))
# cebrace_1$data <- "CEBRACE 1"  
# cebrace_1$lat <- -26.63483075996334
# cebrace_1$lon <- -48.70386013492838
# 
# # https://www.google.com.br/maps/place/Cebrace+Cristal+Plano+Ltda/@-23.1318121,-45.7558691,17z/data=!3m1!4b1!4m5!3m4!1s0x94cc4df8d262c527:0x94f2acdd8e147c47!8m2!3d-23.1318219!4d-45.7536746
# cebrace_2 <- vidro
# cebrace_2$t_dia <- rep(600, 9+7)
# cebrace_2$data <- "CEBRACE 2"  
# cebrace_2$lat <- -23.131619692393166
# cebrace_2$lon <- -45.753637487200244
# 
# # https://www.google.com.br/maps/place/Cebrace+Vidros/@-23.2852445,-45.9427194,17z/data=!3m1!4b1!4m5!3m4!1s0x94cdcadf5fee2c61:0x52541d6e18e19203!8m2!3d-23.2852445!4d-45.9405307
# cebrace_3 <- vidro
# cebrace_3$t_dia <- rep(600, 9+7)
# cebrace_3$data <- "CEBRACE 3"  # JACAREI
# cebrace_3$lat <- -23.285032603161703
# cebrace_3$lon <- -45.94056288904746
# 
# # c5 https://www.google.com.br/maps/place/Cebrace+c5/@-23.2905774,-45.9331332,1068m/data=!3m1!1e3!4m5!3m4!1s0x94cdcb5b352d1a73:0xfb5bc49902d5c45b!8m2!3d-23.293044!4d-45.934939
# cebrace_4 <- vidro
# cebrace_4$t_dia <- rep(600, 9+7)
# cebrace_4$data <- "CEBRACE 4"  
# cebrace_4$lat <- -23.292858667555112
# cebrace_4$lon <- -45.93491420773615
# 
# # c5 https://www.google.com.br/maps/place/Cebrace+c5/@-23.2905774,-45.9331332,1068m/data=!3m1!1e3!4m5!3m4!1s0x94cdcb5b352d1a73:0xfb5bc49902d5c45b!8m2!3d-23.293044!4d-45.934939
# cebrace_5 <- vidro
# cebrace_5$t_dia <- c(rep(0, 6), rep(900, 10))
# cebrace_5$data <- "CEBRACE 5"  
# cebrace_5$lat <- -23.292858667555112
# cebrace_5$lon <- -45.93491420773615
# 
# # c5 https://www.google.com.br/maps/place/Cebrace+c5/@-23.2905774,-45.9331332,1068m/data=!3m1!1e3!4m5!3m4!1s0x94cdcb5b352d1a73:0xfb5bc49902d5c45b!8m2!3d-23.293044!4d-45.934939
# cebrace_6 <- vidro
# cebrace_6$t_dia <- c(rep(0, 8), 900, rep(0, 7))
# cebrace_6$data <- "CEBRACE 6"  
# cebrace_6$lat <- -23.292858667555112
# cebrace_6$lon <- -45.93491420773615
# 
# guardian_rj <- vidro
# guardian_rj$t_dia <- rep(600, 16)
# guardian_rj$data <- "GUARDIAN RJ"
# guardian_rj$lat <- -22.42244839828243
# guardian_rj$lon <- -44.31404676931424
# 
# guardian_sp <- vidro
# guardian_sp$t_dia <- c(rep(0, 3), rep(800, 6), rep(830, 2), rep(800, 5))
# guardian_sp$data <- "GUARDIAN SP"
# guardian_sp$lat <- -23.297994637584544
# guardian_sp$lon <- -47.814759632534674
# 
# VIVIX <- vidro # ex CBVP
# VIVIX$t_dia <- c(rep(0, 7), rep(900, 2+7))
# VIVIX$data <- "VIVIX PE"
# VIVIX$lat <- -7.512554296523776
# VIVIX$lon <- -34.98510855979098
# 
# AGC_1 <- vidro
# AGC_1$t_dia <- c(rep(0, 7), rep(600, 2+7))
# AGC_1$data <- "AGC GUARA 1"
# AGC_1$lat <- -22.780198300185614
# AGC_1$lon <- -45.13986708465712
# 
# AGC_2 <- vidro
# AGC_2$t_dia <- c(rep(0, 13), rep(850, 3))
# AGC_2$data <- "AGC GUARA 2"
# AGC_2$lat <- -22.780198300185614
# AGC_2$lon <- -45.13986708465712
# 
# saint_gobain_glass <- vidro
# saint_gobain_glass$t_dia <- c(rep(160, 6), rep(180, 10))
# saint_gobain_glass$data <- "SAINT GOBAIN GLASS SP"
# saint_gobain_glass$lat <- -23.519859019433447
# saint_gobain_glass$lon <- -46.688002242825924
# 
# UBV <- vidro
# UBV$t_dia <- c(rep(210, 3), rep(240, 9), rep(0, 4))
# UBV$data <- "UBV SP"
# UBV$lat <- -23.766672029537602
# UBV$lon <- -46.714508801903634
# 
# dtf <- rbind(
#   cebrace_1,
#   cebrace_2,
#   cebrace_3,
#   cebrace_4,
#   cebrace_5,
#   cebrace_6,
#   guardian_rj,
#   guardian_sp,
#   VIVIX,
#   AGC_1,
#   AGC_1,
#   saint_gobain_glass,
#   UBV
# )
# 
# setDT(dtf)
# 
# dtf$t <- units::set_units(dtf$days*dtf$t_dia, "t")
# 
# ef$Value <- as.numeric(ef$Value)
# 
# ef$gt <- ifelse(ef$Unit == "g/Mg glass", ef$Value,
#                         ifelse(ef$Unit %in% c("% of PM2.5"), ef$Value,
#                                       NA))
# 
# ef$gt <- units::set_units(ef$gt, "g/t")
# 
# rbindlist(pbapply::pblapply(seq_along(ef$Pollutant), function(i) {
#   dtf$gt <- ef$gt[i]
#   dtf$Pollutant <- ef$Pollutant[i]
#   dtf$g <- dtf$t*dtf$gt
#   dtf
# })) -> emi_1 
# 
# emi_1$g <- units::set_units(emi_1$g, "g")
# 
# saveRDS(emi_1, "inventory/2A3_cemento.rds")
# 
# rm(emi_1)
# gc()
# 
