library(sf)
library(data.table)
library(ggplot2)

list.files("inventory/", pattern = ".rds", full.names = T) -> f

# biomassas ####
dt <- readRDS(f[1])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, 
          mesReferencia, 
          Pollutant, 
          LAT_DEC, 
          LONG_DEC)] -> biomassas

biomassas$type <- "UTE Biomassas"

# carvao ####
dt <- readRDS(f[2])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, 
          mesReferencia,
          Pollutant, 
          LAT_DEC, 
          LONG_DEC)] -> carvao

carvao$type <- "UTE Carvao"

# gas natural ####
dt <- readRDS(f[3])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, 
          mesReferencia,
          Pollutant, 
          LAT_DEC, 
          LONG_DEC)] -> gas_natural

gas_natural$type <- "UTE Gas Natural"

# oleo diesel ####
dt <- readRDS(f[4])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, 
          mesReferencia,
          Pollutant, 
          LAT_DEC, 
          LONG_DEC)] -> oleo_diesel

oleo_diesel$type <- "UTE Oleo Diesel"

# processos industriais ####
dt <- readRDS(f[5])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, 
          mesReferencia,
          Pollutant, 
          LAT_DEC, 
          LONG_DEC)] -> proce_indus

proce_indus$type <- "UTE Procesos industriais"


# refinaria ####
dt <- readRDS(f[6])

dt[, 
   sum(g, na.rm = T), 
   by = .(year, 
          month,
          Pollutant, 
          lat, 
          lon)] -> refinaria

refinaria$type <- "Refinaria"


# cemento ####
dt <- readRDS(f[7])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, 
          month,
          Pollutant, 
          Latitude, 
          Longitude)] -> cemento

cemento$type <- "Cementos"

# refinaria ####
dt <- readRDS(f[8])

dt[, 
   sum(g, na.rm = T), 
   by = .(year, 
          month,
          Pollutant, 
          lat, 
          lon)] -> refinaria2

refinaria2$type <- "Refinaria"

# pulpa ####
dt <- readRDS(f[9])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, 
          month,
          Pollutant, 
          Latitude, 
          Longitude)] -> pulpa

pulpa$type <- "Papel e Polpa"

dt <- rbind(
  biomassas,
  carvao,
  gas_natural,
  oleo_diesel,
  proce_indus,
  cemento,
  pulpa,
  refinaria,
  refinaria2,
  use.names = F
)
names(dt)
# saveRDS(dt, "inventory/ALL.rds")
names(dt) <- c("year", "month", "Pollutant", "lat", "lon", "g", "type")
# 15 millones puntos
fwrite(dt, "inventory/inventory.csv.gz")

dt$V1 <- units::set_units(dt$g, "t")

dt <- st_as_sf(dt, coords = c( "lon", "lat"), crs = 4326)

ggplot(dt) +
  geom_sf(aes(colour = type)) +
  theme_bw() +
  labs(title = "Fontes") 

plot(dt[dt$Pollutant == "CO", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T), breaks = "sd",
     main = "CO [t/ano]")


plot(dt[dt$Pollutant == "PM2.5", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T, pal = "mpl_viridis"), breaks = "sd",
     main = "PM2.5 [t/ano]")

plot(dt[dt$Pollutant == "NOx", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T), breaks = "sd",
     main = "NOx [t/ano]")

plot(dt[dt$Pollutant == "NMVOC", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T, pal = "mpl_viridis"), breaks = "sd",
     main = "NMVOC [t/ano]")


