library(sf)
library(data.table)
library(ggplot2)
library(fem)

(list.files("inventory/", pattern = ".rds", full.names = T) -> f)

# biomassas ####
dt <- readRDS(f[1])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, mesReferencia, Pollutant, NFR, Sector, Technology)] -> biomassas

biomassas$type <- "UTE Biomassas"

# carvao ####
dt <- readRDS(f[2])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, mesReferencia, Pollutant, NFR, Sector, Technology)] -> carvao

carvao$type <- "UTE Carvao"

# gas natural ####
dt <- readRDS(f[3])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, mesReferencia, Pollutant, NFR, Sector, Technology)] -> gas_natural

gas_natural$type <- "UTE Gas Natural"

# oleo diesel ####
dt <- readRDS(f[4])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, mesReferencia, Pollutant, NFR, Sector, Technology)] -> oleo_diesel

oleo_diesel$type <- "UTE Oleo Diesel"

# processos industriais ####
dt <- readRDS(f[5])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, mesReferencia, Pollutant, NFR, Sector, Technology)] -> proce_indus

proce_indus$type <- "UTE Procesos industriais"


# refinaria ####
dt <- readRDS(f[6])

names(dt)

dt[, 
   sum(g, na.rm = T), 
   by = .(year, month, Pollutant, NFR, Sector, Technology)] -> refinaria

refinaria$type <- "Refinaria"


# refinaria ####
dt <- readRDS(f[8])

dt[, 
   sum(g, na.rm = T), 
   by = .(year, month, Pollutant, NFR, Sector, Technology)] -> refinaria2

refinaria2$type <- "Refinaria"

# cemento ####
dt <- readRDS(f[7])

names(dt)

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, month, Pollutant, NFR, Sector, Technology)] -> cemento

cemento$type <- "Cementos"


# pulpa ####
dt <- readRDS(f[9])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, month, Pollutant, NFR,Sector, Technology)] -> pulpa

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
saveRDS(dt, "inventory/ALL_YM_.rds")


dt$t <- units::set_units(dt$V1, "t")
dt$date <- ISOdate(dt$anoReferencia, 
                   dt$mesReferencia,
                   1)

ggplot(dt, 
       aes(x = date, 
           y = as.numeric(t), 
           fill = type)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Pollutant, scales = "free_y") +
  bbplot::bbc_style() -> p

bbplot::finalise_plot(plot_name = p, source_name = "Sergio Ibarra-Espinosa", save_filepath =  "pol_YM.png")
