library(sf)
library(data.table)
library(ggplot2)

list.files("inventory/", pattern = ".rds", full.names = T) -> f

# biomassas ####
dt <- readRDS(f[1])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, Pollutant, NFR, Sector, Technology)] -> biomassas

biomassas$type <- "UTE Biomassas"

# carvao ####
dt <- readRDS(f[2])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, Pollutant, NFR, Sector, Technology)] -> carvao

carvao$type <- "UTE Carvao"

# gas natural ####
dt <- readRDS(f[3])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, Pollutant, NFR, Sector, Technology)] -> gas_natural

gas_natural$type <- "UTE Gas Natural"

# oleo diesel ####
dt <- readRDS(f[4])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, Pollutant, NFR, Sector, Technology)] -> oleo_diesel

oleo_diesel$type <- "UTE Oleo Diesel"

# processos industriais ####
dt <- readRDS(f[5])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, Pollutant, NFR, Sector, Technology)] -> proce_indus

proce_indus$type <- "UTE Procesos industriais"


# refinaria ####
dt <- readRDS(f[6])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, Pollutant, NFR, Sector, Technology)] -> refinaria

refinaria$type <- "Refinaria"


# cemento ####
dt <- readRDS(f[7])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, Pollutant, NFR, Sector, Technology)] -> cemento

cemento$type <- "Cementos"

# refinaria ####
dt <- readRDS(f[8])

dt[, 
   sum(g, na.rm = T), 
   by = .(year, Pollutant, NFR, Sector, Technology)] -> refinaria2

refinaria2$type <- "Refinaria"

# pulpa ####
dt <- readRDS(f[9])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, Pollutant, NFR,Sector, Technology)] -> pulpa

pulpa$type <- "Papel e Polpa"

dt <- rbind(
  biomassas,
  carvao,
  gas_natural,
  oleo_diesel,
  proce_indus,
  cemento,
  pulpa,
  # refinaria,
  refinaria2,
  use.names = F
)
saveRDS(dt, "inventory/ALL.rds")

dt$V1 <- units::set_units(dt$V1, "t")

library(fem)
data(brazil)
setDT(brazil)

names(brazil)[5:22]

brazil[, 
       lapply(.SD, sum, na.rm = T),
       .SDcols = c("NOx-low(Gg/ano)", "SOx-low(Gg/ano)", "CO-low(Gg/ano)",
                   "MP-low(Gg/ano)", "TOC-low(Gg/ano)", "CO2-low(Gg/ano)"),
       by = Sector] -> br

names(br) <- c("Sector", "NOx", "SOx", "CO", "PM2.5", "TOC", "CO2")
unique(dt$type)

br$type <- ifelse(
  br$Sector == "Petroleum refining", "Refinaria",
  ifelse(
    br$Sector == "Portland cement", "Refinaria",
    ifelse(
      br$Sector == "Paper and cellulose", "Papel e Polpa",
      ifelse(
        br$Sector == "Bagasse TPP", "UTE Biomassas",
        ifelse(
          br$Sector == "Natural gas TPP", "UTE Gas Natural",
          ifelse(
            br$Sector == "Coal TPP", "UTE Carvao",
            ifelse(
              br$Sector %in% c("Diesel TPP"), "UTE Oleo Diesel",
              ifelse(
                br$Sector %in% c("Fuel oil TPP"), "Fuel oil TPP",
                
                "caca")
              )))))))

melt.data.table(data = br[, -1]) -> br


br[, sum(value), by = .(type, variable)] -> br

names(br)[2] <- "Pollutant"
br$Pollutant <- as.character(br$Pollutant)
br$Pollutant <- ifelse(br$Pollutant == "TOC", "NMVOC", br$Pollutant)
br$V1 <- units::set_units(br$V1, "Gg")
br$V1 <- units::set_units(br$V1, "t")
br$year <- 2011
names(dt)[1] <- "year"
dt$Estimation <- "Sergio"
br$Estimation <- "Ana"


dx <- rbind(dt[, c("type", "Pollutant", "V1", "year", "Estimation")], 
            br,
            use.names = F)

dx$type_est <- paste(dx$type, dx$Estimation)

dx[Pollutant == "CO" & year == 2011, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation          V1
# 1:        CO     Sergio 4403015 [t]
# 2:        CO        Ana 7539728 [t]

dx[Pollutant == "NOx" & year == 2011, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation           V1
# 1:       NOx     Sergio 125773.2 [t]
# 2:       NOx        Ana 441498.2 [t]

dx[Pollutant == "NMVOC" & year == 2011, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation           V1
# 1:     NMVOC     Sergio 106958.5 [t]
# 2:     NMVOC        Ana 222018.5 [t]


dx[Pollutant == "PM2.5" & year == 2011, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation            V1
# 1:     PM2.5     Sergio  35893.87 [t]
# 2:     PM2.5        Ana 297973.51 [t]

library(ggplot2)

ggplot(dx, 
       aes(x = year, 
           y = as.numeric(V1)/1000000, 
           fill = Estimation)) +
  labs(y = "Mt/ano", x = NULL)+
  geom_bar(stat = 'identity',
           position = "dodge", 
           width = 1) +
  facet_wrap(~Pollutant, scales = "free_y") +
  bbplot::bbc_style() +
  theme(strip.text = element_text(size = 20))


ggplot(dx[year == 2011 &
            Pollutant %in% c("CO") &
            Estimation == "Sergio"], 
       aes(x = type, 
           y = as.numeric(V1))) +
  labs(y = "t/ano", x = NULL)+
  geom_bar(stat = 'identity', position = "dodge") +
  facet_grid(Pollutant~Estimation, scales = "free")  +
  coord_flip()



# dx$V1 <- units::set_units(dx$V1, "t")

ggplot(dx[Estimation == "Sergio"], 
       aes(x = year, 
           y = as.numeric(V1), 
           fill = type)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Pollutant, scales = "free_y") 

