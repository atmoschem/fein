library(sf)
library(data.table)
library(cptcity)
library(rnaturalearth)
library(ggplot2)

list.files("inventory/", pattern = ".rds", full.names = T) -> f

# biomassas ####
dt <- readRDS(f[1])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, LAT_DEC, LONG_DEC, Pollutant)] -> biomassas

biomassas$type <- "UTE Biomassas"

# carvao ####
dt <- readRDS(f[2])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, LAT_DEC, LONG_DEC, Pollutant)] -> carvao

carvao$type <- "UTE Carvao"

# gas natural ####
dt <- readRDS(f[3])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, LAT_DEC, LONG_DEC, Pollutant)] -> gas_natural

gas_natural$type <- "UTE Gas Natural"

# oleo diesel ####
dt <- readRDS(f[4])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, LAT_DEC, LONG_DEC, Pollutant)] -> oleo_diesel

oleo_diesel$type <- "UTE Oleo Diesel"

# processos industriais ####
dt <- readRDS(f[5])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(anoReferencia, LAT_DEC, LONG_DEC,  Pollutant)] -> proce_indus

proce_indus$type <- "UTE Procesos industriais"


# refinaria ####
dt <- readRDS(f[6])

names(dt)
dt[, 
   sum(g, na.rm = T), 
   by = .(year, lat, lon, Pollutant)] -> refinaria

refinaria$type <- "Refinaria"


# cemento ####
dt <- readRDS(f[7])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, Latitude, Longitude,  Pollutant)] -> cemento

cemento$type <- "Cementos"


# refinaria 2 ####
dt <- readRDS(f[8])

names(dt)

dt[, 
   sum(g, na.rm = T), 
   by = .(year, lat, lon, Pollutant)] -> refinaria2

refinaria2$type <- "Refinaria"


# pulpa ####
dt <- readRDS(f[9])

dt[, 
   sum(emi_g, na.rm = T), 
   by = .(year, Latitude, Longitude, Pollutant)] -> pulpa

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

dt$V1 <- units::set_units(dt$V1, "t")

cl <-  rnaturalearth::ne_countries(returnclass = "sf")

cl <- st_cast(cl[cl$adm0_a3 == "BRA", ], "MULTILINESTRING")


names(dt)
dt <- dt[, sum(V1), 
         by = .(anoReferencia, LAT_DEC, LONG_DEC, Pollutant)]


dt <- st_as_sf(dt, coords = c( "LONG_DEC", "LAT_DEC"), crs = 4326)

saveRDS(dt, "INDUSTRIA_ESPACIAL.rds")

dir.create("plot_country")
# CO ####
dir.create("plot_country/CO")

dx <- dt[dt$Pollutant == "CO", ]
ys <- unique(dx$anoReferencia)
ys <- ys[order(ys)]

rg <- round(range(dx$V1)*1.01)


br <- classInt::classIntervals(var = dx$V1, n = 100, style = "sd")
br <- unique(br$brks)

for(i in seq_along(ys) ){
p <- ggplot(dx[dx$anoReferencia == ys[i], ]) +
  geom_sf(aes(colour = as.numeric(V1))) +
  scale_color_gradientn(colours = cpt(569 ),
                        limits = rg,
                        values = br/max(br))+
  facet_wrap(~anoReferencia) +
  bbplot::bbc_style() +
  geom_sf(data = cl)+
  theme(legend.key.width = unit(4, "line"))

png(
  filename = paste0("plot_country/CO/", ys[i], ".png"), 
  width = 2000,
  height = 2500, 
  res = 300
  )
print(p)

dev.off()

}
library(magick)

im <- list.files(path = "plot_country/CO/", pattern = ".png", full.names = T)
im <- do.call("c", lapply(im, image_read))

image_write(image = magick::image_animate(image = im, 
                                          fps = 5), 
            path = "plot_country/CO/CO.gif")

# NOx ####
dir.create("plot_country/NOx")
dx <- dt[dt$anoReferencia > 1999 &
           dt$Pollutant == "NOx", ]
ys <- unique(dx$anoReferencia)
ys <- ys[order(ys)]

rg <- round(range(dx$V1)*1.01)


br <- classInt::classIntervals(var = dx$V1, n = 100, style = "sd")
br <- unique(br$brks)

for(i in seq_along(ys) ){
  p <- ggplot(dx[dx$anoReferencia == ys[i], ]) +
    geom_sf(aes(colour = as.numeric(V1))) +
    scale_color_gradientn(colours = cpt(607 ),
                          limits = rg,
                          values = br/max(br))+
    facet_wrap(~anoReferencia) +
    bbplot::bbc_style() +
    geom_sf(data = cl)+
    theme(legend.key.width = unit(4, "line"))
  
  png(
    filename = paste0("plot_country/NOx/", ys[i], ".png"), 
    width = 2000,
    height = 2500, 
    res = 300
  )
  print(p)
  
  dev.off()
  
}

im <- list.files(path = "plot_country/NOx/", pattern = ".png", full.names = T)
im <- do.call("c", lapply(im, image_read))

image_write(image = magick::image_animate(image = im, 
                                          fps = 5), 
            path = "plot_country/NOx/NOx.gif")



plot(dt[dt$Pollutant == "PM2.5", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T, pal = "mpl_viridis"), breaks = "sd",
     main = "PM2.5 [t/ano]")

plot(dt[dt$Pollutant == "NOx", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T), breaks = "sd",
     main = "NOx [t/ano]")

plot(dt[dt$Pollutant == "NMVOC", "V1"], axes = T, pch = 16,
     pal = cpt(colorRampPalette = T, rev = T, pal = "mpl_viridis"), breaks = "sd",
     main = "NMVOC [t/ano]")





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
# br$V1 <- units::set_units(br$V1, "g")
br$year <- 2020
names(dt)[1] <- "year"
dt$Estimation <- "Sergio"
br$Estimation <- "Ana"


dx <- rbind(dt, br)

dx$type_est <- paste(dx$type, dx$Estimation)

dx$V1 <- units::set_units(dx$V1, "t")

dx[Pollutant == "CO" & year == 2020, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation            V1
# 1:        CO     Sergio  700995.6 [t]
# 2:        CO        Ana 7539727.6 [t]

dx[Pollutant == "NOx" & year == 2020, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation            V1
# 1:       NOx     Sergio 4067183.0 [t]
# 2:       NOx        Ana  441498.2 [t]

dx[Pollutant == "NMVOC" & year == 2020, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation           V1
# 1:     NMVOC     Sergio 185703.7 [t]
# 2:     NMVOC        Ana 222018.5 [t]


dx[Pollutant == "PM2.5" & year == 2020, sum(V1), by = .(Pollutant, Estimation)]
# Pollutant Estimation           V1
# 1:     PM2.5     Sergio 100510.9 [t]
# 2:     PM2.5        Ana 297973.5 [t]

library(ggplot2)

ggplot(dx, 
       aes(x = year, 
           y = as.numeric(V1), 
           fill = Estimation)) +
  labs(y = "t/ano", x = NULL)+
  geom_bar(stat = 'identity',
           position = "dodge", 
           width = 1) +
  facet_wrap(~Pollutant, scales = "free_y") +
  bbplot::bbc_style() +
  theme(strip.text = element_text(size = 20))


ggplot(dx[year == 2020 &
            Pollutant %in% c("CO") &
            Estimation == "Sergio"], 
       aes(x = type, 
           y = as.numeric(V1))) +
  labs(y = "t/ano", x = NULL)+
  geom_bar(stat = 'identity', position = "dodge") +
  facet_grid(Pollutant~Estimation, scales = "free")  +
  coord_flip()



dx$V1 <- units::set_units(dx$V1, "t")

ggplot(dx[Estimation == "Sergio"], 
       aes(x = year, 
           y = as.numeric(V1), 
           fill = type)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Pollutant, scales = "free_y") 

