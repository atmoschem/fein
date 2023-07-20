library(sf)
library(data.table)
library(cptcity)
library(ggplot2)
library(magick)


dt <- readRDS("INDUSTRIA_ESPACIAL.rds")
cl <-  rnaturalearth::ne_countries(returnclass = "sf")
cl <- st_cast(cl[cl$adm0_a3 == "BRA", ], "MULTILINESTRING")

dir.create("plot_country")

# CO ####
dx <- dt[dt$anoReferencia < 2021 &
           dt$Pollutant == "CO", ]

yf <- function(x) x^2
br <- yf(1:100)/100^2

ggplot(dx) +
  geom_sf(aes(colour = as.numeric(V1))) +
  facet_wrap(~anoReferencia, nrow = 4) +
  scale_color_gradientn(colours = cpt(569),
                        values = br)+
  #bbplot::bbc_style() +
  geom_sf(data = cl)+
  theme(legend.key.width = unit(4, "line")) -> p



png(
  filename = paste0("plot_country/CO.png"), 
  width = 5000,
  height = 3000, 
  res = 300
  )
print(p)
dev.off()


# NOx ####
unique(dt$Pollutant)
dx <- dt[dt$anoReferencia < 2021 &
           dt$Pollutant == "NOx", ]

yf <- function(x) x^2
br <- yf(1:100)/100^2

ggplot(dx) +
  geom_sf(aes(colour = as.numeric(V1))) +
  facet_wrap(~anoReferencia, nrow = 4) +
  scale_color_gradientn(colours = cpt(rev = TRUE),
                        values = br)+
  bbplot::bbc_style() +
  geom_sf(data = cl)+
  theme(legend.key.width = unit(4, "line")) -> p2


png(
  filename = paste0("plot_country/NOx.png"), 
  width = 5000,
  height = 3000, 
  res = 300
)
print(p2)
dev.off()



# NMVOC ####
unique(dt$Pollutant)
dx <- dt[dt$anoReferencia < 2021 &
           dt$Pollutant == "NMVOC", ]

yf <- function(x) x^2
br <- yf(1:100)/100^2

ggplot(dx) +
  geom_sf(aes(colour = as.numeric(V1))) +
  facet_wrap(~anoReferencia, nrow = 4) +
  scale_color_gradientn(colours = lucky(), #2645 
                        values = br)+
  bbplot::bbc_style() +
  geom_sf(data = cl)+
  theme(legend.key.width = unit(4, "line")) -> p2


png(
  filename = paste0("plot_country/NMVOC.png"), 
  width = 5000,
  height = 3000, 
  res = 300
)
print(p2)
dev.off()




# PM2.5 ####
unique(dt$Pollutant)
dx <- dt[dt$anoReferencia < 2021 &
           dt$Pollutant == "PM2.5", ]

yf <- function(x) x^2
br <- yf(1:100)/100^2

ggplot(dx) +
  geom_sf(aes(colour = as.numeric(V1))) +
  facet_wrap(~anoReferencia, nrow = 4) +
  scale_color_gradientn(colours = cpt(pal = "mpl_viridis", rev = TRUE),
                        values = br)+
  bbplot::bbc_style() +
  geom_sf(data = cl)+
  theme(legend.key.width = unit(4, "line")) -> p2


png(
  filename = paste0("plot_country/PM25.png"), 
  width = 5000,
  height = 3000, 
  res = 300
)
print(p2)
dev.off()


