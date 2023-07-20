
library(data.table)
library(emep)
library(vein)
library(fem)
data("emep")

class(emep)

format(object.size(emep), units = "Mb")


dir.create("inventory")
setorderv(emep, 
          cols = c("NFR", "Sector", "Table", "Type", "Technology", "Fuel", "Pollutant"), 
          order = 1)


# writexl::write_xlsx(unique(emep), "inventory/emep.xlsx")
                   
unique(emep$NFR)
nfr <- unique(emep[, c("NFR", "Sector", "Type", "Technology", "Fuel")])
writexl::write_xlsx(nfr, "inventory/fuentes.xlsx")

unfr <- unique(emep$NFR)

lapply(seq_along(unfr), function(i) {
dt <- emep[NFR == unfr[i]]
writexl::write_xlsx(dt, paste0("inventory/",unfr[i], ".xlsx"))
})

# crear llave en emep
data(brazil)
setDT(brazil)

brazil[, unique(Sector)]
names(brazil)

# Refinarias ####

# Tabela 2.27 – Evolução da capacidade de refino, segundo refinarias – 2011-2020

dir.create("datos")

download.file(
  url = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-processamento-de-petroleo-e-producao-de-derivados/processamento-petroleo-b.xls", 
  destfile = "datos/REFI_1A1b_barris.xls")


df <-  fread("datos/REFI_1A1b_barris.csv")

names(df)[4] <- "MATERIA_PRIMA"

dff <- melt(data = df, id.vars = c("ANO", "ESTADO", "REFINARIA", "MATERIA_PRIMA", "UNIDADE"))

dff[variable != "TOTAL", 
    sum(value, na.rm = T),
    by = .(REFINARIA, variable, ANO)
    ] -> dt

names(dt)[4] <- "barris"
# 1 barrel of crude oil	= 6.193 gigajoules
# https://www150.statcan.gc.ca/n1/pub/57-601-x/00105/4173282-eng.htm
dt$GJ <- 6.193*dt$barris

emep[NFR == "1.A.1.b" &
       Technology == "Process Furnaces, Heaters and Boilers",
     unique(Fuel)]

emep[NFR == "1.A.1.b" &
       Technology == "Process Furnaces, Heaters and Boilers" &
       Fuel == "Gas Oil"] -> emep_refi

emep_refi$ef_gGJ <- ifelse(emep_refi$Unit == "mg/GJ", 
                       as.numeric(emep_refi$Value)/1000, 
                       as.numeric(emep_refi$Value))

library(units)
dt$GJ <- units::set_units(dt$GJ, "GJ")
emep_refi$ef_gGJ <- units::set_units(emep_refi$ef_gGJ, "g/GJ")
for(i in seq_along(emep_refi$Pollutant)) {
  dt[[emep_refi$Pollutant[i]]] <- dt$GJ*emep_refi$ef_gGJ[i]
}

dt[, set_units(sum(CO),"Gg"), by = ANO][, plot(y = V1, x = ANO)]

dt[, set_units(sum(NOx),"Gg"), by = ANO][, plot(y = V1, x = ANO)]

dt[, set_units(sum(SOx),"Gg"), by = ANO][, plot(y = V1, x = ANO)]

dt[, set_units(sum(PM10),"Gg"), by = ANO][, plot(y = V1, x = ANO)]
