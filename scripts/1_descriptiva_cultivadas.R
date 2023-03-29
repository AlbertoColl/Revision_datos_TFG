### Analisis estadistico TFG 1                                        Comparacion entre individuos salvajes de tres entornos naturales diferentes

# Alberto Coll Fernandez                                                   Comienzo: 23/03/2022                                                           Fin:

### SETUP ----
library(tidyverse)
library(ggthemr)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFG Alberto Coll

# Directorio en portatil: D:/collf/Documents/GitHub/TFG-Alberto-Coll

setwd("D:/collf/Documents/GitHub/TFG-Alberto-Coll")

# Llamamos el script de lectura
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa
datos <- datos %>% filter(cultivo == "Si")

source(file = "./scripts/1_funciones_graficas.R") # Para tener las funciones de las graficas

### Ejecucion ----

# Este bloque itera cada variable del estudio y genera su grafica de barras y de interaccion con el modelo extendido.
for (i in colnames(datos[6:25])) {
  #print(i)
  (grafica <- barras(datos))
  ggsave(paste0("./resultados/graficas/", i, "_barras1.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "retina")
  saveRDS(grafica, file = paste0("./resultados/graficas/", i, "_barras1.RDS"))
  (grafica <- interact(datos))
  ggsave(paste0("./resultados/graficas/", i, "_interaccion1.png"), width = 1500, height = 750, units = "px",
         scale = 2, dpi = "retina")
  saveRDS(grafica, file = paste0("./resultados/graficas/", i, "_interaccion1.RDS"))
  
}
