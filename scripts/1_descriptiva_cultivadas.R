### Analisis estadistico TFG 1                                        Comparacion entre individuos salvajes de tres entornos naturales diferentes

# Alberto Coll Fernandez                                                   Comienzo: 23/03/2022                                                           Fin:

# Problemas y aspectos a resolver:                                                   - Podria hacer otra version de la grafica de interaccion y barras para           los casos en los que no incluya la playa en el modelo, sobre todo en el         caso de interaccion que creo que ayudaria mucho a hacerla mas visual         - Deberia hacer version de las graficas para el modelo simplificado de           corte * playa

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
  print(barras(datos))
  ggsave(paste0("./resultados/graficas/", i, "_barras1.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "retina")
  print(interact(datos))
  ggsave(paste0("./resultados/graficas/", i, "_interaccion1.png"), width = 1500, height = 750, units = "px",
         scale = 2, dpi = "retina")
}
