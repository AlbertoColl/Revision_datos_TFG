### ORTIMAR - Objetivos 1 y 2
# Lectura y limpieza de datos
# Septiembre 2024 - revision para publicacio

library(tidyverse)

# Importacion ----

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")

datos <- read.csv2("./datos/TFG_datos_2024.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         corte = as.factor(corte), madurez = as.factor(madurez))


datos$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles


# Poner in english los nombres de los niveles
datos$corte <- factor(datos$corte, levels = c("No", "Si"), labels = c("control", "dissected"))
datos$cultivo <- factor(datos$cultivo, levels = c("No", "Si"), labels = c("wild", "cultured"))


# Limpieza de datos ----
datos <- datos %>% 
  mutate(GPx_t = ifelse(GPx_t < 0, NA, GPx_t),
         GPx_p = ifelse(GPx_p < 0, NA, GPx_p))  # hay datos de gpx negativos
