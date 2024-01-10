### ORTIMAR - Lectura y limpieza de datos
# Analisis estadistico para publicacion
# Enero 2024

library(tidyverse)

# Importacion ----

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")

datos <- read.csv2("./datos/TFG_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         corte = as.factor(corte), madurez = as.factor(madurez),
         tejido = as.factor(tejido))

datos$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles

# Pivotar variable tejido a nuevas variables
#datos <- datos %>% pivot_longer(cols = c(SOD, CAT, GPx, GR, GST, DTD, G6PDH, TEAC, MDA, proteina)) %>% 
#  mutate(variable = paste0(name, "_", ifelse(tejido == "Tentaculo", "t", "p")),
#         tiempo = ifelse(madurez == "No", "0", "1")) %>% 
#  select(individuo, playa,cultivo, corte, tiempo, variable, value) %>% 
#  pivot_wider(names_from = variable, values_from = value)


# Poner in english los nombres de los niveles
datos$corte <- factor(datos$corte, levels = c("No", "Si"), labels = c("control", "dissected"))

# Limpieza de datos ----
datos <- datos %>% 
  mutate(GPx = ifelse(GPx < 0, NA, GPx))  # hay datos de gpx negativos
