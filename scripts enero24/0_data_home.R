### ORTIMAR - Lectura y limpieza de datos
# Hecho el 21/12/20222

library(tidyverse)

# Importacion ----

setwd("D:/collf/Documents/GitHub/TFG-Alberto-Coll")

datos <- read.csv2("./datos/TFG_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         corte = as.factor(corte), madurez = as.factor(madurez),
         tejido = as.factor(tejido))

datos$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles

# Pivotar variable tejido a nuevas variables
datos <- datos %>% pivot_longer(cols = c(SOD, CAT, GPx, GR, GST, DTD, G6PDH, TEAC, MDA, proteina)) %>% 
  mutate(variable = paste0(name, "_", ifelse(tejido == "Tentaculo", "t", "p")),
         tiempo = ifelse(madurez == "No", "0", "1")) %>% 
  select(individuo, playa,cultivo, corte, tiempo, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


# Poner in english los nombres de los niveles
datos$corte <- factor(datos$corte, levels = c("No", "Si"), labels = c("control", "dissected"))

# Limpieza de datos ----
datos <- datos %>% 
  mutate(GPx_t = ifelse(GPx_t < 0, NA, GPx_t),
         GPx_p = ifelse(GPx_p < 0, NA, GPx_p))  # hay datos de gpx negativos

# Aqui voy eliminando (y marcando) los posibles outliers
datos[13,8] <- NA # GPX tentacular, n 13
datos[12,8] <- NA
datos[6,8] <- NA
# datos[16,6] <- NA #posible outlier, en SOD tent Al mirar en los datos, las dos replicas de abs tenian un error bastante alto ¿Quitamos valor?
