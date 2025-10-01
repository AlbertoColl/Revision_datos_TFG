### ORTIMAR - Objetivos 1 y 2
# Lectura y limpieza de datos
# Enero 2025 - revision para publicacion

library(tidyverse)

# Importacion ----

#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

datos <- read.csv2("./datos/TFG_datos_2025.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         section = as.factor(corte), time = as.factor(madurez)) %>% 
  select(-madurez, -corte) %>% relocate(section, .after = cultivo)

datos_long <- read.csv2("./datos/TFG_datos_2025_long.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         section = as.factor(corte), time = as.factor(madurez)) %>% 
  select(-madurez, -corte) %>% relocate(section, .after = cultivo)


datos_long2 <- read.csv2("./datos/Datos_sin_NaS_2025.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         section = as.factor(corte), time = as.factor(madurez), group = section:time) %>% 
  select(-madurez, -corte) %>% relocate(section, .after = cultivo)

datos$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles
datos_long$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles



# Poner in english los nombres de los niveles
datos$section <- factor(datos$section, levels = c("No", "Si"), labels = c("control", "dissected"))
datos$cultivo <- factor(datos$cultivo, levels = c("No", "Si"), labels = c("wild", "cultured"))
datos$time <- factor(datos$time, levels = c("No", "Si"), labels = c("0", "1"))


# Limpieza de datos ----
datos <- datos %>% 
  mutate(GPx_t = ifelse(GPx_t < 0, NA, GPx_t),
         GPx_p = ifelse(GPx_p < 0, NA, GPx_p))  # hay datos de gpx negativos


#ggboxplot(filter(datos_long2, tejido == "Tentaculo", cultivo == "Si"), x = "group", y = "TEAC", color = "section", add = "jitter")

datos_long2$CAT[41] <- NA
datos_long2$MDA[31] <- NA
datos_long2$TEAC[17] <- NA
