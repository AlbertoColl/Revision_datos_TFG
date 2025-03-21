### ORTIMAR - Objetivos 1 y 2
# Lectura y limpieza de datos
# Enero 2025 - revision para publicacion

library(tidyverse)

# Importacion ----

#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

# Cambiar tiempo a time o sampling
# Cambiar corte a sectioning o treatment
datos <- read.csv2("./datos/TFG_datos_2025.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         section = as.factor(corte), time = as.factor(madurez)) %>% 
  select(-madurez, -corte) %>% relocate(section, .after = cultivo)


datos$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles


# Poner in english los nombres de los niveles
datos$section <- factor(datos$section, levels = c("No", "Si"), labels = c("control", "dissected"))
datos$cultivo <- factor(datos$cultivo, levels = c("No", "Si"), labels = c("wild", "cultured"))
datos$time <- factor(datos$time, levels = c("No", "Si"), labels = c("0", "1"))
