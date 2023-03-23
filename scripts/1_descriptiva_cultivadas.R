### Analisis exploratorio ORTIMAR 2 - Efecto de inducir la reproduccion asexual en ortiguillas cultivadas y seguimiento tras maduracion

# Alberto Coll Fernandez
# Comienzo: 14/12/2022
# Fin: 31/12/2022

### SETUP ----

library(dplyr)
library(ggplot2)
library(ggthemr)
library(lme4)
library(multcompView)


setwd("D:/collf/Documents/ORTIMAR (Alberto Coll)")
#setwd("C:/Users/Usuario/Documents/ORTIMAR (Alberto Coll)")

# Llamamos el script de lectura
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa

# Filtramos para tener solo anemonas cultivadas
datos <- datos %>% filter(cultivo == "Si")

### Examen de outliers----

# Plantilla de boxplot para ver distribucion y outliers
ggplot(datos, aes(x = corte, y = CAT)) +
  geom_boxplot(aes(fill = tiempo),
               alpha = 0.5, varwidth = T, na.rm = T, outlier.alpha = 0) +
  #stat_summary(aes(group = tiempo), # para mostrar la media
  #             fun.y = "mean", geom = "point", size = 2.5, color = "white",
  #             shape = 15, position = position_dodge2(width = 0.75), na.rm = T) +
  geom_point(aes(color = tiempo),
             size = 2, position = position_jitterdodge(), na.rm = T) +
  facet_wrap(~tejido, scales = "free")

# Outliers en potencia segun boxplot (fuera de IQR * 1.5)
# Todos las alteraciones estan tambien reflejadas en el excel

datos[42,7] <- NA # 21, pie, SOD - outlier 
datos[16,7] <- NA # 31, tent, SOD - outlier
datos[7,7] <- 58.135  # 22, tent, SOD - cambio de valor por proteina
datos[3,7] # 18, tent, SOD - mantenido

datos[52,8] <- NA # 31, pie, CAT - outlier
datos[12,8] <- NA # 27, tent, CAT - outlier, ademas sin replica
datos[7,8] <- 29.807 # 22, tent, CAT - cambio de valor por proteina
datos[1,8] <- 13.985 # 16, tent, CAT - cambio de valor por replica mala

datos[45,9] <- NA # 24, pie, GPx - outlier
datos[2,9] <- NA # 17, tent, GPx - outlier

datos[41,10] # 20, pie, GR - mantenido
datos[11,10] # 26, tent, GR - mantenido
datos[35,10] # 50, tent, GR - mantenido

datos[53,11] # 32, pie, GST - mantenido
datos[48,11] # 27, pie, GST - mantenido
datos[68,11] <- NA # 47, pie, GST - outlier
datos[69,11] <- NA  # 48, pie, GST - outlier
datos[16,11] <- 96.820 # 31, tent, GST - cambio de valor por proteina
datos[7,11] # 22, tent, GST - mantenido?? dudas proteina
datos[9,11] # 24, tent, GST - mantenido?? dudas
datos[34,11] <- NA # 49, tent, GST - outlier
datos[29,11] # 44, tent, GST - mantenido

datos[67,12] # 46, pie, DTD - mantenido
datos[12,12] <- NA # 27, tent, DTD - outlier

datos[46,13] # 25, pie, G6PDH - dudas
datos[47,13] # 26, pie, G6PDH - dudas
datos[45,13] <- NA # 24, pie, G6PDH - outlier
datos[70,13] <- NA # 49, pie, G6PDH - outlier
datos[71,13] <- NA # 50, pie, G6PDH - OUTLIER
datos[23,13] <- 18.524 # 38, tent, G6PDH - cambio de valor por mala replica

datos[61,16] <- NA # 40, pie, MDA - outlier 
datos[65,16] <- NA # 44, pie, MDA - outlier
datos[2,16] # 17, tent, MDA - mantenido
datos[6,16] # 21, tent, MDA - mantenido


# Analisis Descriptivo ----

ggplot(filter(datos, corte == "No", tiempo == 0)) +
  geom_density(aes(x = SOD, fill = playa, color = playa),
              alpha = 0.3) +
  facet_grid(tejido~playa, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        axis.ticks.x=element_blank())

