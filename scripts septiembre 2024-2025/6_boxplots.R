#### ORTIMAR - Objetivo 2 (V2 cambiando graficas)
# Boxplots para publicacion
# Febrero 2025 - revision para publicacion

library(tidyverse)
library(rstatix)
library(ggpubr)
library(patchwork)

### SETUP ----
setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
#setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
#source(file = "./scripts septiembre 2024-2025/0_data_laptop.R")

data <- filter(datos_long, cultivo == "Si") %>%
  mutate(grupo = time:section)

data$GPx[9] <- NA # Afecta a homcedasticidad y es extremo
data$GPx[6] <- NA #HOMOCEDASTICIDAD
data$GPx[2] <- NA
data$GPx[33] <- NA #posible
data$GPx[4] <- NA # Quitar el otro dato  #4 para l GPx tentacular


data$GPx[38] <- NA # outlier extremo
data$GPx[51] <- NA # extremo
data$GPx[53] <- NA
data$GPx[66] <- NA
data$GPx[69] <- NA

# Se eliminan:
data$SOD[7] <- NA #para normalidad
data$SOD[3] <- NA #para normalidad
# Quitar la  #35 tambien


data$CAT[35] <- NA # La funcion ha identificado otro, pero su eliminacion no afecta a la normalidad de residuos, este si.

data$Mielo[62] <- NA
data$Mielo[18] <- NA #Sospechoso, y afecta a normalidad

data$Facida[66] # sospechoso, outlier no extremo

data$Fbasica[48] <- NA # outlier extremo
data$Fbasica[51] # incrementa mucho error pero no se puede quitar porque violaria homocedasticidad

data$Fbasica[9] <- NA # Afecta a homocedasticidad

data$Lisozima[69] <- NA # outlier extremo
data$Lisozima[70] <- NA # outlier extremo
data$Lisozima[38] <- NA # outlier extremo

data$MDA[65] <- NA # Outlier no extremo pero parece que puede afectar al resultado del analisis y afecta a la varianza del grupo.

### Modelo de grafica ----

ggboxplot(data, x = "time", y = "CAT", color = "grupo", fill = "grupo", alpha = 0.15, width = 0.5, add = c("jitter", "mean"), add.params = list(shape = 1), facet.by = "tejido", ylab = "U/mg protein", panel.labs = list(tejido = c("Column", "Tentacle"))) +
  theme_test() + labs(color = "Treatment")+ labs_pubr() + 
  scale_x_discrete(labels = c("T1", "T2")) +
  scale_fill_manual(values = c("#12A3F8","#E64B35FF","#0571B0","#BE2F36")) +
  scale_color_manual(values = c("#12A3F8","#E64B35FF","#0571B0","#BE2F36"), labels = c("Control T1", "Sectioned T1","Control T2", "Sectioned T2")) + guides(fill = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 9))

# Paleta (unificar con PCA)
c("#12A3F8","#E64B35FF","#0571B0","#BE2F36")

### Automatizacion ----

# Hacerlo con sapply()
# Intentar hacerlo con doo() pero saldria en dataframe
# Resignarse y hacerlo en bucle

