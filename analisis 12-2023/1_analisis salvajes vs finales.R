# ORTIMAR - Objetivo 1
# Analisis efecto del cultivo
# Diciembre 2023

# Comparar los parametros entre los salvajes y los controles cultivados 22 semanas (cultivo si, corte no, madurez si)

# Explorar las variables
# Identificar posibles outliers
# Ver si distribucion es normal y transformar
# Hacer modelos y comprobar asunciones
# Hacer graficas

library(tidyverse)
library(car)
### Cargar datos y filtrar para el analisis ----
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")
source(file = "./analisis 12-2023/0_data_home.R")

data_1 <- filter(datos, corte == "control") %>% 
  filter(cultivo == "No" | madurez == "Si")

### Exploracion ----

ggplot(filter(data_1, tejido == "Tentaculo")) +
  geom_histogram(aes(x = TEAC, fill = playa), bins = 10, alpha = 0.8) +
  facet_wrap(~ madurez)

shapiro.test(filter(data_1, tejido == "Tentaculo")$SOD)


ggplot(data_1) +
  geom_boxplot(aes(x = playa, y = SOD, fill = playa), alpha = 0.8) +
  geom_point(aes(x = playa, color = playa, y = SOD), alpha = 0.9) +
  facet_wrap(~tejido)

# Observacion n 38 (individuo 14) la GR sale mu alta (columna 10)
# Observación n 12 (individuo 12) la G6PDH sale mu alta (columna 13)
# Observación n 6 (individuo 6) la TEAC sale mu baja (columna 15)
# Observación n 1 (individuo 1) la TEAC sale mu alta (columna 15)

### Potenciales outliers ----
data_1[38, 10] <-  NA
data_1[12, 13] <-  NA
data_1[6, 15] <-  NA
data_1[1, 15] <-  NA
data_1[25, 7] <- NA
# Revisar

### Prueba modelos y normalidad ----
# Modelo basico: SOD ~ tiempo
# Modelo con playa: SOD ~ tiempo + playa
data_1 <- select(data_1, -GPx)

modelos_p <- lapply(colnames(data_1[c(7:15)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ cultivo")), filter(data_1, tejido == "Pie"))})

modelos_t <- lapply(colnames(data_1[c(7:15)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ cultivo")), filter(data_1, tejido == "Tentaculo"))})


(modelo_np <- kruskal.test(MDA ~ playa, data = filter(data_1, tejido == "Tentaculo")))


### Graficas ----

  geom_col(aes(fill = playa),
           alpha = 0.6, position = position_dodge()) +
  facet_wrap(~tejido)


