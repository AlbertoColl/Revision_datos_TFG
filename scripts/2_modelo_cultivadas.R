### Modelo estadistico ORTIMAR 2 - Efecto de inducir la reproduccion asexual en ortiguillas cultivadas y seguimiento tras maduracion

# Alberto Coll Fernandez
# Comienzo: 31/12/2022
# Fin: 


# Pregunta de investigacion 1; ¿Se ven afectados los parametros medidos por el corte del animal?

# Pregunta de investigacion 2: ¿Como interaccionan estos parametros cuando se produce una posterior maduracion sexual del animal?


# Para cada parametro (variables continuas) quiero ver el efecto de dos variables cualitativas (corte y tiempo), y posiblemente su interaccion. Anova de dos vias podria estar bien 

# Problema: hay variabilidad asociada a la playa de origen -> Modelo de efectos mixtos, incluyendo playa como factor aleatorio


### SETUP ----

library(dplyr)
library(ggplot2)
library(ggthemr)
library(lme4)
library(multcompView)


setwd("D:/collf/Documents/ORTIMAR (Alberto Coll)")
#setwd("C:/Users/Usuario/Documents/ORTIMAR (Alberto Coll)")

# Llamamos el script de lectura
#source(file = "./scripts/1_descriptiva_cultivadas.R") # Laboratorio
source(file = "./scripts/1_descriptiva_cultivadas.R") # En casa


### MODELO  BASICO (EJEMPLO SOD ~ CORTE y SOD ~ CORTE*TIEMPO)----

# Examinar distribucion de los datos por playa y agrupados
ggplot(data = filter(datos, tejido == "tentacle")) +
  geom_histogram(aes(x = SOD, fill = tiempo), bins = 7) +
  facet_wrap(~playa)

# MDA, DTD, GST, SOD y CAT siguen mas o menos distribucion normal (revisar CAT y SOD)

# GPx y G6PDH no siguen ninguna distribucion clara, un caos

# TEAC, GR, CAT y SOD son mas o menos bimodales


m.prueba <- lm(log(MDA) ~ corte*tiempo, data = filter(datos, tejido == "tentacle", playa == "Almuñecar"))
summary(m.prueba)
plot(m.prueba)

m.prueba <- lmer(CAT ~ corte + (1|playa), data = filter(datos, tejido == "pedalDisk"))

