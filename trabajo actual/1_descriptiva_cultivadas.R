### Analisis exploratorio ORTIMAR 2 - Efecto de inducir la reproduccion asexual en ortiguillas cultivadas y seguimiento tras maduracion

# Alberto Coll Fernandez
# Comienzo: 14/12/2022
# Fin:

### SETUP ----

setwd("C:/Users/Usuario/Documents/TFG Alberto Coll")

library(dplyr)
library(ggplot2)
library(ggthemr)
library(multcompView)

# Llamamos el script de lectura
source(file = "./scripts/0_data_lab.R") # Laboratorio
#source(file = "./scripts/0_data_home.R") # En casa

# Filtramos para tener solo anemonas cultivadas
datos <- datos %>% filter(cultivo == "Si")

### Analisis exploratorio----

# La pregunta de investigacion es: ¿afecta el corte y la maduracion a los niveles de las distintas enzimas antiox. (y MDA/TEAC) en las anemonas cultivadas de la costa de granada?

# Voy a dejar hechos los tres modelos de graficas con la SOD

ggplot(datos, aes(x = corte, y = SOD)) +
  geom_boxplot(aes(fill= madurez), alpha = 0.5) +
  #geom_jitter(aes(color = madurez), size = 3,width =  0.1, alpha = 0.5) +
  facet_wrap(~tejido, scales = "free")

# posibles outliers segun boxplot
datos[42,7] # no hay evidencia
datos[7,7] #  no hay evidencia
datos[9,7] <- 63.999 # quitada una replica
datos[52,8] # no hay evidencia
datos[18,8] #menos seguro
datos[45,9]
datos[68,11] #revisar
datos[69,11] # revisar, son los dos de GST
datos[7,11]
datos[9,11] # revisar tb
datos[66,12]
datos[67,12]
datos[65,16]



# Queremos ver efecto del corte y del tiempo (maduracion), pero tenemos individuos de tres playas diferentes. Corte y tiempo son factores fijos, mientras que playa será un factor aleatorio. Primero voy a hacer histogramas y un modelo generico

ggplot(data = filter(datos, tejido == "pedalDisk")) +
  geom_density(aes(x = GPx))

mem.cat_p <- lmer(CAT ~ corte + madurez + (1|playa),
                  data = filter(datos, tejido == "pedalDisk"))
summary(mem.cat_p)
full.lme.cat_t <- lmer(CAT ~ corte * madurez + (1|playa),
                       data = filter(datos, tejido == "tentacle"))
summary(full.lme.cat_t)
