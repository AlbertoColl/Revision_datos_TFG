### Analisis estadistico TFG 1 (derivacion por modelo mixto)                Efecto del corte sobre anemonas, controlando por playa

# Version III (optimizada)

# Alberto Coll Fernandez                                                   Comienzo: 023/03/2023                                                       Fin :

# Problemas y aspectos a resolver:                                                  - Implementar test de Tukey en bucle, con ifelse para que solo lo             haga a los que resulten significativos, y al resto les rellene las           letras con ""

### SETUP ----
library(tidyverse)
library(ggthemr)
library(multcompView)
library(car)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFG Alberto Coll

# Directorio en portatil: D:/collf/Documents/TFG Alberto Coll

setwd("D:/collf/Documents/TFG Alberto Coll")

# Llamamos el script de lectura
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa
datos <- datos %>% filter(cultivo == "Si")

#### Prueba modelo mixto ----

### PRUEBAS DE anova

m_base <- aov(SOD ~ corte*madurez, data= filter(datos, tejido == "tentacle"))
car::Anova(m_base, type = 3)

m1 <- aov(SOD ~ corte + madurez, data= filter(datos, tejido == "tentacle"))
car::Anova(m_1, type = 3)



anova(m_base, m1)
plot(m_base)

## GRAFICO DE CHATGPT:

# Crear el gráfico de dispersión con los puntos de datos
ggplot(data = datos, aes(x = playa, y = SOD, color = factor(corte):factor(madurez))) +
  geom_point() +
  labs(x = "Playa", y = "SOD", color = "Corte:madurez") +
  
  # Añadir líneas de ajuste para cada combinación de corte y tiempo
  stat_smooth(aes(group = interaction(corte, madurez)), method = "lm", formula = y ~ x, se = FALSE) +
  
  # Añadir barras de error para cada playa
  stat_summary(aes(group = playa), fun.data = mean_se, geom = "errorbar", width = 0.2)


ggplot(data = filter(datos, tejido == "tentacle") %>% group_by(playa, corte:madurez) %>%  summarise(media = mean(SOD))) +
  geom_point(aes (x = playa, y = media, color = corte:madurez))


datos_s <- datos %>% mutate(cm = corte:madurez) %>%  group_by(playa, cm) %>% summarise(sod_m = mean(SOD, na.rm = T),
          sod_sd = sd(SOD, na.rm = T),
          cat_m = mean(CAT, na.rm = T),
          cat_sd = sd(CAT, na.rm = T))


ggplot(data = datos_s) +
  geom_point(aes (x = playa, y = sod_m, color = cm), size = 4) +
  geom_path(aes (x = playa, y = sod_m, color = cm, group = cm), linewidth = 1) +
  scale_color_manual(values = c("#507DBC", "#79A2D3", "#E24949", "#FF6B6B"))
  

ggplot(filter(datos, tejido == "tentacle")) +
  geom_boxplot(aes(x = playa, y = CAT, fill = playa) , color = "grey6", alpha = 0.4) +
  geom_jitter(aes(x = playa, y = CAT, color = playa), size = 3, 
              width = 0.2, alpha = 0.9) +
  scale_color_manual(values = c("#605E97", "#69B4AB", "#FBBC4C")) +
  scale_fill_manual(values = c("#605E97", "#69B4AB", "#FBBC4C")) +
  theme(panel.grid.major.x = element_blank())
